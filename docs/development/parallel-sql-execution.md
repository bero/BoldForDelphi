# Parallel SQL Execution for Bold

## Summary

Bold for Delphi currently executes **all SQL queries synchronously on the main VCL thread**. This means the GUI freezes during every database round-trip — whether fetching a single object or loading thousands of rows across multiple tables.

This document proposes **parallel SQL execution**: dispatching multiple SQL queries to worker threads (each with its own database connection), collecting the raw results, and feeding them back to the main thread for object population. This approach:

- **Does not require making TBoldSystem thread-safe** (the largest risk)
- **Overlaps database I/O** — the actual bottleneck — across multiple connections
- **Keeps the object space single-threaded** — no locks, no deadlocks, no risk to existing behavior
- **Uses the Flow framework** (`C:\Attracs\Attracs-Flow`) for structured concurrency, replacing manual thread management

Expected impact: **2-4x faster multi-table fetch operations** with a responsive GUI throughout.

---

## Current Architecture: Why It's Slow

### The Single-Threaded Fetch Path

Every Bold data fetch follows this synchronous call chain on the main thread:

```
UI action (navigate list, open form, expand grid)
  → TBoldSystem.EnsureObjects / FetchMember
    → TBoldSystemPersistenceHandler.PMFetch
      → TBoldObjectSQLMapper.FetchObjects
        → IBoldDatabase.OpenQuery          ← BLOCKS main thread (network I/O)
        → Read result rows                 ← BLOCKS main thread
        → Populate TBoldObject instances   ← CPU work on main thread
      → TBoldPublisher.SendEvent           ← Subscription notifications
    → UI components update via subscriptions
```

**Problem 1: Sequential SQL.** When Bold needs data from 5 tables (e.g., Trip + Parcel + Invoice + Driver + Vehicle), it executes 5 queries **one after another**. Each query involves a network round-trip to the database server (typically 5-50ms per query). Total wall-clock time = sum of all queries.

**Problem 2: Blocked GUI.** The main VCL message pump cannot process Windows messages while waiting for SQL results. The application appears frozen — no repainting, no mouse response, no progress indication.

**Problem 3: Single connection.** Bold uses one `IBoldDatabase` connection per `TBoldSystem`. Even if you wanted to run queries in parallel, a single database connection can only process one query at a time.

### What SpanFetch Does (and Doesn't Do)

The `{$DEFINE SpanFetch}` optimization (Attracs-specific) batches multiple object fetches into fewer SQL statements. This reduces the **number** of round-trips but does not parallelize them. Each batch still blocks the main thread.

---

## Proposed Solution: Parallel SQL with Flow Framework

### Core Concept

```
Main Thread                     Worker Pool (Flow TTask)
───────────                     ────────────────────────
                                Thread 1: Own DB connection
Prepare query list              Thread 2: Own DB connection
Dispatch to workers ──────────→ Thread 3: Own DB connection
                                Thread 4: Own DB connection
                                │
                                ├─ Thread 1: SELECT * FROM Trip WHERE ...
                                ├─ Thread 2: SELECT * FROM Parcel WHERE ...
                                ├─ Thread 3: SELECT * FROM Invoice WHERE ...
                                └─ Thread 4: SELECT * FROM Driver WHERE ...
                                │
Collect raw results ←───────────┘  (all queries run simultaneously)
Populate TBoldSystem objects
Fire subscription notifications
UI updates
```

**Key principle:** Only the SQL execution and result fetching move to worker threads. Object population and subscription notifications remain on the main thread where they are safe.

### Why Flow Framework?

The [Flow framework](file:///C:/Attracs/Attracs-Flow/) (`C:\Attracs\Attracs-Flow`) is a Delphi 12.3+ concurrent programming framework purpose-built for this kind of work:

| Need | Flow Provides |
|------|---------------|
| Parallel task execution | `TTask.Run`, `TTask.Scope`, `TTask.WaitForAll` |
| Structured concurrency (no leaked threads) | `ITaskScope` — child tasks cannot outlive scope |
| Main-thread result delivery | `TMainThreadDispatch.Post` (lock-free, VCL-integrated) |
| Per-thread storage (DB connections) | `TThreadLocal<T>` |
| Timeout and cancellation | `IContext` with deadlines, `ICancellationToken` |
| Partial failure handling | `TTask.AwaitAllSettled<T>` with `TTaskResult<T>` |
| Thread pool management | OS-backed thread pool via `TPlatformPools` |

Flow replaces `System.Threading` (Delphi's PPL) with a lock-free, OS-integrated alternative — no `TCriticalSection`, no `TMonitor`, no manual thread lifetime management.

---

## Technical Design

### 1. Per-Thread Database Connections

Each worker thread needs its own independent database connection. Bold already supports this via `IBoldDatabase.CreateAnotherDatabaseConnection`:

```pascal
// IBoldDatabase interface (already exists in Bold)
function CreateAnotherDatabaseConnection: IBoldDatabase;
```

This method creates a new connection with the same configuration (server, database, credentials) but as an independent session. It is used by `BoldDbValidator` (production-deployed, multi-threaded schema validation) and `BoldDbCopy` (4 parallel threads, not production-deployed).

**Implementation using Flow's TThreadLocal:**

```pascal
type
  TBoldParallelQueryPool = class
  private
    FMainConnection: IBoldDatabase;
    FThreadConnections: TThreadLocal<IBoldDatabase>;
  public
    constructor Create(AMainConnection: IBoldDatabase);
    function GetConnectionForCurrentThread: IBoldDatabase;
  end;

constructor TBoldParallelQueryPool.Create(AMainConnection: IBoldDatabase);
begin
  inherited Create;
  FMainConnection := AMainConnection;
  FThreadConnections := TThreadLocal<IBoldDatabase>.Create(
    function: IBoldDatabase
    begin
      // Each thread gets its own connection on first use
      Result := FMainConnection.CreateAnotherDatabaseConnection;
    end);
end;

function TBoldParallelQueryPool.GetConnectionForCurrentThread: IBoldDatabase;
begin
  Result := FThreadConnections.Value;
end;
```

### 2. Parallel Query Dispatch with TTask.Scope

```pascal
procedure TBoldParallelFetcher.FetchObjectsParallel(
  const Queries: TArray<TBoldSQLQuery>);
var
  RawResults: TArray<TBoldFetchResult>;
begin
  SetLength(RawResults, Length(Queries));

  // Structured scope: all tasks complete before scope exits
  TTask.Scope(procedure(const Scope: ITaskScope)
  var
    Futures: TArray<IFuture<TBoldFetchResult>>;
    I: Integer;
  begin
    SetLength(Futures, Length(Queries));

    // Launch all queries in parallel
    for I := 0 to High(Queries) do
    begin
      var QueryIndex := I; // capture for closure
      Futures[I] := TTask.Spawn<TBoldFetchResult>(Scope,
        function: TBoldFetchResult
        var
          Conn: IBoldDatabase;
        begin
          Conn := FPool.GetConnectionForCurrentThread;
          Result := ExecuteQueryOnConnection(Conn, Queries[QueryIndex]);
        end);
    end;

    // Collect all results (blocks until all complete)
    RawResults := TTask.AwaitAll<TBoldFetchResult>(Futures);
  end);

  // Back on calling thread — populate objects (must be main thread)
  for var R in RawResults do
    PopulateObjectsFromResult(R);
end;
```

### 3. Raw Result Container

Results must be transported from worker threads to the main thread without touching the object space. Define a simple value container:

```pascal
type
  TBoldFetchResult = record
    ClassIndex: Integer;           // Which Bold class this data belongs to
    SQLQuery: string;              // The original query (for debugging)
    Rows: TArray<TArray<Variant>>; // Raw row data [row][column]
    ColumnNames: TArray<string>;   // Column metadata
    RowCount: Integer;
    Success: Boolean;
    ErrorMessage: string;          // If query failed
  end;
```

This record contains only primitive types and arrays — it is safe to create on a worker thread and read on the main thread.

### 4. Integration Point in Bold

The natural integration point is `TBoldObjectSQLMapper.FetchObjects` in `Source/PMapper/SQL/BoldPMappersSQL.pas`. This is where SQL queries are built and executed during a fetch operation.

**Current flow (simplified):**
```
TBoldObjectSQLMapper.FetchObjects
  → BuildSQL for class
  → OpenQuery (synchronous)
  → ReadRows into objects
  → Next class...
```

**Proposed flow:**
```
TBoldObjectSQLMapper.FetchObjects
  → BuildSQL for ALL classes in this fetch batch
  → IF query count > 1:
      Dispatch to TBoldParallelFetcher.FetchObjectsParallel
    ELSE:
      Execute single query synchronously (no overhead)
  → ReadRows into objects (always main thread)
```

The threshold (parallel vs. sequential) avoids overhead for single-query fetches, which are the common case for individual attribute access.

### 5. Async Fetch with GUI Responsiveness

For large loads where you want the GUI to remain responsive during the entire operation:

```pascal
procedure TBoldAsyncFetcher.FetchAsync(
  const Queries: TArray<TBoldSQLQuery>;
  OnComplete: TProc<TArray<TBoldFetchResult>>;
  OnError: TProc<Exception>);
begin
  TTask.Run(procedure
  var
    Results: TArray<TBoldFetchResult>;
  begin
    try
      // Run all queries in parallel on worker threads
      Results := ExecuteQueriesParallel(Queries);

      // Marshal results to main thread for object population
      TMainThreadDispatch.Post(procedure
      begin
        OnComplete(Results);
      end);
    except
      on E: Exception do
        TMainThreadDispatch.Post(procedure
        begin
          OnError(E);
        end);
    end;
  end);
end;
```

This pattern keeps the main thread completely free while queries execute. The VCL message pump runs normally — the UI stays responsive, progress indicators animate, and the user can interact with already-loaded data.

### 6. Reactive Stream Pattern (Advanced)

For scenarios where you want to process results **as they arrive** rather than waiting for all queries to complete:

```pascal
Observable<TBoldSQLQuery>.FromArray(Queries)
  .SubscribeOn(TSchedulers.ThreadPool)
  .Map<TBoldFetchResult>(
    function(Q: TBoldSQLQuery): TBoldFetchResult
    begin
      var Conn := FPool.GetConnectionForCurrentThread;
      Result := ExecuteQueryOnConnection(Conn, Q);
    end)
  .ObserveOn(TSchedulers.MainThread)
  .Subscribe(
    procedure(R: TBoldFetchResult)
    begin
      // Main thread: populate objects incrementally
      PopulateObjectsFromResult(R);
      // Grid/UI updates as each table loads
    end,
    procedure(E: Exception)
    begin
      HandleQueryError(E);
    end,
    procedure
    begin
      // All queries complete
      NotifyFetchComplete;
    end);
```

This approach gives the best user experience — tables appear progressively as their queries complete.

---

## Advantages

### Performance

| Benefit | Explanation |
|---------|-------------|
| **Overlapping I/O** | N queries across N connections run simultaneously. Wall-clock time ≈ slowest single query instead of sum of all queries. |
| **Near-linear speedup** | For multi-table fetches, 4 connections ≈ 4x faster (limited by DB server capacity and network bandwidth). |
| **Complementary to SpanFetch** | SpanFetch reduces the number of queries; parallel execution overlaps what remains. Both optimizations stack. |
| **No lock overhead on hot path** | The object space stays single-threaded — no lock acquisition cost on every attribute access or subscription notification. |

### User Experience

| Benefit | Explanation |
|---------|-------------|
| **Responsive GUI** | The async pattern keeps the VCL message pump running. No frozen windows. |
| **Progressive loading** | With the reactive pattern, data appears table-by-table as queries complete. |
| **Cancellation support** | Flow's `IContext` with deadlines enables timeout on long-running queries without killing the connection. |
| **Progress indication** | The main thread is free to update progress bars or status messages. |

### Architecture

| Benefit | Explanation |
|---------|-------------|
| **Minimal invasion** | Changes are confined to the PMapper/persistence layer. The object space, subscription system, and GUI layer are untouched. |
| **Backward compatible** | Single-query fetches bypass the parallel path entirely. Existing behavior is preserved. |
| **Production-proven pattern** | `CreateAnotherDatabaseConnection` with multi-threaded workers is production-proven by `BoldDbValidator`. The interface, thread-safe queues, and per-thread connection pattern are battle-tested. |
| **No global state changes** | No need to modify the 15+ unguarded global singletons in the Bold core. |

---

## Challenges

### Database Connection Management

| Challenge | Mitigation |
|-----------|------------|
| **Connection pool sizing** | Too many connections waste server resources; too few reduce parallelism. Start with 4 workers, make configurable. |
| **Connection lifecycle** | Connections must be created lazily (first use per thread) and cleaned up when the pool is destroyed. Flow's `TThreadLocal<T>` handles per-thread lifecycle. |
| **Connection limits** | SQL Server default max is 32,767; PostgreSQL default is 100. Monitor `max_connections` on the server. |
| **Transaction isolation** | Worker connections are independent sessions. They see committed data only (READ COMMITTED). In-flight main-thread transactions are not visible to workers. |

### Result Marshaling

| Challenge | Mitigation |
|-----------|------------|
| **Memory for raw results** | Large result sets (100K+ rows) must be held in memory as `TArray<TArray<Variant>>` until processed. Consider streaming or chunked processing for very large fetches. |
| **Variant overhead** | Storing values as `Variant` adds boxing/unboxing cost. For maximum performance, use typed arrays matching the column types. |
| **Column type mapping** | The raw result must carry enough metadata (column names, types) for the main-thread population code to map values correctly to Bold attributes. |

### Object Population Ordering

| Challenge | Mitigation |
|-----------|------------|
| **Cross-table dependencies** | If object A references object B (foreign key), and both are fetched in parallel, B must be populated before A's reference is resolved. Solution: populate base classes first, then dependent classes, same as Bold already does in sequential mode. |
| **Subscription storm** | Populating many objects triggers subscription notifications. This is the same as current behavior but happens in a burst after parallel fetch completes. Consider `TBoldSystem.DelaySubscriptions` during bulk population. |

### Error Handling

| Challenge | Mitigation |
|-----------|------------|
| **Partial failure** | If 3 of 5 queries succeed and 2 fail, what happens? Use `TTask.AwaitAllSettled` to collect individual success/failure results. Populate successful results; report failures. |
| **Connection failure** | A worker connection may drop mid-query. Implement retry at the connection level (reconnect and re-execute). Flow's `TStrategy.CircuitBreaker` can automate this. |
| **Deadlocks on DB server** | Parallel read queries rarely deadlock, but mixed read/write can. Limit parallel execution to read-only fetch operations. |

### Integration Complexity

| Challenge | Mitigation |
|-----------|------------|
| **Bold's internal fetch API** | The current fetch path is deeply intertwined — SQL generation, execution, and object population happen in the same method. Refactoring requires separating "build + execute SQL" from "populate objects." |
| **Testing** | Unit testing parallel code is inherently harder. Use Flow's `TSchedulers.Test` (deterministic test scheduler) for repeatable tests. |
| **SpanFetch interaction** | SpanFetch batches queries before execution. The parallel fetcher should receive the post-SpanFetch query list, not interfere with batching logic. |
| **Debugging** | Multi-threaded bugs are hard to reproduce. Flow's structured concurrency (tasks cannot outlive scope) eliminates leaked threads. Use `BoldThreadSafeLog` for thread-safe diagnostics. |

---

## Implementation Phases

### Phase 1: Infrastructure (Low Risk)

- Create `TBoldParallelQueryPool` with `TThreadLocal<IBoldDatabase>` connection management
- Create `TBoldFetchResult` record type for raw result transport
- Add unit tests for connection pool (create, use, cleanup)
- **No changes to existing Bold code**

### Phase 2: Parallel Fetch API (Medium Risk)

- Create `TBoldParallelFetcher` class with `FetchObjectsParallel` method
- Refactor `TBoldObjectSQLMapper.FetchObjects` to separate SQL execution from object population
- Add threshold: parallel path only when query count > 1
- Integration tests: fetch same data via sequential and parallel paths, verify identical results

### Phase 3: Async GUI Integration (Medium Risk)

- Add `TBoldAsyncFetcher` with `FetchAsync` callback pattern
- Integrate `TMainThreadDispatch.Post` for result delivery
- Add cancellation support via `IContext` deadlines
- Test with real forms: verify GUI stays responsive during large loads

### Phase 4: Reactive Streaming (Optional, Higher Complexity)

- Implement progressive loading via `Observable<T>` pattern
- Results appear in the UI as each query completes
- Add progress notification (X of Y tables loaded)

---

## Configuration

```pascal
type
  TBoldParallelFetchConfig = record
    Enabled: Boolean;              // Master switch (default: True)
    MaxConnections: Integer;       // Worker thread count (default: 4)
    MinQueriesForParallel: Integer; // Threshold to activate (default: 2)
    QueryTimeoutMs: Integer;       // Per-query timeout (default: 30000)
    RetryOnFailure: Boolean;       // Retry failed queries once (default: True)
  end;
```

---

## Performance Estimates

Assumptions: 4 worker connections, average query latency 20ms, 10 tables to fetch.

| Scenario | Sequential | Parallel (4 conn) | Speedup |
|----------|-----------|-------------------|---------|
| 10 simple queries (20ms each) | 200ms | ~60ms (3 batches of 3-4) | **3.3x** |
| 5 medium queries (50ms each) | 250ms | ~100ms (2 batches) | **2.5x** |
| 20 queries mixed (10-100ms) | ~1,100ms | ~300ms | **3.7x** |
| 1 single heavy query (500ms) | 500ms | 500ms (no parallel) | 1.0x |

The speedup is most significant when there are **many independent queries** of similar duration — which is exactly what happens during form-load or navigation in Bold applications.

---

## References

- **Flow Framework**: `C:\Attracs\Attracs-Flow\` — Structured concurrency, TTask, TMainThreadDispatch
- **BoldDbCopy**: `Source/Persistence/DB/BoldDbCopy.pas` — Existing 4-thread parallel pattern with `CreateAnotherDatabaseConnection` (not production-deployed)
- **BoldDbValidator**: `Source/PMapper/Validator/BoldDbValidator.pas` — Production-proven multi-threaded worker pattern with `CreateAnotherDatabaseConnection`
- **BoldThreadSafeQueue**: `Source/Common/Core/BoldThreadSafeQueue.pas` — Thread-safe queue (alternative to Flow channels)
- **IBoldDatabase**: `Source/Persistence/DB/BoldDBInterfaces.pas` — `CreateAnotherDatabaseConnection` interface
- **TBoldObjectSQLMapper**: `Source/PMapper/SQL/BoldPMappersSQL.pas` — Integration point for parallel fetch
