# Full-History Code Review — BoldForDelphi (Sep 2020 → Jul 2026)

Review of all 551 commits from "Initial commit" (21 Sep 2020) to HEAD (b3ef672, develop).
Method: ~22 parallel review agents over commit ranges and subsystem merge-diffs; every
finding below was **verified to still exist at current HEAD** unless marked PLAUSIBLE.
Defects that later commits already fixed are excluded (dozens were found and confirmed fixed).

Test suite state at review time: 2,098 tests, 2,096 pass, 2 ignored, 0 failed.
FastMM reports a shutdown leak traceable to production code (finding H12).

Legend: severity High/Medium/Low; file:line references are current HEAD.

---

## HIGH severity — confirmed at HEAD

Priority order within HIGH (by silence x reachability x blast radius; silent data
corruption in exercised paths first, loud failures later, unproven reachability last):
**H1 (fixed in BoldForDelphi b831a2f; port to Attracs-Bold pending) > H11 > H5 > H3 > H2 >
H10 > H12 > H9 > H8 > H4 > H6 > H7 > H13.**
H13 has the worst mechanism (heap corruption) but no confirmed data-owning sort caller —
verify reachability before fixing; if reachable it jumps to the top three.

### H1. Nested RollbackTransaction is silently ignored (data integrity)
- `Source/ObjectSpace/BORepresentation/BoldSystem.pas:3172-3173` — introduced by merge 92196d9 (Apr 2021)
- Pre-merge, rolling back a nested transaction set `fTransactionRollbackOnly := true`, so the
  outer `CommitTransaction` refused with sCommitNotAllowed. The merge dropped that assignment;
  the nested branch now only decrements nesting. `CommitTransaction` (3114-3116) still tests
  the flag, which is now mostly dead.
- Failure: inner `StartTransaction`/`RollbackTransaction` pair aborts without raising → its
  changes remain in the object space and are **silently committed** by the outer commit.
- Verified independently by two agents against both the pre-merge parent and HEAD.

### H2. TBoldFollower.Display: exception handling exactly inverted (UI data-aware controls)
- `Source/Common/Support/BoldControlPack.pas:1589` — merge 92196d9
- Merge inserted `not`: `if assigned(Controller) and not Controller.HandleDisplayException(...) then`
  with an empty then-branch. Display exceptions (OCL errors, renderer errors) are now silently
  swallowed when unhandled (the default for virtually all apps) and re-raised when a handler
  handled them. COM sibling `BoldControlPackCom.pas:1271` still has the original non-inverted
  logic, confirming the accident.
- Related: `DoDisplayException` (:539-540) guards on `fApplyException` but calls
  `fDisplayException` (wrong field; latent nil-pointer call), and has no callers — the
  published `OnDisplayException` controller event is never dispatched.

### H3. b3ef672 (UNPUSHED): nil guards can generate silently wrong SQL on the fetch path
- `Source/PMapper/SQL/BoldSqlNodesResolver.pas:104-109`, `BoldSqlQueryGenerator.pas:82-83`
- `VisitTBoldSqlIteration` silently `exit`s when Symbol/ObjectMapper is nil, leaving the subtree
  unresolved with no error. These visitors are shared with `PMFetchClassWithCondition`
  (`BoldPMappersDefault.pas:3290-3303`), which has **no** exception handler: result is either an
  AV at `GenerateSQL`, or SQL missing the iteration's constraint → **wrong result set**; in
  `CanEvaluateInPS` a false `True` is possible. Sibling `VisitTBoldSqlOperation` (:157) correctly
  raises EBold — the iteration guard should raise too. Root cause (nil Symbol for SQLite
  collect(role)) still unfixed.

### H4. 19019df (UNPUSHED range): DecRef raises EBold from inside a destructor
- `Source/PMapper/SQL/BoldSqlNodeMaker.pas:61-83` + `BoldSqlNodes.pas:661-666`
- The commit only changed tests to avoid `including(self)`/`excluding(self)`. At HEAD,
  `TBoldSqlNodeMaker.destroy` unconditionally DecRefs external bindings and `DecRef` raises
  when refcount is 0 — i.e. an exception thrown from a destructor invoked in
  `CanEvaluateInPS`'s `finally` (BoldPMappersDefault.pas:555). Masks the original failure,
  skips `aVariableIDLists.Free`, leaks remaining fields.

### H5. TBADateTime.SetAsDate wipes the time-of-day
- `Source/ObjectSpace/BORepresentation/BoldAttributes.pas:4040-4043` — commit 4d8f323 (Dec 2024)
- Old `TBAMoment.SetAsDate` did `SetAsDateTime(Int(Value) + AsTime)`; now raw `SetDataValue(Value)`.
  `attr.AsDate := <newdate>` on a datetime holding 09:00 now stores midnight. `SetAsInteger`
  routes through the same path.
- Related (same commit, medium): `IsSameValue` uses `Math.SameValue` default epsilon ≈ 4 ms at
  current date magnitudes — sub-4ms timestamp updates are treated as unchanged and never
  persisted (BoldAttributes.pas:4045-4048, guard at :3337). TBADate equality can become
  permanently false once a fractional value is stored via AsDateTime (:5102-5111).

### H6. TBABlob content-type validation breaks Assign for all non-image blobs
- `Source/ObjectSpace/BORepresentation/BoldAttributes.pas:2743-2746`, caller :2891 — commit bcadc56 (Jan 2026)
- Validation moved from the two image classes into base `TBABlob.SetStringRepresentation`,
  replacing a documented silent no-op with a raise. Plain `TBABlob.GetStringRepresentation(brShort)`
  returns '', so assigning any typed blob (e.g. 'application/pdf') to a plain TBABlob member
  raises EBold **mid-Assign** (after LoadFromStream already ran).

### H7. TBAMLString blob proxy wiring lost — ML string persistence raises
- `Source/ObjectSpace/BORepresentation/BoldMLAttributes.pas:714-724` (dead correct class at :110) — merge 92196d9
- `GetProxy` now instantiates `TBAString_Proxy`, whose ancestry implements no `IBoldBlobContent`,
  so requesting the blob-content proxy of a multi-language string always raises EBoldInternal.
  The correct `TBAMLString_Proxy` still exists in the file but is never instantiated.

### H8. Threaded DB validator: unsynchronized shared TList<String>
- `Source/PMapper/Validator/BoldDbValidator.pas:371-383` — commit 0b52cef (Dec 2024)
- Up to 6 worker threads call `fRemedyList.Add(s)` on the validator's single shared list with
  no lock anywhere in the unit. Concurrent remedies → list corruption / lost remedies / AV.
- Same rewrite (low): `Execute` lost its try/except + `BoldLog.EndLog` pairing.

### H9. TBoldDbCopy: infinite retry loop + wrong-connection release + off-by-one
- `Source/Persistence/DB/BoldDbCopy.pas` — commits 514e01c/2ec9326/601f44b/870d47f (Dec 2024)
- :255-266 — stale 'invalid byte sequence' handler from the row-by-row era doesn't reset
  ParamIndex/advance the source/rollback after the multi-row batch conversion → re-executes the
  identical failing statement forever (and on PostgreSQL the aborted transaction fails everything).
- :230-232 — `for x := Length(s)-1 downto 1` never examines index Length(s) (Delphi strings are
  1-based) → trailing control char survives, feeding the loop above.
- :163 vs :284 — query obtained from destination connection released to `SourceDatabaseInterface`.
- :65,133,152,181 — hard `TUniQuery`/`TUniConnection` casts in the generic persistence layer:
  EInvalidCast for FireDAC users; unit doesn't compile without UniDAC.

### H10. TThread.Resume replaced with Start — resuming a suspended thread raises EThread
- Commit 5dfe274 (2025). Sites: `Source/Common/ConnectionCOM/BoldComServer.pas:443,550`;
  `Source/Persistence/Propagation/BoldListenerHandle.pas:192`; `BoldIDAdder.pas:55,66,78`;
  `BoldListenerThread.pas:273`; `Source/Common/SupportWin/BoldThread.pas:119`
- These pair with `Suspended := True` self-suspension; `Start` cannot resume a suspended thread.
  OSS propagator sender thread: next SendEvent after queue drain → EThread. Fix: `Suspended := False`.

### H11. OSS class events: superclass change notifications silently dropped
- `Source/Persistence/Core/BoldAbstractSnooper.pas:367-382` — merge era
- `bsClassChanged` now emitted only for the leaf class, while the loop marks all superclass
  flags "already sent" without emitting. Clients watching superclass lists never refresh, and
  a later direct superclass change in the same batch is dropped by the early-exit (:371-372).
  Pre-merge emitted for the whole chain. Multi-client stale data.

### H12. CanEvaluateInPS failure path leaks the whole OLW node tree (live FastMM leak)
- `Source/ObjectSpace/Ocl/BoldOclLightWeightNodeMaker.pas:91-105` — `TBoldOLWNodeMaker.Destroy`
  never frees `fRootNode`; on `CanEvaluateInPS` success, ownership transfers to TBoldOclCondition,
  on failure (`aOLWNodeMaker.Failed`, BoldPMappersDefault.pas:529) the tree is orphaned.
  Matches the FastMM shutdown signature (TBoldOLWNodeList x51, Operation x25, Member x22 ...).
  Every production CanEvaluateInPS call with untranslatable OCL leaks a tree.
  Fix note: beware double-free on the success path — use an ownership flag.

### H13. Default merge sort corrupts data-owning arrays (defect certain, reachability unproven)
- `Source/Common/Core/BoldContainers.pas:227` (`InsertSort`, used by default smMergeSort per BoldDefs.pas:290)
- Shifts elements with raw `System.Move` (duplicating a pointer), then `Put(J, T^)` — `Put`
  (:667-674) calls `Dispose` when `bcoDataOwner` is set, freeing the just-duplicated live object.
  Any Sort of a data-owning array with one out-of-order pair → use-after-free now, double-free
  at destruction. QuickSort branch in the same method is safe (uses Exchange). No confirmed
  in-repo caller sorts a data-owning array — verify before/when fixing.
  Minor: `GetMem(T,...)` has no try/finally (leak if compare raises).

---

## MEDIUM severity — confirmed at HEAD

### Framework core / object space
- **M1. SendExtendedEvent lacks try/finally around StartNotify/EndNotify** —
  `BoldSubscription.pas:870-883` (commit 0bd8628 fixed SendQuery but not this sibling; SendEvent
  delegates here). One raising subscriber permanently breaks the post-notify queue.
- **M2. Cross-system guard dropped from TBoldObjectReference.CanSetLocator** — `BoldSystem.pas:6681-6687`
  (merge). Single-role assignment across two TBoldSystems is accepted → foreign-ID link corruption.
  List side kept the guard (:8516/8526/8540) — asymmetric.
- **M3. DirtyObjects by ClassTypeInfo uses exact-type match** — `BoldSystem.pas:2706` (d492eaf/ac26a7b).
  `DirtyObjectsAsBoldListByClassExpressionName['Base']` omits dirty subclass instances; nil arg +
  abstract root matches nothing. Sibling TBoldObjectClass overload (:2690) still uses `is`.
- **M4. Uninitialized `PreChangeCalled`** — `BoldLinks.pas:2560/2595` (522f35e propagated it).
  Read via var in `TBoldIndirectMultiLinkController.SetFromIDLists` before assignment — undo/
  old-value snapshots depend on stack garbage. Sibling direct-link method hardcodes `True`
  (:1012), making its 5 DoPreChangeIfNeeded calls dead — at least one of the two is wrong.
- **M5. DoPreUpdate runs outside DelayObjectDestruction** — `BoldSystemPersistenceHandler.pas:527-542`
  (9319d6c). A PreUpdate subscriber that deletes objects leaves nil-BoldObject locators in the
  cloned update list → AV mid-database-update. Also ReleaseUnneededRegions skipped on early-out/exception.
- **M6. TBoldDirtyObjectTracker.DiscardChanges incomplete loop guard** — `BoldSystem.pas:1623-1639`
  (64412dd). If Discard leaves the object dirty and listed, `fDirtyObjects.Last` returns the same
  object forever → UI hang.
- **M7. TBoldTransactionHandler: empty Receive → dangling fBoldSystem** — `BoldSystem.pas:10626-10631`
  (ac26a7b). Subscribes to beDestroying but never nils fBoldSystem; handler outliving the system
  reads freed memory in BeforeDestruction.
- **M8. TBoldPublisher.Destroy nils the wrong thing** — `BoldSubscription.pas:933` (ac26a7b).
  `fPublisherReference := nil` should be `fPublisherReference^ := nil`; the dangling-publisher
  safety net is a no-op.
- **M9. "becoming dirty" events fire on every modification** — `BoldSystem.pas:2402-2410`
  (be96e8b/ac26a7b). Moved outside the `not InDirtyList` guard; transition subscribers misbehave,
  bulk edits broadcast-storm. Stray mis-indented `end;` at :2410.
- **M10. Every object list subscribes beDestroying directly on the system** —
  `BoldObjectListControllers.pas:440` (fbedb37). Contradicts the routing design documented at
  :430-431; tens of thousands of subscriptions on one publisher (perf/scaling).
- **M11. ReceiveEventFromOwned: nil ClassList dereference path** — `BoldSystem.pas:2866-2878`
  (84b624c). Non-member/non-object originators AV; old code was event-whitelisted. Latent.
  Same commit: system publisher no longer broadcasts beObjectCreated/Deleted/Fetched/Unloaded
  (only class lists get them) — silent API behavior change for external subscribers.
- **M12. ReleaseEvaluator on shared TypeInfo** — `BoldSystem.pas:2249-2250` + `BoldSystemRT.pas:1981` +
  `BoldHandles.pas:525` (a1c13c6). Recreated meta evaluator loses InstallOclDefinitionLookUp;
  second system sharing TypeInfo gets its evaluator freed under it.
- **M13. TBoldObjectIdList.RemoveNonExistingIds removes while iterating forward** — `BoldId.pas:1263-1270`
  (merge). Out-of-bounds after first removal; adjacent matches skipped. Needs `downto`.
- **M14. OCL method-call arg shift duplicates Args[0]** — `BoldOclSemantics.pas:490-494` (merge).
  Ascending shift; destructor (BoldOclClasses.pas:474-481) then double-frees duplicated node.
- **M15. Typed OCL variable constructors AV without an active default system** —
  `BoldOclVariables.pas:763-818` (95a62f9). `TBoldOcl(nil).StringType` deref; CreateFloatVariable
  inconsistent-but-safe.
- **M16. TBoldOldValueHandler.GetIsEmpty scan loop is dead code** — `BoldSystemOldValuehandler.pas:59-81`
  (merge). Can never return True once IdCount > 0; "entries exist, values empty" case inverted.
- **M17. Pessimistic lock handler: per-iteration clone leak + nil deref** — `BoldLockHandler.pas:370-402`
  (merge). ExactId overwritten per loop (guard frees only the last); clone executes for
  bsGotLocks/bsLockLost where ClassName is empty → ClassTypeInfoByExpressionName nil → AV.
- **M18. SetEmptyValue family leaves null attributes null** — `BoldAttributes.pas:1294,1518,1574,1984,2260`
  (merge). Guards test the data field, not the null flag; non-nullable null attribute stays null
  (later EBoldAccessNullValue or NULL into NOT NULL column). TBAValueSet.SetEmptyValue (:4422-4428)
  additionally *reads* AsInteger first → raises on a null valueset.
- **M19. TBoldBlobStream.LoadFromStream no longer rewinds** — `BoldAttributes.pas:2614-2631` (merge).
  Sizes by full stream but reads from current position → EReadError for callers at position > 0
  (TBABlob.Assign compensates at its call site, proving the trap).
- **M20. RawIdStringForElement: uninitialized Result appended to** — `BoldManipulators.pas:192-210`
  (merge). Classic Delphi managed-Result bug; corrupt element ids possible in loops.

### Persistence / SQL
- **M21. PostgreSQL DatabaseExists always True** — template `BoldSQLDatabaseConfig.pas:999` returns
  `select exists(...)` (always one row); readers test row presence: `BoldFireDACInterfaces.pas:781`,
  `BoldUniDACInterfaces.pas:853`. CreateDatabase(DropExisting) then always tries DROP.
- **M22. MakeIDsExact dereferences nil MainTable for child-table-mapped classes** —
  `BoldPMappersDefault.pas:1020` (merge). Old dispatch (InternalMakeIDsExact, :993-1009) is now dead code.
- **M23. PMCreate error-path leak** — `BoldPMappersDefault.pas:1200-1351` (merge). Per-table
  MemberPMList/SQL freed inside loop without try/finally; leaks per table on ExecSQL failure.
- **M24. UniDAC CreateAnotherDatabaseConnection leaks TUniConnection** —
  `BoldUniDACInterfaces.pas:924-929` (06bb863). FireDAC twin got fOwnsConnection; UniDAC didn't.
- **M25. SetNextPersistenceController(nil) AVs** — `BoldPersistenceControllerPassthrough.pas:69-78`
  (3783251). `nil.AddSubscription` — impossible to detach a chained controller.
- **M26. FireDAC transient-error retry loops silently removed** — `BoldFireDACInterfaces.pas`
  Open/ExecSQL (bdae011, 2025). UniDAC still retries; adapters diverge; transient deadlocks now
  surface immediately.
- **M27. BoldCleanDatabaseForced now silently drops non-Bold tables** — `BoldPSDescriptionsSQL.pas:399-404`
  (46e1b83). Flag previously only skipped the Bold-tables confirmation; destructive semantic
  broadening for downstream users of the flag.
- **M28. StrToDateFmt mutates global FormatSettings** — `BoldUtils.pas:291-322` (c430394 added
  try/finally but kept the global mutation). Thread-unsafe in server use; use the TFormatSettings
  overload.
- **M29. OSS ClassNameFromObjectID returns superclass for inexact ids** — `BoldAbstractSnooper.pas:356-359`
  (guard + assert removed). Member-changed events with wrong class name → EOSS or dropped events.
- **M30. Locale-sensitive TryStrToFloat in SQL literal quoting** —
  `BoldExternalPersistenceControllerSQL.pas:478-482` (5dfe274). Comma-decimal locales (FI/SE)
  quote numbers as strings or emit `3,14` in SQL.
- **M31. sqlLike in-memory vs in-PS divergence for interior wildcards** —
  `BoldOclSymbolImplementations.pas:3112-3156`. `'a%b'` matched literally in memory, as wildcard in SQL.

### Configuration / build
- **M32. SpanFetch disabled repo-wide** — `Bold.inc:113` `{.$DEFINE SpanFetch}` (since Dec 2024
  merge 9d1bc4a; docs/CLAUDE.md still document it as on). Attracs-define builds silently lose
  batch fetching. Decide: re-enable or update docs.
- **M33. Performance stub boolean inverted** — `BoldPerformanceStub.pas:35-38` returns False
  ("Never log" requires True); every DeriveAndSubscribe in cursor/expression/filtered/sorted
  handles builds and discards diagnostic strings (5020ee3). Hot path overhead.
- **M34. cxGridBoldSupportUnit references non-existent AttracsSpanFetchManager** —
  `cxGridBoldSupportUnit.pas:1028` (8faeb71). SpanFetch-defined builds cannot compile; should be
  `BoldSpanFetchManager` (the stub).
- **M35. Transient instances of persistent classes now forbidden in default builds** —
  `BoldSystem.pas:4878` (4cdca48 hard-coded the Attracs-only branch; guarding test was gutted,
  aniv_1.pas:1433). Breaking change for community users of `Create(System, False)`.
- **M36. Units moved to Deprecated still referenced by active code** — `BoldExpressionHandleCom.pas:10`
  (+ BoldHandleComReg, BoldComHandlesConst) needs BoldVariableDefinitionCom; `BoldOCLExplorer.pas:10`
  and two examples use BoldVariableDefinition (removable uses). Compilation breaks without adding
  Deprecated to the search path (e48a724).
- **M37. FormSaver Apply/OK actions inherit Ctrl+Z** — `BoldFormSaverActions.pas:76,97,145` (c3e4582).
  Pressing "Undo" can commit changes; ambiguous dispatch with TBoldUndoAction.
- **M38. OSS FOnFailedMessage called without Assigned() guard** —
  `BoldExternalObjectSpaceEventHandler.pas:229-231` (81e4c5c). Nil method-pointer AV masks the
  original error; also silently swallows exceptions when assigned.
- **M39. BoldSystemCopy defect cluster** — `BoldSystemCopy.pas` (Dec 2024 series):
  :176 nil-check tests DestinationClasses instead of DestinationClass (missing-class → AV instead
  of diagnostic); :783 OnPreUpdate handler installed on destination system never removed (dangling
  method pointer after component free); :771-775 optimistic locking silently+permanently disabled
  on the shared destination model; :145-148 fProcessedObjects/fSkippedObjects never reset between
  runs (stale locators after source deactivation); :300/:397 batch window off-by-one and inverted
  negative EnsureRange (perf only).
- **M40. TBoldClassEventMapping.Assign loses ClassTypeName** — `BoldClassSubscriber.pas:139-148`
  (dfcf19f). Collection assign/copy-paste yields mappings subscribing to nothing/wrong class.
- **M41. TBoldVariableHandle.GetValue silently returns nil for bad ValueTypeName** —
  `BoldVariableHandle.pas:108-121` (2badf39). Typo in type name → nil instead of diagnostic raise.
- **M42. Production-live Assert in BoldHandles finalization** — `BoldHandles.pas:700` (4592450).
  Can fire on clean VCL shutdown (Forms finalization order); plus `Destroy` (:398) uses
  G_BoldSystemHandleList without nil guard after finalization → late-destroyed handle AVs.
- **M43. OLW XML read leaks variable-reference bindings** — `BoldOclLightWeightNodes.pas:1097-1113`.
  Binding materialized by a VariableReference (external var like self) is owned by nobody. One
  leak per distinct external variable per deserialization. (Residual after d09bb17's 4 fixes.)

## LOW severity / diagnostics (confirmed unless noted)

- L1. `IndexOf` on TBoldIntegerIndex returns the LAST occurrence (`BoldIndex.pas:813-818`,
  f223554 deliberate "reverse search") — Remove semantics changed for duplicate-bearing lists.
- L2. `TBoldHashIndex.IsCorrectlyIndexed` missing empty-bucket guard → EDivByZero
  (`BoldIndex.pas:504-512`; Find/FindAll/Remove all have the guard).
- L3. Traverser `CurrentIndex` permanently 0 (`BoldIndexableList.pas:644-647`).
- L4. `TBoldSubscribableObject.Destroy` missing `inherited` (`BoldSubscription.pas:1052-1055`) — benign today.
- L5. ISO datetime: `T`+minutes form rejected; dot-milliseconds rejected at datetime level;
  ms always discarded (`BoldIsoDateTime.pas:76-108`) — AsISODateTimeMS doesn't round-trip.
  (HH:MM without seconds works — 77b53cc fix verified intact.)
- L6. Malformed format strings raising EConvertError instead of the real error:
  `BoldComObjectSpaceAdapters.pas:1604,1649` ('%.L...'), `BoldComConst.pas:93`,
  `BoldMLAttributes.pas:673` (%d with no arg).
- L7. 'MethodNotImplemented' literals with ignored args: `BoldFireDACInterfaces.pas:546,552,565`;
  `BoldUniDACInterfaces.pas:615,620,1591` (resource string sMethodNotImplemented exists).
- L8. DB evolutor "Save scripts" saves SQLScript instead of the composed list
  (`BoldDbEvolutorForm.pas:110`) — mapping-info section silently missing.
- L9. BoldDbPlugins handle-selection loop indexes `List[0]` instead of `List[I]`
  (`BoldDbPlugins.pas:251`) — second persistence handle unreachable (design-time).
- L10. UTF-8 conversion corrupted a BoldGrid literal to U+FFFD (`BoldGrid.pas:2776`).
- L11. Orphaned uncompilable unit `BoldEnvironmentAllowBothUseVCL.pas` (uses deleted
  BoldEnvironmentCLX); not referenced by any package — delete it.
- L12. `TBoldUnLoader.Scan`: IncMilliSecond result discarded — ScanTime statistic always 0
  (`BoldUnloader.pas:144`).
- L13. NoNegativeDates guards disagree by factor 1000 (`BoldAttributes.pas:3377` vs `:3515`) —
  EBoldInternal for dates after ~2899 that MaySetValue approved (Attracs define only).
- L14. `TBATypedBlob.SetContentTypeContent` lost its Changed event — UI doesn't refresh on
  content-type-only change (`BoldAttributes.pas:3323-3328`).
- L15. Issue #11 fix (c6376c8) is a functional no-op — `BoldPMappersAttributeDefault.pas:337-342`
  assigns a value fAllowNull already holds; the original Oracle AllowNull+EmptyStringMarker
  symptom is likely still unfixed.
- L16. `AddPersistenceSubscription`'s Events parameter is dead but documented
  (`BoldPersistenceHandle.pas:124-129`).
- L17. Optimistic-lock retrieval instantiates unassigned members (dropped BoldMemberAssigned
  guard, `BoldOptimisticLockingSupport.pas:302-305`) — allocation overhead only.
- L18. Wrong label in cursor-handle trace ('Deriving TBoldExpressionHandle',
  `BoldCursorHandle.pas:139`).
- L19. Stale `BoldGeneratorTemplatesCPP` entry in `UnitTest/coverage_units.lst:220`.
- L20. bqMayRead permanently commented out rather than IFDEF'd (`BoldSystem.pas:5782-5786`) —
  only matters for non-BOLD_NO_QUERIES builds.
- L21. TBoldNonSystemHandle silently falls back to DefaultBoldSystemHandle (`BoldHandles.pas:312-321`) —
  masks wiring errors in multi-system apps.
- L22. TBoldClassListController.AddLocator is a silent no-op (`BoldObjectListControllers.pas:767`) —
  callers get no signal.
- L23. dclBold DCU/interface divergence risk from NoObjectSpaceTransactions moved to
  design-time packages only (c0de8e7).

## PLAUSIBLE — flagged but not fully verified

- P1. OSS HandleMessage `CommaText` parsing vs multi-member events containing commas/spaces
  (`BoldExternalObjectSpaceEventHandler.pas:198`) — sender is outside this repo.
- P2. `TBFSObjectIdRef.SetFromId` Adopt=True same-instance use-after-free
  (`BoldFreeStandingValues.pas:1235-1260`).
- P3. `TBABlob.GetAsBlob` bytes→Unicode→Ansi lossy roundtrip vs raw proxy comparison —
  spurious "modified" under CompareToOldValues (codepage-dependent).
- P4. Sub-second precision loss writing DateTime to string-typed columns
  (`BoldDBInterfaces.pas`, FormatDateTime without .zzz) — depends on backend field mapping.
- P5. `TBoldSystemLocatorList.AssertIntegrity` fails on unloaded locators; Assert is
  production-live; no in-repo callers.
- P6. `TBoldFollowerList.UnsafeGetSelected` nil lazy subfollower deref.
- P7. UniDAC perf-counter ifdef polarity flip (NO_PERFORMANCE_COUNTERS → BOLD_PERFORMANCE_COUNTERS).
- P8. `BoldUtils.UserTimeInTicks` final process handle never closed.
- P9. XML SQL-condition streaming `TBoldAnsiString` implicit conversion may corrupt binary
  TParam data (`BoldCondition.pas`).
- P10. FindSystemHandleForSystem(nil) returns an arbitrary inactive handle (`BoldHandles.pas:410-414`).

## Test-suite findings (UnitTest/)

- T1. Order-dependent OCL tests will flake like TestFirst did: TestLast (:1189), TestAt (:1199),
  TestIndexOf (:1206) in `Test.BoldOclEvaluation.pas` assume allInstances creation order.
- T2. ~24 TestCanEvaluateInPS* tests discard the Boolean result and end with Assert.Pass
  (`Test.BoldLinks.pas:1330-1810`) — a regression making all OCL untranslatable would still pass.
  Also ideal [TestCase] parameterization candidate (~200 lines → 1 method).
- T3. Midnight race in `Test.BoldThreadSafeLog.pas:173-179` (date computed after the log write).
- T4. Cracker/"Access" subclasses now in 14+ test files; several unnecessary (e.g. TBA*Access
  where public StringRepresentation[] exists).
- T5. Bare `except end` around DropDatabase cleanup in `Test.PersistenceFireDAC.pas:325,373` —
  log the swallowed error.

## Coverage statement

Reviewed: all 551 commits enumerated; every commit with Source/ logic changes 2022→2026 diff-reviewed;
the two mega-merges (92196d9 Apr 2021, 9d1bc4a Dec 2024) reviewed per-subsystem. Verified-fixed-later
regressions excluded throughout.

Known residual gaps (never line-reviewed; treat as unreviewed, not clean):
- 92196d9 per-file diffs: BoldOcl.pas (1168 lines), BoldOclClasses.pas (940), most of
  BoldOclSymbolImplementations.pas (2778), BoldUndoHandler.pas (1539), BoldDefaultXMLStreaming.pas
  (839), BoldElements.pas (1009), BoldSystemRT.pas (1927), most Handles/Core files
  (BoldRootedHandles, BoldSystemHandle, BoldOclRepository, ...), MoldModel/UMLModel editor,
  individual BoldAwareGUI controls, DB interfaces (BoldDBInterfaces/BoldSQLQuery),
  Common: BoldQueue, BoldSharedStrings, BoldXMLStreaming, BoldMemoryManager, BoldComServer/Adapter.
- (BoldLinks.pas and BoldObjectListControllers.pas 92196d9 diffs, and the 9d1bc4a validator/
  Meta/SQLDatabaseConfig/PSDescriptionsSQL diffs, were subsequently reviewed line-by-line —
  see addendum below.)

## Gap-fill addendum (completed)

### 92196d9 — BoldLinks.pas / BoldObjectListControllers.pas (line-by-line)

- **M44. Cross-system list guard is a no-op** — `BoldObjectListControllers.pas:173-182, 248-256, 332-340`.
  Under `{$IFNDEF AllowCrossSystemLists}`, a foreign-system locator triggers
  `SetBoldLastFailureReason(...)` (which only stores a global string, never raises) and then
  falls through to `LocatorList.Add(Locator)` anyway. The cross-system locator is silently added
  (deletion subscriptions live on the wrong system → dangling locator), and the stale failure
  reason later decorates an unrelated BoldRaiseLastFailure. Medium / high confidence.
  (Companion to M2 — the single-link side lost its guard entirely.)
- **M45. Fetch-triggered ReOrder dirties fetched objects** — `BoldLinks.pas:1095-1099` (ReOrder
  at :911-931). Refetching an ordered multilink whose DB order differs from memory now
  EnsureBoldObjects every locator (fetch cascade during fetch) and calls SetAndModifyOrderNo on
  every other end — *reading* a list marks the whole collection modified, causing unintended
  UpdateDatabase writes and optimistic-locking conflicts. The commit's own PATCH comment at
  :1636 acknowledges the hazard elsewhere. Medium-high / high confidence on mechanism.
- **M46. PreDiscard dropped the BoldDirty guard before invalidating the other end** —
  `BoldLinks.pas:1648-1678`. Pre-merge: `if not OtherEnd...BoldDirty then Invalidate`. Now
  unconditional; `Invalidate` on a bvpsModified member raises EBoldInternal (StateError) —
  discarding a modified single link whose other end is also dirty aborts mid-discard, leaving a
  half-reverted object space. Medium / high on mechanism.
- **M47. Class-var proxy cache shared across all systems/threads** — `BoldLinks.pas:285-286,
  315-316, 383-398, 503-517`. Single/multi link controller proxies are cached in *class*
  variables with an unsynchronized `RefCount = 1` reuse check. Two threads each running their
  own TBoldSystem can Retarget the same proxy concurrently → cross-system data corruption or AV.
  Pre-merge created a proxy per call. Medium (latent; multi-threaded multi-system services) /
  high on mechanism.
- Intentional-but-noteworthy (not defects): the merge deliberately relaxed relational integrity
  enforcement — linkto duplicate-add asserts and Unlink missing-locator asserts became silent
  exits/log-and-exit (BoldLinks.pas:836, 1137-1152, 1611-1613, 2329, ~2895), and link-object
  property access now calls MakeDbCurrent (:3051, :3125). These mask inconsistencies instead of
  failing fast; documented author decisions.
- Fixed later: the merge's ReceiveClassEvent blanket-Invalidate churn was fully rewritten at
  HEAD with fLoadedObjectCount bookkeeping.

### 9d1bc4a — validators / SQLDatabaseConfig / PSDescriptionsSQL / BoldMeta / OSS handler

**No findings survive at HEAD.** The merge introduced ~10 real defects in these files
(LogFmt type crash, SetBoldTV default regression, broken PG index templates, OSS delimiter wire
incompatibility, validator de-threading regressions, removed AnsiString column APIs) — every one
was reverted or fixed by later commits (0b52cef, d25722a, c3780ad, 0205569, 0e66b32, 2f2179a, ...).
