# Transactions

Bold transactions are about the object space, not about the database.
`StartTransaction`, `CommitTransaction` and `RollbackTransaction` bracket a set
of in-memory changes; a rollback puts every object back to the values it had,
without a database round trip. Validation that is expensive or that only makes
sense on the finished result is asked at commit time, through the `bqMayCommit`
query.

This demo is a bank. You queue up a batch of requests, press one button, and
either all of them happen or none of them do.

## Running it

There is no `.dproj`. Open `Transaction.dpr` in the IDE and let Delphi create
one.

It needs a database: InterBase through `TBoldDatabaseAdapterIB` on `dmMain`,
`sysdba` / `masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it
first.

`TfrmStart` runs modally before the main form: **Create** schema, **Open**
system (`ModalResult` 1) or **Cancel**, and the main form is only created if
the dialog returns `idOK`.

Then: create a couple of accounts with the navigator, give them a total and a
credit limit, queue a **Transfer**, a **Modify credit** and a **Close
account**, and press **Run!**.

## The batch

```pascal
dmMain.bshMain.System.StartTransaction;
try
  while blhAllRequests.List.Count > 0 do
  begin
    (blhAllRequests.list[0] as TRequest).Perform;
    (blhAllRequests.list[0] as TRequest).Delete;
  end;
  dmMain.bshMain.System.CommitTransaction;
except
  dmMain.bshMain.System.RollbackTransaction;
  raise;
end;
```

The loop consumes the list: each request performs itself and is then deleted,
which is why the condition is `Count > 0` rather than an index walk. If
anything raises part-way, the rollback undoes the deletions **and** the balance
changes together, so a half-applied batch cannot survive.

`TRequest` is abstract with three concrete subclasses, each overriding
`Perform`:

| Class | Attributes and roles | `Perform` |
| --- | --- | --- |
| `TTransfer` | `Amount`, `source`, `target` | moves the amount between the two accounts |
| `TModifyCredit` | `NewCredit`, `account` | sets the account's credit limit |
| `TClose` | `account` | deletes the account |

Each starts by checking its links and calling `BoldRaiseLastFailure` if one is
missing, so an incomplete request fails the batch rather than doing half a job.

## What makes a batch fail

Two rules on `TAccount`, and they fire at different moments.

```pascal
if (OriginalEvent = bqMayCommit) and (Originator = M_Total) and (Total < -Credit) then
begin
  result := false;
  SetBoldLastFailureReason(TBoldFailureReason.Create(
    Format('Account total is under credit limit for account %s', [number]), self));
end;
```

`ReceiveQueryFromOwned` answers `bqMayCommit`, which Bold asks when the
transaction commits, not when `Total` is assigned. An account may therefore go
below its credit limit in the middle of a batch and recover before the end.
That is exactly why the demo is a queue of requests with one Run button rather
than a button per operation.

`TAccount.MayDelete` returns `Total = 0`, so a Close request against a non-zero
account fails and takes the whole batch with it.

Both use `TBoldFailureReason`, so the message names the account.

## Two UI details worth stealing

`BoldPageControl1` picks its own tab from the class of the selected request:

```
BoldProperties.Expression = 'ts' + self.oclType.asString
```

The tab sheets are named `tsTransfer`, `tsClose` and `tsModifyCredit`, so
selecting a request in the list brings up the editor for its type with no code.

Each tab's editors hang off a `TBoldExpressionHandle` that filters by type:

```
behTransfer     : self->filterOnType(Transfer)->first
behClose        : self->filterOnType(Close)->first
behModifyCredit : self->filterOnType(ModifyCredit)->first
```

all rooted on `blhAllRequests`, so a tab binds to the current request only when
it is of that tab's type, and is empty otherwise.

The account combo boxes use `BoldSetValueExpression` (`source`, `target`,
`account`) with `BoldSelectChangeAction = bdcsSetValue`, which is how a combo
over a list handle writes a link rather than a string.

**Debug, SystemDebugger** opens `TBoldSystemDebuggerFrm` on the live system,
which is the quickest way to see what the transaction actually did.

## Files

| File | What it holds |
| --- | --- |
| `Transaction.dpr` | data module, then the start dialog, then the main form |
| `fStart.pas` / `.dfm` | create schema / open system / cancel |
| `dMain.pas` / `.dfm` | system handle, model, persistence, the Bold actions |
| `fMain.pas` / `.dfm` | the batch loop, the page control, the type-filtered handles |
| `BankingClasses.inc` | `Perform` for each request, `bqMayCommit`, `MayDelete` |
| `BankingClasses*.pas/.inc` | generated `Account`, `Request` and subclasses |
