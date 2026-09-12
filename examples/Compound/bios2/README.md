# Account Server

The smallest COM demo in the repository, and the one that asks a specific
question: when the object space is on the other side of a process boundary,
should the client navigate it, or should it call a service that navigates it
server-side?

The client has two buttons that give the same answer to the same question, one
each way. Pressing both and noticing the difference is the whole demo.

## The projects

| Project | What it builds |
| --- | --- |
| `AccountServer.dpr` | the COM server: the model, the database, a console window, and a plain COM service object |
| `AccountServerClient.dpr` | a four-control client |

Neither has a `.dproj`; open the `.dpr` files.

## Running it

1. Build and run **AccountServer.exe**. `server\ServerCode.pas` registers it as
   `BoldServer`, class id `{5BC282A9-970A-4DE1-8644-2EB4465000FC}`.
2. On the console: **Create DB**, then **Open system**. The database is
   Interbase, `bios2.gdb`, behind a `TBoldDatabaseAdapterIB`, which lives under
   `Source\Deprecated\Persistance\IBX` and is not in the maintained package set.
3. Use the navigator to create accounts. Each new account gets a random number
   and a random credit from `CompleteCreate`. Create a few hundred if you want
   the two buttons on the client to feel different. **Update DB**.
4. Run **AccountServerClient.exe**, type one of the account numbers from the
   server's grid into the edit box, and press each button in turn.

## The model

One class.

| Class | Members |
| --- | --- |
| `Account` | `Number: String`, `Credit: Integer`, `Total: Integer` |

`TAccount.CompleteCreate` builds a number by appending random three-digit groups
until it is at least six characters long, then sets `Credit` to a random value
between 100 and 90100 and copies it into `Total`.

`AccountClasses.inc` also holds `ReceiveQueryFromOwned` and `MayDelete`
overrides that do nothing but call `inherited`. They are generated stubs that
were never filled in.

## The two ways to answer the question

### Navigate the object space remotely

`client\FMain.pas`, **Fetch accounts**. The client has a `TBoldListHandleCom` on

```
Account.allInstances
```

rooted on `DM.BoldSystemHandleCom1`, and it walks that list itself:

```pascal
for i := 0 to blhcAccounts.Count - 1 do
begin
  Element := blhcAccounts.List.Elements[i] as IAccount;
  if (Element).Number = WideString(trim(edAccountNumber.Text)) then ...
end;
```

Every `Elements[i]` and every `.Number` is a COM call. The handler sets the
cursor to an hourglass before it starts, which tells you what the author
expected.

### Call a service object

**Validate**. The server also publishes an ordinary COM object that is not part
of the object space at all:

```pascal
obj := DM.ClientObjectHandle.ComObject;
olecheck(obj.QueryInterface(IID_IAccountValidator, ValidatorService));
ValidatorService.Validate(WideString(Trim(edAccountNumber.Text)), Valid);
```

One call out, one boolean back. The loop is identical, but it runs inside the
server, against a local `TBoldListHandle`:

```pascal
function TAccountValidator.Validate(const Number: WideString; out Value: WordBool): HResult;
begin
  if not dataModule2.BoldSystemHandle1.Active then
    dataModule2.BoldSystemHandle1.Active := true;
  for i := 0 to DataModule2.blhAccounts.Count - 1 do
    if ((DataModule2.blhAccounts.List[i] as TAccount).Number = Number) then
    begin
      Value := true;
      Break;
    end;
end;
```

Note that it opens the system on demand, so the validator works even if nobody
has pressed **Open system** on the console.

## How the server publishes both

`server\dmProduct.dfm` puts two things on one `TBoldComServerHandle`:

| Handle | `ObjectName` | What it publishes |
| --- | --- | --- |
| `BoldComServerElementHandle1` | `System` | the system handle, `ExportMode = emHandle` |
| `ServiceObject` | `AccountValidator` | an arbitrary COM object, through `OnGetComObject` |

`ServiceObject` is a `TBoldComServerObjectHandle`, which is the general escape
hatch: its event just returns an interface, so anything you can make a COM
object out of can be published beside the object space.

`TAccountValidator` is a plain `TCOMObject` with its own class id
`{B4E9AF33-541F-4F32-AFBE-D617289E4126}`, registered by a factory in the unit's
initialization section with `ciInternal, tmSingle`.

The client reaches each one through its own handle on the same connection:

```
BoldSystemHandleCom1.ObjectName = 'System'
ClientObjectHandle.ObjectName   = 'AccountValidator'
```

Both hang off `BoldComConnectionHandle1`, which has `AutoConnect` set, so
neither button needs to connect first.

## The COM layer

Same generated stack as the sibling `Bios` demo, one class smaller.

| File | What it is |
| --- | --- |
| `server\AccountClasses.idl` | the generated `IAccount` definition |
| `Core\AccountServer.tlb` | the compiled type library |
| `Core\AccountClasses_TLB.pas` | the Delphi import, used by **both** projects |
| `server\AccountClasses_Adapters.pas` | the `TBoldObject`-to-`IAccount` adapter |

`Core\` exists precisely because the TLB unit is the only thing the two projects
share. The client links no Bold classes, no model, and no persistence at all.

## The server console

`server\FServerConsoleUnit` is a grid on the same `Account.allInstances` handle
with `total`, `number` and `credit` columns, a navigator, a `TBoldLabel` on

```
Account.allInstances->size
```

and the three standard actions: **Create DB**
(`TBoldIBDatabaseAction`), **Open system**, **Update DB**.

## Files

| Path | What it holds |
| --- | --- |
| `AccountServer.dpr` | the server |
| `AccountServerClient.dpr` | the client |
| `server\dmProduct.pas` / `.dfm` | the model, the system and persistence handles, the IB database, the COM server handle, and `TAccountValidator` |
| `server\FServerConsoleUnit.pas` / `.dfm` | the server window |
| `server\ServerCode.pas` | registers the COM server factory |
| `server\AccountClasses*.pas`, `.inc`, `.idl` | generated classes and adapter, plus `CompleteCreate` |
| `server\Account.bld` | exported model; the authoritative copy is inside `dmProduct.dfm` |
| `Core\AccountClasses_TLB.pas`, `AccountServer.tlb` | the type library, shared by both projects |
| `client\dmSystem.pas` / `.dfm` | the connection handle, the COM system handle, the validator object handle |
| `client\FMain.pas` / `.dfm` | the two buttons |
