# Buildings and Owners over COM

The `Building` demo split across a process boundary. Same model, same form
layout, same renderers, but the object space lives in a server process and the
client reaches it over COM.

The point is how little changes. The client's controls are the `Com` twins of
the ordinary ones, `TBoldGridCom` for `TBoldGrid` and so on, they take the same
OCL expressions in the same properties, and they are rooted on a
`TBoldSystemHandleCom` instead of a `TBoldSystemHandle`. What does change is
worth knowing, and this demo shows that too, because the same form is written
twice, once each way.

## The projects

| Project | What it builds | Uses |
| --- | --- | --- |
| `Server.dpr` | the COM server, with a small console form | `Core\`, `Server\` |
| `Client.dpr` | the client UI | `Core\BuildingsAndOwners_TLB`, `Client\`, `ClientGui\` |
| `ServerAndClient.dpr` | both in one process | everything except `Gui\` |

None has a `.dproj`; open the `.dpr` files. `ServerAndClient` exists so that you
can debug both halves under one debugger; it still goes through the COM layer,
it just does not cross a process boundary.

## Running it

1. Build and run **Server.exe**. `Server\ServerCode.pas` registers it through
   `TBoldComServerConnectionFactory` as `Server.BoldServer`, class id
   `{D7724C3F-9082-460F-9321-CB47C232CA29}`.
2. On the server console: **Create DB**, then **Open system**. The database is
   Interbase, `Bios.gdb`, behind a `TBoldDatabaseAdapterIB`. That adapter and
   `BoldBDEInterfaces`, which `DMCore` also pulls in, both live under
   `Source\Deprecated` and are not in the maintained package set.
3. Leave the server running and start **Client.exe**. It connects on demand;
   there is a button that sets `BoldComConnectionHandle1.Connected := true`, and
   closing the form disconnects.

The server console is deliberately plain: a list box of objects, an auto form
provider, and the three database actions. It is there to prove the server has a
real object space, not to be used.

## The model

`Core\` holds the model and everything generated from it. It is the same model
as the `Building` demo minus two members: there is no `Capacity` on
`Residential_Building` and no derived `name` on `Person`.

| Class | Members |
| --- | --- |
| `Building` | `Address: String`, `ZipCode: Integer` |
| `Residential_Building` (: `Building`) | `TotalRent: Currency` |
| `Person` | `FirstName`, `LastName`, `Assets: Currency`, `IsMarried: Boolean` |
| `Ownership` | link class |

| Association | Shape |
| --- | --- |
| `Person.ownedBuildings` / `Building.owners` | many to many through `Ownership` |
| `Person.home` / `Residential_Building.residents` | 0..1 to many |

`Person.inc`, `Building.inc` and `ResidentialBuilding.inc` are the same method
bodies as the `Building` demo: random seeding in `CompleteCreate`,
`TPerson.BorrowFrom`, and `TResidential_Building.ChargeRent`, which divides the
rent among the residents, evicts anybody who goes negative by setting
`Home := nil`, and pays the owners.

## The COM layer, in four files

Exposing a Bold model over COM needs an interface per class, and the generator
writes them.

| File | What it is |
| --- | --- |
| `BuildingsAndOwners.idl` | the generated interface definition: `IBuilding`, `IPerson`, `IOwnership`, `IResidential_Building` |
| `BuildingsAndOwners.tlb` | the compiled type library, built from the IDL |
| `BuildingsAndOwners_TLB.pas` | the Delphi import of that type library, used by both sides |
| `BuildingsAndOwners_Adapters.pas` | a `TBoldComObjectAdapter` descendant per class, wrapping a `TBoldObject` and implementing its interface |

The header comment in `_Adapters.pas` documents the one manual step, because the
`.tlb` cannot be generated from the IDL by the framework: create a new type
library in the IDE, add the Bold ObjectSpace type library under Uses, paste the
IDL into the text tab, and save it next to the unit.

## Exporting the system, two ways

`Server\DMServer.dfm` exports the same system handle twice, under two names and
two export modes:

| Handle | `ObjectName` | `ExportMode` |
| --- | --- | --- |
| `SystemHandleExporter` | `System` | `emHandle` |
| `SystemExporter` | `BoldSystem` | `emValue` |

The client's `TBoldSystemHandleCom` asks for `System`, that is, for the handle
rather than for its value. `BoldSystem` is exported alongside it and no project
in this folder connects to it; it is there to show that the same element can be
published either way.

`DMServer.pas` also links the type library into the executable:

```pascal
{$R '..\core\BuildingsAndOwners.TLB'}
```

## The client is a normal Bold form with different type names

`Client\DMClient.dfm` is the whole client side of the plumbing: a
`TBoldComConnectionHandle` naming the server, and a `TBoldSystemHandleCom` on
`ObjectName = 'System'` with `IsDefault` set.

Everything on `ClientGui\Mainform` is then rooted on that, and the expressions
are ordinary OCL:

```
Person.allInstances          Building.allInstances
Residential_Building.allInstances
ownedBuildings   owners   home   residents
Person.allInstances->size
lastName + ', '+firstName
if ocliskindof(Residential_Building) then totalRent else 0 endif
residents.Assets->Sum
```

Method calls go through the generated interface:

```pascal
(blhAllResidentialBuilding.CurrentBoldObject as IResidential_Building).ChargeRent;
```

and creating an object is done by name rather than by constructor, since the
client has no `TPerson` class to construct:

```pascal
DMClientSystem.SystemHandle.System.CreateNewObject('Person', true);
```

The renderers in `ClientGui\Renderers.pas` are the COM variants:
`TBoldAsStringRendererCom` and `TBoldAsCheckBoxStateRendererCom`. Their event
signatures differ from the direct ones, taking an `IBoldElement`, a
representation, an expression and a `TBoldComClientSubscriber` rather than a
`TBoldFollower`. The bodies do the same work.

Saving is a server-side operation, which the close handler states plainly:

> There are unsaved changes **on the server**, do you want to save these to the
> db before disconnecting?

## What the COM client cannot do

The demo is honest about the gaps, in the form of code that is compiled out or
commented out. Read `ClientGui\Mainform.pas` for these:

- **Filters and comparers.** Both check-box handlers are wrapped in
  `{$IFNDEF BOLDCOMCLIENT}`, so in the client build they do nothing. A
  `TBoldFilter` and a `TBoldComparer` are Delphi objects with Delphi callbacks;
  there is nothing to attach them to across the wire.
- **Auto forms.** `ShowInOwnWindow` collects the object and then the
  `AutoFormProviderRegistry` call is commented out.
- **New building from the menu** and `EnsureObjects` are commented out too.

So the COM object space supports handles, expressions, navigation, editing and
method calls, and does not support the parts of the framework that take a Delphi
callback.

## `Gui\` versus `ClientGui\`

Both folders contain a unit called `mainform` declaring a form called
`Tallform`, and both contain a `Renderers` unit declaring `TDataModule2`. They
are the same screen written against the two APIs:

| | `Gui\` | `ClientGui\` |
| --- | --- | --- |
| first line | `{$UNDEF BOLDCOMCLIENT}` | `{$DEFINE BOLDCOMCLIENT}` |
| controls | `TBoldGrid`, `TBoldEdit`, `TBoldListBox` | `TBoldGridCom`, `TBoldEditCom`, `TBoldListBoxCom` |
| handles | `TBoldListHandle`, `TBoldExpressionHandle` | `TBoldListHandleCom`, `TBoldExpressionHandleCom` |
| objects | `TPerson`, `TResidential_Building` | `IPerson`, `IResidential_Building` |
| creating | `TPerson.Create(nil)` | `System.CreateNewObject('Person', true)` |
| filter and comparer | compiled in | compiled out |
| auto forms | `AutoFormProviderRegistry.FormForElement(...).Show` | commented out |

**No `.dpr` in this folder references `Gui\`.** All three projects use
`ClientGui\`. The direct version is there to be diffed against the COM one, and
its form file is a smaller, older layout than the COM form's.

## Files

| Path | What it holds |
| --- | --- |
| `Core\DMCore.pas` / `.dfm` | the model, the system and persistence handles, the IB database, a filter and a comparer (binary form file) |
| `Core\BuildingsAndOwners*.pas`, `.inc`, `.idl`, `.tlb` | generated classes, interfaces and adapters |
| `Core\Person.inc`, `Building.inc`, `ResidentialBuilding.inc` | the method bodies |
| `Server\DMServer.pas` / `.dfm` | the two element exporters and the COM server handle |
| `Server\FServerConsole.pas` / `.dfm` | the server window and its DB actions |
| `Server\ServerCode.pas` | registers the COM server factory |
| `Client\DMClient.pas` / `.dfm` | the connection handle and the COM system handle |
| `ClientGui\Mainform.pas`, `Renderers.pas` | the COM client UI |
| `Gui\Mainform.pas`, `Renderers.pas` | the direct-API twin, referenced by no project |

`Server.exe` is checked in, along with `Server.dof` and `Server.cfg` from the
Delphi 5 era.
