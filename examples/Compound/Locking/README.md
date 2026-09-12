# Locking Demo

Two or more clients editing the same objects at the same time, with a lock
manager deciding who is allowed to. This is the only demo in the repository that
exercises Bold's pessimistic locking, its change propagation between clients,
and its optimistic locking, and it lets you switch each of the three on and off
independently at startup so you can see what each one buys you.

The interesting part is that locks are not taken on attributes. They are taken
on **regions**, declared in the model, and a region can pull in neighbouring
objects. Two clients can collide without ever touching the same field.

## The projects

Four projects live here. The fifth executable, the one everything else connects
to, lives in `Source`.

| Project | What it is | Where |
| --- | --- | --- |
| `LockingDemo.dpr` | The client. Run two or more copies, each with its own user name | `Locking\` |
| `LockingPServer.dpr` | Persistence server. A COM server exposing a Bold SOAP persistence handle, with a snooper in front of the database | `Locking\Server\` |
| `DbGen.dpr` | One button, creates `LockingDemo.gdb` and the Bold schema | `Locking\Server\` |
| `LockManagerAdminClient.dpr` | Monitor and administration tool for the lock manager | `Locking\LockManagerAdminClient\` |
| `BoldPropagator.dproj` | **Not in this folder.** The propagator and lock manager COM server | `Source\Propagator\` |
| `ModelDM.pas` | The shared model: `Item`, `Colour`, `PurchaseOrder`, `OrderLine` | `Locking\Common\` |

`LockingPServer.exe` and the two clients all connect to `BoldPropagator.exe` by
CLSID `{11C6E940-CEC6-45BA-873D-27854A82A023}`. `TBoldComConnectionHandle` uses
`ServerCLSID` when it is set and falls back to the ProgID only when it is blank,
so the fact that the admin client stores a different ProgID string than the
others makes no difference.

## Running it

1. Build and run `DbGen.exe`, press **Create DB**. It creates `LockingDemo.gdb`
   with `sysdba` / `masterkey`.
2. Move that file next to `LockingPServer.exe`. The server rewrites its database
   name in `IBDatabase1BeforeConnect` to
   `'localhost:' + ExtractFilePath(Application.ExeName) + 'LOCKINGDEMO.GDB'`.
3. Build `Source\Propagator\BoldPropagator.dproj`, register it as a COM server
   and start it. Nothing else will connect until it is running.
4. Start `LockingPServer.exe`. Its `TdmPServer.Loaded` activates the database
   handle and the SOAP server handle (deliberately in `Loaded` rather than
   `OnCreate`, because the first client can arrive before `OnCreate` runs). Its
   propagator and lock manager connection handles have `AutoConnect = True`.
5. Start two copies of `LockingDemo.exe`. In each one type the server's machine
   name into **Server Machine Name**, a *different* name into **User Name**,
   tick the boxes you want, and press **Start System**.
6. Optionally start `LockManagerAdminClient.exe` and press **Connect**.

`Button4Click` is where the configuration is applied, and it only reads the edit
boxes and checkboxes while the system is closed. Stop the system before changing
anything.

## The main idea

### The client's persistence chain

Reading `MainDM.dfm` from the system handle downwards:

```
bshLocking (TBoldSystemHandle)
  -> BoldIdAdderHandle1 (TBoldIdAdderHandle)
       -> BoldSOAPClientPersistenceHandle1 (object 'SOAPPersistenceHandle')
            -> bcchPersistence (COM, LockingPServer.LockingDemoPSrv)
```

Beside it, and just as important:

| Component | Job |
| --- | --- |
| `BoldListenerHandle1` | registers this client with the propagator. Lease 300000 ms, extended at 80 percent, polled every 5000 ms |
| `BoldExternalObjectSpaceEventHandler1` | applies other clients' committed changes into `bshLocking` |
| `BoldIdAdderHandle1` | knows about the listener, so this client's own saves are not echoed back to it |
| `BoldLockingHandle1` | ties the system handle, the listener and `BoldLockManagerHandleCom1` together |

### The server's chain

```
BoldSOAPServerPersistenceHandle1 (class 'LockingDemoPSrv')
  -> BoldSnooperHandle1  (CheckDatabaseLock = True,
                          LockManagerHandle, PropagatorHandle)
       -> BoldPersistenceHandleDB1 -> BoldDatabaseAdapterIB1 -> IBDatabase1
```

`TBoldSnooperHandle` is the component that makes multi-client Bold work. It sees
every update on its way to the database, publishes what changed to the
propagator so the other clients hear about it, and refuses the update when the
lock manager says someone else holds the affected regions.

### Regions

The model's `Bold.RegionDefinitions` tagged value, reproduced in a comment in
`MainDM.pas`:

```
Region1[Item]:          Name, Price | Region1[Colour]
Region1[Colour]:        Name
Region1[PurchaseOrder]: OrderNo | Region1[OrderLine]
Region1[OrderLine]:     quantity, PurchaseOrder
```

A region names the members it covers and, after the bar, the regions it drags in
with it. Locking an `Item` therefore also locks its `Colour`, and locking an
`OrderLine` also locks its `PurchaseOrder`. The comment notes that this model
does not use a default region, so an object outside these definitions is not
locked at all.

### What contention looks like

When a client tries to modify an object whose region another client holds, Bold
raises `EBoldGetLocksFailed`. The demo catches it in
`BoldExceptionHandler1.OnApplyException`, walks `ConflictingRegions` for the
root object of each, reads `ClientIds` for the names of the holders, shows one
message naming objects, users, the component and the element, and sets
`Discard := true` so the edit is rolled back instead of the exception escaping to
the user.

To reproduce it: open two clients with pessimistic locking ticked, start editing
an Item's name in one, then try the same Item in the other.

### The pessimistic database lock

With **PessimisticLocking** ticked, `bshLocking.OnPreUpdate` takes an exclusive
lock on the whole database before saving:

```pascal
if not BoldLockingHandle1.LockHolder.LockDatabase then
  raise Exception.Create('Cannot get database lock');
ShowMessage('Got exclusive database lock. Press OK to update.');
```

The message box is deliberate. It holds the lock open for as long as you leave
it on screen, which is what lets you watch the other client fail.

### Optimistic locking

The third checkbox does not touch a component. It rewrites a model tagged value
in memory and tells the model it changed:

```pascal
dmModel.BoldModel1.MoldModel.BoldTVByName[TAG_OPTIMISTICLOCKING] :=
  TV_OPTIMISTICLOCKING_TIMESTAMP;   // or TV_OPTIMISTICLOCKING_OFF
dmModel.BoldModel1.SendEvent(dmModel.BoldModel1, beModelChanged);
```

The model as shipped has `Bold.OptimisticLocking=TimeStamp`. Timestamp mode
makes the server reject an update whose objects have changed since the client
read them, which is a conflict detected at save time rather than prevented at
edit time. The two strategies are independent and the demo lets you run either,
both or neither.

Pessimistic locking does need the propagator, and the two checkbox handlers
enforce that: ticking pessimistic ticks propagator, unticking propagator unticks
pessimistic.

### Selective update and enclosure

The group box at the bottom is a separate lesson. Drag objects from any grid
into the list box (a `TBoldListBox` over a `TBoldCursorHandle` on a
`TBoldVariableHandle` of type `Collection(BusinessClassesRoot)`), then
**Update** calls `UpdateDatabaseWithList` to save only those objects, and
**Enclosure** calls `EnsureEnclosure`, which works out which *other* objects
must be saved in the same transaction for the result to be consistent, adds them
to the list, and says so.

## The lock manager admin client

`TBoldLockManagerAdminHandleCom` over the same COM connection. The status tab
binds a radio group to `LockManagerSuspended`, so the whole lock manager can be
suspended and resumed. The clients tab lists either `ListAllClients` or
`ListLockingClients` depending on its radio group, and shows the locks each
client holds with their durations. Right-click a client for **Kill**, which
calls `LockManagerAdmin.KillClient(ClientId)` and refreshes: the escape hatch
for a client that died holding locks before its lease expired.

## Notes

- `TForm1.FormCreate` shows two message boxes before the window appears. The
  first says the demo needs a licence for the "ObjectSpace Synchronization
  Server and Concurrency Management Server" extensions; those were Boldsoft
  commercial add-ons, and their source is in this repository under
  `Source/Propagator`, so no key exists or is needed. The second is
  `ShowMessage(GuidToString(WIN_TRUST_SUBJTYPE_RAW_FILE))`, which prints an
  unrelated Windows GUID and is plainly leftover debugging. Both can be deleted.
- The admin client's main form has a **Server Name** edit, and its data module
  has a `ServerName` property that reads it, but nothing ever assigns it to
  `BoldComConnectionHandle1.ServerHost`. The admin client always talks to the
  propagator on the local machine.
- `bcchPersistence.ServerHost` is stored in the DFM as `locahost`, misspelt.
  It never takes effect because **Start System** overwrites it from the edit box.
- Running the clients and the servers on different machines means configuring
  DCOM. Everything on one machine is much easier and shows the same behaviour.
- Persistence is IBX and InterBase through `TBoldDatabaseAdapterIB`, from
  `Source/Deprecated/Persistance/IBX`, which this repository does not maintain.
  There are no `.dproj` files here either, only `.dpr` and one Delphi 5 era
  `.dof`.
