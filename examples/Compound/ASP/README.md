# ASP Demo

A Bold client that keeps no database of its own. Its whole object space is
served over HTTP by a WebBroker ISAPI extension, which also exposes two named
server-side operations the client can invoke by name.

The point is the split: the same `TBoldSystemHandle` API works whether the
persistence handle underneath it talks to a database or to a web server, and
the client never learns which. The second endpoint shows the other half of the
picture, business logic that has to run on the server because that is where the
objects live.

## Obsolete before you start

Nothing here is ASP. There is no `.asp` file in the folder and no scripting
host involved. The server is a Delphi WebBroker **ISAPI DLL**
(`library ASPserver ... uses WebBroker, ISAPIApp`). The name survives only in
the client's hard-coded URL, `http://n87zb/ASPdemo/ASPserver.dll/persistence`,
which was a virtual directory called `ASPdemo` on a machine called `n87zb`.

Both server-side projects were **ported off IBX to FireDAC with SQLite**, so
nothing has to be installed to build them and there is no InterBase server to
set up. `DBGenerator.exe` and `ASPserver.dll` both build, and the server has
been confirmed to open the database the generator writes.

The client was stuck too. `Client\ClientMainForm.dfm` was a **byte-for-byte
copy of `ClientDM.dfm`**, so `ASPDemoClient` had no resource for `TForm1` and
would not open - true since the 2020 source release. The form has been rebuilt
from the component list in `ClientMainForm.pas`, so the project loads, compiles
and streams again. The layout is a reconstruction: the original arrangement is
not in the repository and cannot be recovered from it.

One thing still stands between that and a running demo:

- Hosting an ISAPI extension needs IIS with the ISAPI-Extensions role service
  and an application pool whose bitness matches the DLL. The setup is written
  out under **Hosting the ISAPI extension** below, but nothing behind the
  request dispatch - the persistence endpoint, the SOAP action endpoint - has
  been exercised on a running server.

What follows describes what the code does, which is still worth reading.

## The projects

| Project | What it is | Where |
| --- | --- | --- |
| `ASPserver.dpr` | ISAPI DLL. Holds the model, the database connection and a live server-side object space | `Server\` |
| `DBGenerator.dpr` | Small VCL exe. One button that creates `ASPDemo.db` (SQLite, through FireDAC) and the Bold schema in it | `Server\` |
| `ASPDemoClient.dpr` | VCL client. No database components at all | `Client\` |
| `ModelDM.pas` | `TBoldModel` named `bmASPDemo`, used by the client and by DBGenerator | `Common\` |
| `BuildingClasses.pas` plus the `.inc` files | Generated classes and the hand-written method bodies | `Common\` |

The server does **not** use `Common\ModelDM.pas`. `MainWebModule.dfm` carries
its own `TBoldModel` with its own copy of the model text. The two copies have
to stay in step by hand.

## Running it

1. Build `DBGenerator.exe`, run it and press **Create DB**. It calls
   `BoldPersistenceHandleDB1.CreateDataBaseSchema`, which writes 15 tables: the
   11 `BOLD_*` system tables plus `Building`, `Ownership`, `Person` and
   `Residential_Building`. Pressing it again deletes the existing database
   first, after asking. Nothing needs to be installed - SQLite is a file.

   `DBGenerator.ini` holds the database name (`ASPDemo.db` by default). A bare
   name is taken as relative to the executable, so the demo does not depend on
   the working directory. The project also has a `DebugUniDAC` build
   configuration that uses UniDAC instead of FireDAC; it needs the `UniDAC`
   environment variable pointing at a UniDAC 11 installation, and writes its
   exe to `Server\UniDAC\`.
2. Nothing to move while developing. `DBGenerator.exe` writes the database
   beside itself, and both it and `ASPserver.dll` build into `Server\`, so the
   server already finds it - it looks for the database beside its own module,
   not beside the running executable (see the note on locating the database
   below).

   The `DebugUniDAC` build is the exception: it writes to `Server\UniDAC\`, so
   the database lands there too. Copy `ASPDemo.db` up one level, or point
   `ASPserver.ini` at the one in `UniDAC\`.

   When deploying for real, `ASPDemo.db` has to travel with `ASPserver.dll`
   into the IIS virtual directory. The application pool identity needs write
   access to the file **and** to the folder holding it, because SQLite creates
   journal files next to the database.
3. **Register the BoldSOAP type library on the client machine.** The client
   reaches Bold's SOAP layer through `TBoldHTTPSOAPService`, whose constructor
   calls `LoadRegTypeLib(LIBID_BoldSOAP, ...)`. That looks the type library up
   in the registry by GUID, so without the registration the client raises

   ```
   TBoldHTTPSOAPService.Create: Unable to load type library LIBID_BoldSOAP
   ```

   the moment you press **Open system**. The type library itself is in the
   repository - `Source\Common\SOAP\BoldSOAP.tlb` - and Delphi ships the tool
   that registers it. Nothing has to be built and no administrator rights are
   needed:

   ```powershell
   & 'C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\tregsvr.exe' -c `
       'C:\path\to\BoldForDelphi\Source\Common\SOAP\BoldSOAP.tlb'
   ```

   Both paths are spelled out on purpose. `$env:BDS` is only defined inside a
   Delphi-configured environment - `rsvars.bat`, or the IDE's own command
   prompt - and is empty in an ordinary session, where it silently collapses
   the path to `\bin\tregsvr.exe`. The `23.0` is Delphi 12.3; use `22.0` for
   11.3 or `37.0` for 13.

   `-c` registers for the current user, under `HKCU\Software\Classes`, which
   merges into `HKEY_CLASSES_ROOT` for that account. Drop `-c` to register for
   the whole machine, which does need an elevated prompt. `-u` unregisters.

   **Check it took:**

   ```powershell
   Test-Path 'HKCU:\Software\Classes\TypeLib\{9BF07220-6C8A-11D4-BBAC-0010A4F9E114}'
   ```

   Nothing needs restarting - the client looks the library up when
   `TBoldHTTPSOAPService` is constructed, so just press **Open system** again.

   **Do not use `regsvr32` here.** `Source\Common\SOAP\BoldSOAP.dpr` looks like
   the way in - it is an ActiveX library that embeds the same `.tlb` and
   exports `DllRegisterServer` - but registering it fails with
   `0x80004005`. `DllRegisterServer` calls `ComServer.UpdateRegistry`, which
   registers coclasses and their factories, and `BoldSOAP` has none: it is a
   pure type library, interfaces and a LIBID only. `tregsvr -t` registers the
   type library itself, which is the only thing `LoadRegTypeLib` looks for.

   Registration records the *path* of the `.tlb`, not a copy of it, so the file
   has to stay where it is. The error names only a LIBID, so this is worth
   doing before anything else - nothing else in the demo points at it.

   **The server does not need this.** `wmASPServerSOAPCallsAction` calls
   `BoldXMLDispatcher1.DispatchAction` directly. The `LoadRegTypeLib` call on
   that side lives in `TBoldXMLSOAPService.Create`, reached only through
   `TBoldXMLDispatcher.GetComObject` - what a COM server handle would call, and
   nothing in this demo does. Registering the type library on the IIS machine is
   harmless but pointless.
4. Deploy `ASPserver.dll` into an IIS application that is allowed to execute
   ISAPI extensions. This is the one step with real setup behind it - see
   **Hosting the ISAPI extension** below for the commands.
5. Run `ASPDemoClient.exe`, type the URL root into the edit box, for example
   `http://localhost/ASPdemo/ASPserver.dll`, and press **Connect**. That sets
   both web connections:

   ```
   BoldWebConnection1.URL := edtURLRoot.Text + '/persistence';
   BoldWebConnection2.URL := edtURLRoot.Text + '/soapcalls';
   ```

6. Press **Open system**. The grids fill from the server.

## Hosting the ISAPI extension

Step 4 of *Running it* is the only one with real setup behind it. Four facts
about this particular DLL decide the settings:

- **It is 32-bit.** `ASPserver.dproj` targets `Win32` only, and the built DLL's
  PE machine field reads `0x014C` (i386). On 64-bit Windows that forces one
  application-pool setting, and getting it wrong gives a `500` carrying
  `0x800700c1` rather than anything that names the cause.
- **It links statically.** The project uses no runtime packages, so there are no
  BPLs to deploy. FireDAC links the SQLite engine statically as well, so there
  is no `sqlite3.dll` either. One file.
- **It needs its database beside it.** `ASPserver.ini` names `ASPDemo.db`, and a
  bare name is resolved against `ModuleDirectory` - the folder holding the DLL,
  not the folder holding the worker process.
- **It does not need the BoldSOAP type library.** Only the client does; see
  step 3 of *Running it*.

Everything below is PowerShell, and all of it needs an **elevated** session:
`appcmd` writes `applicationHost.config`, and `Enable-WindowsOptionalFeature`
refuses outright with *The requested operation requires elevation*.

Set these three once; the rest of the section uses them. `$server` is the only
one to edit - point it at wherever the repository sits:

```powershell
$server = 'C:\path\to\BoldForDelphi\examples\Compound\ASP\Server'
$deploy = 'C:\inetpub\ASPdemo'
$appcmd = "$env:windir\system32\inetsrv\appcmd.exe"
```

### 1. Install IIS with the ISAPI-Extensions role service

```powershell
Enable-WindowsOptionalFeature -Online -FeatureName IIS-ISAPIExtensions -All
Enable-WindowsOptionalFeature -Online -FeatureName IIS-ManagementConsole -All
```

`-All` pulls in the parent features. To look before installing:

```powershell
Get-WindowsOptionalFeature -Online -FeatureName IIS-ISAPIExtensions |
    Select-Object FeatureName, State
```

On Windows Server the same thing is *Add Roles and Features > Web Server (IIS) >
Application Development > ISAPI Extensions*. Without it the `ISAPI-dll` handler
is not registered at all, and the request falls through to the static file
handler.

### 2. Lay out the deployment folder

```powershell
New-Item -ItemType Directory -Force $deploy | Out-Null
Copy-Item "$server\ASPserver.dll", "$server\ASPserver.ini", "$server\ASPDemo.db" $deploy
```

Run `DBGenerator.exe` first if `ASPDemo.db` does not exist yet.

`$deploy` can point inside the working tree if that is more convenient than
`C:\inetpub`. Note what `.gitignore` does and does not cover there: `*.dll` and
`*.db` are ignored, but `ASPserver.ini` and the `ASPDemo.db-journal` that SQLite
writes at runtime are not, so they will show up in `git status`.

### 3. Create a 32-bit application pool with no managed code

```powershell
& $appcmd add apppool /name:ASPdemo /managedRuntimeVersion: /enable32BitAppOnWin64:true
```

`/managedRuntimeVersion:` with nothing after the colon means "No Managed Code";
no .NET is involved here, and PowerShell passes the bare trailing colon through
unchanged. `enable32BitAppOnWin64:true` is the load-bearing one.

### 4. Create the application

```powershell
& $appcmd add app '/site.name:Default Web Site' /path:/ASPdemo "/physicalPath:$deploy" /applicationPool:ASPdemo
```

`add app`, not `add vdir`. A plain virtual directory inherits the parent's
application pool, so it would run 64-bit and fail to load the DLL. The
`/ASPdemo` path segment is what makes the client's URL root
`http://localhost/ASPdemo/ASPserver.dll` resolve.

### 5. Allow Execute on that application

```powershell
& $appcmd set config 'Default Web Site/ASPdemo' /section:handlers /accessPolicy:Read,Script,Execute /commit:apphost
```

The default policy is `Read,Script` - **Execute is not in it**, and an ISAPI
extension needs it. `/commit:apphost` is required because the `handlers` section
is locked at site level; without it the command fails and browsing gives
`500.19`.

### 6. Unblock this DLL in the ISAPI/CGI restriction list

```powershell
& $appcmd set config /section:isapiCgiRestriction `
    "/+[path='$deploy\ASPserver.dll',allowed='true',description='Bold ASP demo']" `
    /commit:apphost
```

`notListedIsapisAllowed` is `false` out of the box, so an unlisted extension is
refused. The path is matched literally - re-add it if the DLL moves.

Two PowerShell details in that command. The line-continuation character is a
backtick, not `^`. And the bracketed argument is wrapped in **double** quotes so
`$deploy` expands, while the inner values keep the single quotes `appcmd` wants;
PowerShell re-quotes the whole thing into one argument on the way out, the
spaces in the description included.

### 7. Give the pool identity write access to the folder

```powershell
icacls $deploy /grant 'IIS AppPool\ASPdemo:(OI)(CI)M'
```

**The single quotes are required.** `(OI)` and `(CI)` - the Object Inherit and
Container Inherit flags - are PowerShell subexpression syntax when unquoted, and
the command fails with *The term 'OI' is not recognized*. Do not put inner
double quotes around the account name either, the way a `cmd` example would:
PowerShell passes those through literally and `icacls` then fails with *No
mapping between account names and security IDs was done*, having looked up an
account whose name includes the quote characters.

`IIS AppPool\ASPdemo` is a virtual account that does not exist until step 3 has
created the pool, so running this out of order gives that same *No mapping*
error for a real reason.

Modify on the **folder**, not just on the file: SQLite writes
`ASPDemo.db-journal` beside the database, so a read-only directory fails at the
first write with `unable to open database file`. `M` rather than `F` - the
worker process has no business rewriting the ACL.

### 8. Check that the extension loads

```powershell
$r = Invoke-WebRequest 'http://localhost/ASPdemo/ASPserver.dll/persistence' -SkipHttpErrorCheck
$r.StatusCode
$r.Content
```

`-SkipHttpErrorCheck` needs PowerShell 7. On Windows PowerShell 5.1 a 4xx or 5xx
throws instead, so wrap the call in `try`/`catch` there, or use
`curl.exe -i <url>` - spelled with the extension, because `curl` is an alias for
`Invoke-WebRequest` in 5.1.

What this checks is who answered. A WebBroker response - including an exception
page, which is what an empty request body deserves - means the DLL loaded and
dispatched. An IIS error page means it never ran:

| Status | Cause | Go back to |
| --- | --- | --- |
| `404.17` | the request went to the static file handler | 1, ISAPI Extensions |
| `404.2` | ISAPI/CGI restriction | 6, the restriction list |
| `403.1` | Execute access denied | 5, the access policy |
| `500.0` with `0x800700c1` | a 64-bit pool loading a 32-bit DLL | 3, the pool bitness |
| `500.19` | configuration section locked | add `/commit:apphost` |

### Two things nobody has exercised

Neither of these has been run, so they are named as the likely next obstacles,
not as established behaviour.

- **COM initialization on the IIS thread.** `TBoldXMLRequest` builds its
  document with `CoDOMDocument.Create` - a `CoCreateInstance` against MSXML,
  since `OXML` is not defined anywhere in the source. If the worker thread is
  not COM-initialized for that apartment, `/soapcalls` raises `CoInitialize has
  not been called` (`0x800401F0`), and a `CoInitialize` / `CoUninitialize` pair
  around the body of `wmASPServerSOAPCallsAction` is the fix. `/persistence`
  never touches the DOM, so it can work while `/soapcalls` fails - that is not a
  networking problem.
- **The message box.** `TResidential_Building.ChargeRent` calls `ShowMessage` on
  eviction, as the notes below say. Inside `w3wp.exe` that blocks the request
  thread with nobody to dismiss it, so **Charge rent** hangs rather than errors.

## Using another database engine

SQLite is only the default, chosen so the demo needs nothing installed. Bold
itself speaks a range of dialects - the `TBoldDataBaseEngine` values in
`Source\PMapper\SQL\BoldSQLDatabaseConfig.pas` are `dbeInterbaseSQLDialect1`,
`dbeInterbaseSQLDialect3`, `dbeGenericANSISQL92`, `dbeSQLServer`, `dbePostgres`,
`dbeMySQL`, `dbeDBISAM`, `dbeOracle`, `dbeAdvantage`, `dbeParadox` and
`dbeInformix`.

The generator picks its engine in code rather than from the ini, so switching
means editing `DBGeneratorForm.pas`. Three things have to agree:

1. **The driver.** `DriverID=` for FireDAC (`cDriverSQLite`), or `ProviderName`
   for the UniDAC build (`cProviderSQLite`).
2. **The connection parameters.** A server engine wants `Server`, `User_Name`
   and `Password` instead of a bare file name.
3. **Bold's dialect.** `Adapter.DatabaseEngine`, currently
   `dbeGenericANSISQL92`.

Points 1 and 3 are independent settings and both have to be right: the driver
decides who talks to the database, `DatabaseEngine` decides what SQL Bold
generates. Setting one and not the other produces SQL the server rejects rather
than a clear error.

One piece of the generator is deliberately file-engine-specific. `Button1Click`
deletes the database file before recreating it, and the comment above it
explains why it does not call `TBoldDatabaseAdapterFireDAC.CreateDatabase` or
`DatabaseExists`: both are written for server engines, and under
`dbeGenericANSISQL92` the latter raises because `DatabaseExistsTemplate` is
empty. For a server engine you would use those adapter calls instead of
deleting a file.

For a demo that switches engine from a menu, with the preconditions checked
before it tries, see `examples\Simple\Tools\OclWorkbench` - it drives the shared
`examples\Shared\DemoDataModule.pas`, which reads its engine from an ini. This
ASP demo predates that and uses its own `Common\ModelDM.pas` instead.

## The main idea

### The persistence endpoint

`TBoldHTTPServerPersistenceHandlePassthrough` is the server end of Bold's
persistence protocol. The whole web action is three lines:

```pascal
BoldHTTPServerPersistenceHandlePassthrough1.Get(Request.Content, reply);
Response.Content := Reply;
Handled := true;
```

On the client, `TBoldHTTPClientPersistenceHandle` is the mirror image. It is
assigned to `BoldSystemHandle1.PersistenceHandle` exactly where a
`TBoldPersistenceHandleDB` would go, and it posts to a `TBoldWebConnection`.
That one substitution is the entire client-side change.

### The server keeps a live object space

The passthrough does not sit on the database handle. It sits on a
`TBoldPersistenceHandleSystem`, which in turn wraps the server's own
`TBoldSystemHandle`. So the server holds real Bold objects in memory and answers
client requests out of them. `WebModuleCreate` activates both handles on the
first request.

That is what makes the second endpoint possible.

### The action endpoint

`TBoldXMLDispatcher` maps an action name in an XML request to an event handler.
Two are registered:

| Action | What it does |
| --- | --- |
| `SaveToDB` | calls `BoldSystemHandle1.UpdateDatabase` on the server's own object space and returns `OK` or the exception message |
| `ChargeRent` | reads the `Building` parameter as a `TBoldDefaultId`, looks the locator up in `PersistenceControllerSystem.LocatorById`, and calls `ChargeRent` on the object |

The client builds those requests with `TBoldXMLRequest.CreateInitialized` and
posts them through `TBoldHTTPSOAPService` over the second web connection. Note
what `btnChargeRentClick` checks first: `anObjectId.IsStorable`. A building
created in the client but not yet saved has no server-side identity, so the
demo says so rather than sending a request that cannot resolve.

## Notes

- **Locating the database.** The old `IBDatabase1BeforeConnect` built the path as
  `'localhost:' + GetModuleFileNameAsString(True) + ExtractFileName(...)`.
  `GetModuleFileNameAsString(True)` returns the full path **including the module
  file name**, so the result was `...\ASPserver.dllASPDemo.gdb`. The port
  removed that handler; `ModuleDirectory` in `MainWebModule.pas` now wraps the
  call in `ExtractFilePath`. The same construction is still wrong in
  `HTTPPMapper\Common` and `XML\BoldAppDataModUnit`.

  The module handle matters here rather than `Application.ExeName`: inside an
  ISAPI extension the executable is the IIS worker process, so a database
  located relative to the exe would be looked for under `System32\inetsrv`.
- `TResidential_Building.ChargeRent` calls `ShowMessage` when a resident runs
  out of money and is evicted. Invoked through `ChargeRent` that message box
  opens inside the IIS worker process, where nobody can dismiss it.
- All three projects now have a `.dproj`, and all three are in
  `examples\DelphiExamples.groupproj`. `ASPDemoClient` has to be in the group:
  its main form links to `dmClient`, and opening the form without the project
  that owns that module makes the IDE offer to remove the links, which would
  silently unwire the four Bold handles.
- The four list handles are rooted on `dmClient.BoldSystemHandle1`.
  `blhBuildings` deliberately selects `Residential_Building.allInstances`
  rather than every `Building`: **Charge rent** posts the selected object to
  the server, which casts it to `TResidential_Building`, so a plain building
  would raise there.
