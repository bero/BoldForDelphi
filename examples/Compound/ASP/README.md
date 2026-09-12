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

Three things stop this demo working as checked in:

- `Client\ClientMainForm.dfm` is a **byte-for-byte copy of `ClientDM.dfm`**.
  The main form's layout is not in the repository, so `ASPDemoClient` has no
  resource for `TForm1`. This has been true since the 2020 source release.
- Persistence uses `TBoldDatabaseAdapterIB` and IBX against an InterBase
  `.gdb`. Both live in `Source/Deprecated/Persistance/IBX`, which this
  repository does not maintain.
- Hosting an ISAPI extension needs IIS with the ISAPI-Extensions role service
  and an application pool whose bitness matches the DLL.

What follows describes what the code does, which is still worth reading.

## The projects

| Project | What it is | Where |
| --- | --- | --- |
| `ASPserver.dpr` | ISAPI DLL. Holds the model, the database connection and a live server-side object space | `Server\` |
| `DBGenerator.dpr` | Small VCL exe. One button that creates `ASPDemo.gdb` and the Bold schema in it | `Server\` |
| `ASPDemoClient.dpr` | VCL client. No database components at all | `Client\` |
| `ModelDM.pas` | `TBoldModel` named `bmASPDemo`, used by the client and by DBGenerator | `Common\` |
| `BuildingClasses.pas` plus the `.inc` files | Generated classes and the hand-written method bodies | `Common\` |

The server does **not** use `Common\ModelDM.pas`. `MainWebModule.dfm` carries
its own `TBoldModel` with its own copy of the model text. The two copies have
to stay in step by hand.

## Running it

1. Build `DBGenerator.exe` and run it once. It calls
   `BoldDatabaseAdapterIB1.CreateInterbaseDatabase` and then
   `BoldPersistenceHandleDB1.CreateDataBaseSchema`.
2. Move `ASPDemo.gdb` next to the deployed `ASPserver.dll`. The generator writes
   it wherever InterBase resolves the bare name `ASPDemo.gdb`; the server looks
   for it beside its own module (see the note on `IBDatabase1BeforeConnect`
   below).
3. Deploy `ASPserver.dll` into an IIS virtual directory that is allowed to
   execute ISAPI extensions.
4. Run `ASPDemoClient.exe`, type the URL root into the edit box, for example
   `http://localhost/ASPdemo/ASPserver.dll`, and press **Connect**. That sets
   both web connections:

   ```
   BoldWebConnection1.URL := edtURLRoot.Text + '/persistence';
   BoldWebConnection2.URL := edtURLRoot.Text + '/soapcalls';
   ```

5. Press **Open system**. The grids fill from the server.

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

- `IBDatabase1BeforeConnect` builds the database path as
  `'localhost:' + GetModuleFileNameAsString(True) + ExtractFileName(...)`.
  `GetModuleFileNameAsString(True)` returns the full path **including the module
  file name**, so the result is `...\ASPserver.dllASPDemo.gdb`. It needs
  `ExtractFilePath` around it. The Locking demo's server gets the same
  construction right.
- `TResidential_Building.ChargeRent` calls `ShowMessage` when a resident runs
  out of money and is evicted. Invoked through `ChargeRent` that message box
  opens inside the IIS worker process, where nobody can dismiss it.
- There are no `.dproj` files anywhere in this folder, only `.dpr`.
