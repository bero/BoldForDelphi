# HTTP Persistence Mapper

The smallest possible three-tier Bold application: a rich VCL client, an ISAPI
DLL that forwards Bold's persistence protocol to a database, and a one-button
tool that creates the database. The client contains no database components and
no connection string.

Where the ASP demo keeps a live object space on the server, this one does not.
The server's passthrough handle sits directly on the database handle, so it is a
pure relay. That makes it the better example of what the HTTP persistence mapper
is for, and the better one to read first.

## The projects

| Project | What it is | Main unit |
| --- | --- | --- |
| `BoldHTTPServer.dpr` | ISAPI DLL (`WebBroker`, `ISAPIApp`). One web action that relays Bold requests | `HTTPServerDll\MainUnit.pas` |
| `HTTPClient.dpr` | The VCL client. Buildings, owners and residents, with renderers, a filter and a comparer | `Client\mainform.pas` |
| `DBGen.dpr` | Creates `httpPMapper.gdb` and the Bold schema | `DBGenerator\dbGenForm.pas` |

Three shared units in `Common\`:

| Unit | Holds | Used by |
| --- | --- | --- |
| `dmCoreUnit` | `BoldModel1`, the one `TBoldModel` | all three projects |
| `dmPersistenceUnit` | `TBoldPersistenceHandleDB`, `TBoldDatabaseAdapterIB`, `TIBDatabase` | server and `DBGen` only |
| `BuildingClasses.pas` and the `.inc` files | generated classes and method bodies | client and server |

The client's project file deliberately does not include `dmPersistenceUnit`.

## Running it

1. Build and run `DBGen.exe`, press **Create database**. It calls
   `EnsureInterbaseDatabase` and then `CreateDataBaseSchema`, logging in as
   `sysdba` / `masterkey`.
2. Put `httpPMapper.gdb` next to the deployed DLL. Both projects share
   `dmPersistenceUnit`, whose `IBDatabase1BeforeConnect` resolves the file
   relative to the running module, so `DBGen` creates it beside `DBGen.exe` and
   the server looks for it beside `BoldHTTPServer.dll`.
3. Deploy `BoldHTTPServer.dll` to an IIS virtual directory that may execute
   ISAPI extensions. The client's stored URL assumes the directory is called
   `Bold`.
4. Run `HTTPClient.exe` and press **Open system**.

The URL the client posts to is stored in the DFM and there is no edit box for
it, so change it there if your virtual directory differs:

```
BoldWebConnection1.URL = 'http://localhost/Bold/BoldHTTPServer.dll/BuildingsAndOwners'
```

## The main idea

### One component on each side

Server, in `MainUnit.dfm`:

```
object httpPMapper: TBoldHTTPServerPersistenceHandlePassthrough
  PersistenceHandle = dmPersistence.BoldPersistenceHandleDB1
  BoldModel = dmCore.BoldModel1
end
```

and the web action is `httpPMapper.Get(Request.content, reply)`.

Client, in `datamod.dfm`:

```
BoldSystemHandle1.PersistenceHandle = BoldHTTPClientPersistenceHandle1
BoldHTTPClientPersistenceHandle1.WebConnection = BoldWebConnection1
```

`TBoldHTTPClientPersistenceHandle` occupies the slot a
`TBoldPersistenceHandleDB` would occupy in a two-tier application. Nothing above
it, not the list handles, not the grids, not the OCL, is aware of the change.

### The path names do not match, and it still works

The server's action declares `PathInfo = '/BuildingAndOwners'` (no `s` on
`Building`). The client posts to `/BuildingsAndOwners`. The action also carries
`Default = True`, and WebBroker routes any unmatched path to the default action,
which is why the mismatch is invisible. Clear that flag and the demo stops
working with a 404.

### What the client shows off

`datamod` is a catalogue of the Bold control pack, all of it working unchanged
over HTTP:

| Component | Behaviour |
| --- | --- |
| `FullNameRenderer` | `TBoldAsStringRenderer` formatting `LastName, FirstName`, subscribing to both attributes |
| `IsRichRenderer` | `TBoldAsCheckBoxStateRenderer`, ticked when `Assets > 10000` |
| `IsRichFilter` | `TBoldFilter` on the same condition, installed and removed by a checkbox at runtime |
| `NameComparer` | `TBoldComparer` sorting by last name then first name, also toggled at runtime |
| `NegativeRedRenderer` | paints assets red when negative, blue otherwise |

`mainform` adds two more worth reading. `bsrRentPerResident` is a fully
bidirectional computed column: it reads `TotalRent / Residents.Count`, refuses
to be edited when there are no residents (`OnMayModify`), validates each
keystroke and the whole string, and on write multiplies back by the resident
count. `bsrAddress` colours a row by the text of the address, which is how the
demo data ended up with streets called Bold Drive and Rose Path.

## Notes

- `IBDatabase1BeforeConnect` builds the path as
  `'localhost:' + GetModuleFileNameAsString(True) + ExtractFileName(...)`.
  `GetModuleFileNameAsString(True)` includes the module's own file name, so the
  result is `...\DBGen.exehttpPMapper.gdb`. It needs `ExtractFilePath` around
  it.
- `Client\mainform.pas` uses the bare `DecimalSeparator` global, which modern
  Delphi does not declare (it is `FormatSettings.DecimalSeparator` now). The
  client does not compile unmodified.
- `Client\datamod.pas` lists `BoldBDEInterfaces` in its uses clause although the
  client has no BDE component. That unit lives in
  `Source/Deprecated/Persistance/BDE`.
- `BoldUMLRoseLink1.FileName` still points at
  `C:\vss\dev\BfD\examples\Compound\Building\Building.mdl`, a path from the
  original Boldsoft build machine.
- Persistence is IBX and InterBase through `TBoldDatabaseAdapterIB`, from
  `Source/Deprecated/Persistance/IBX`.
- There are no `.dproj` files here, only `.dpr`.
