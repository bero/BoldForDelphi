# OrgChart over XML

The same shape as the sibling `XML` demo, but generic in two places where that
one is hand-written. The XML is produced by a reusable exporter that will walk
any object list, and the query is not fixed: the browser composes an OCL
expression in JavaScript and posts it, and the server evaluates whatever
arrives.

That makes it the better demo of the idea and a worse example of how to expose a
Bold system to the internet. Both halves are worth understanding.

## The projects

| Project | Kind | What it does |
| --- | --- | --- |
| `OrgChart.dpr` | VCL application, also a COM server | owns the model and the database, has an admin UI, and exposes a `TBoldXMLDispatcher` with three actions |
| `OrgChartISAPI.dpr` | ISAPI library | HTTP in, XML request out over COM, HTML back |

Neither has a `.dproj`. A built `OrgChart.exe` and a populated `ORGCHART.GDB`
are checked into the folder.

## Running it

1. Run `OrgChart.exe`. `ServerCode.pas` registers it as
   `OrgChart.OrgChart`, class id `{AE605A6F-9C63-4CA4-90FC-B4D75614D4D7}`.
2. The database is Interbase, `OrgChart.gdb`, through a
   `TBoldDatabaseAdapterIB` (now under `Source\Deprecated\Persistance\IBX`).
   **Create DB**, then **Open system**.
3. Press the button wired to `dmMain.GenerateData` to seed companies, offices,
   departments and people.
4. For the browser half, deploy `OrgChartISAPI.dll` with `OrgChart.htm`,
   `LastPage.htm` and the `XSL\` folder beside it, and request
   `.../OrgChartISAPI.dll/home`.

## The model

| Class | Members |
| --- | --- |
| `Company` | `CompanyName: String`, `Logo: Blob` |
| `Office` | `Name`, `phone`, `fax`, `Email`, `EstablishedDate: Date` |
| `Department` | `Name: String` |
| `Address` | `street`, `city`, `country`, `postcode`, `zip`, `state` |
| `Description` | `Content: String` |
| `Person` | `FirstName`, `LastName`, and a derived `FullName` |
| `Employment` | link class: `Title`, `PhoneExt`, `Email` |
| `OfficesOrganization` | link class, no attributes |

| Association | Shape |
| --- | --- |
| `Person.employer` / `Company.employees` | many to many through `Employment` |
| `Employment.department` | 0..1 to `Department` |
| `Company.offices` / `Office.organization` | through `OfficesOrganization` |
| `Office.departments` | 1 to many |
| `Office.address`, `Office.description` | 0..1 each |

`Person.FullName` is derived in code, in `OrgChartClasses.inc`:

```pascal
Result := FirstName + ' ' + LastName;
M_FullName.AsString := Result;
```

Note that it places no subscription, so it will not be recalculated when the
names change. In the model that would have been one line of
`Bold.DerivationOCL`.

## The three dispatcher actions

`dmServer.dfm` registers the COM object `Dispatcher` under server class
`OrgChart` with three actions:

| Action | Input | What it does |
| --- | --- | --- |
| `EvaluateOCL` (default) | an OCL string | evaluates it, exports the result, transforms by `XSL\<Class>.xslt` |
| `Fetch` | a Bold id | exports that one object, transforms by `XSL\Edit<Class>.xslt` |
| `Update` | id and value pairs | writes through the manipulator, saves, returns `LastPage.htm` |

`EvaluateOCL` is short enough to quote almost whole:

```pascal
dmMain.OrgChartSystem.System.EvaluateExpression(oclExpr, Elem, False);
ObjList := Elem.Value as TBoldObjectList;
if (ObjList is TPersonList) then
  XMLExporter.ExportObjects(ObjList, RootElement,
    ['Company', 'Office', 'Address', 'Employment', 'Department', 'Description'])
else if (Objlist is TCompanyList) then
  XMLExporter.ExportObjects(ObjList, RootElement,
    ['Person', 'Office', 'Address', 'Employment', 'Department', 'Description']);
```

The string array is the list of classes whose *referenced* objects should also
be exported, so a person comes out with the companies it is employed by attached
rather than as a dangling reference.

The stylesheet is then chosen from the result itself:

```pascal
response := XSLTransform(XMLDoc,
  Format('%s\XSL\%s.xslt', [p, ObjList.BoldObjects[0].BoldClassTypeInfo.ExpressionName]));
```

`XSLTransform` falls back to returning the raw XML when the stylesheet file does
not exist, which is a convenient way to see what the exporter produced.

`Fetch` does the same for a single object and picks `Edit<Class>.xslt`, so the
five shipped stylesheets divide into two display pages (`Person.xslt`,
`Company.xslt`) and three edit pages (`EditPerson.xslt`, `EditCompany.xslt`,
`EditDepartment.xslt`).

## The exporter

`XMLExporter\` holds three units that are **not** part of `Source\`:
`BoldXMLExporter`, `BoldXMLExportAdapters` and `BoldXMLExportInterfaces`. They
are compiled into this project directly, from a CVS path dated 2003.

`TBoldXMLExporter` walks an object or a list and emits an element per object and
per attribute, decorated with namespaced attributes such as `XML.boldID` and
`XML.idref`. It has three switches:

| Property | Effect |
| --- | --- |
| `BoldManipulator` | supplies the ids |
| `ExportLinks` | set True here, so link classes such as `Employment` come out as elements |
| `ExportDerivedAttributes` | left False |

The `idref` mechanism is what the stylesheets navigate. `Person.xslt` resolves a
person's employer by matching one against the other inside the same document:

```xml
<xsl:for-each select="../../../Company[@XML.boldID=..//@XML.idref]">
```

## The OCL comes from the browser

`OrgChart.htm` has no server-side logic at all. Its JavaScript assembles an OCL
expression from what you typed and puts it in a hidden field:

```javascript
var c1=new String("firstName.sqlLIkeCaseInsensitive('%%s%')");
var c2=new String("lastName.sqlLIkeCaseInsensitive('%%s%')");
var cond = c1.replace(re, FirstName)+ " and " + c2.replace(re, LastName);
OCLExpr= AllInstances.replace(re, "Person") + Select.replace(re, cond);
```

which produces, for a search on Mary Ananian:

```
Person.allInstances->select(firstName.sqlLIkeCaseInsensitive('%Mary%') and lastName.sqlLIkeCaseInsensitive('%Ananian%'))
```

The same two expressions are on the admin form as `blhSearchPerson` and
`blhFiltered`, which is a useful place to try them before going through the web.

`sqlLikeCaseInsensitive` is the operation you want here: it is one of the
expressions Bold can translate into SQL, so the filtering happens in the
database rather than after fetching every person.

Be clear about what this costs. The server evaluates whatever expression the
request contains, against the whole object space. There is no whitelist and no
parsing of the client's intent. Treat it as a demonstration of
`EvaluateExpression` over a transport, not as a pattern to copy into a public
service.

## The admin form

`MainForm` is an ordinary Bold VCL form over the same system: grids of companies
and people, the offices of the current company, its departments and employees, a
`TBoldImage` on `logo` with a Load-from-file button, an address group box bound
through a `TBoldExpressionHandle` on `address`, and an Add Address action that
creates a `TAddress` when the current office has none.

Two of the columns are worth copying:

```
employment->first.title
employment.department->first.name
```

Both reach through a link class and take the first element, which is how you
render a many-to-many attribute in a single cell.

## Files

| File | What it holds |
| --- | --- |
| `OrgChart.dpr` | the server application: `dmMain`, `ServerDataModule`, `frmMain` |
| `MainDataModule.pas` / `.dfm` | the model, the system handle, the IB database, the manipulator, `GenerateData` |
| `dmServer.pas` / `.dfm` | the dispatcher, its three action handlers, the COM server handle, `XSLTransform` |
| `MainForm.pas` / `.dfm` | the administration UI |
| `ServerCode.pas` | registers the COM server factory |
| `OrgChartISAPI.dpr`, `Webmodule.pas` / `.dfm` | the ISAPI extension |
| `OrgChart.htm` | the search page, and the JavaScript that writes the OCL |
| `LastPage.htm` | the confirmation page, served through a `TPageProducer` |
| `XSL\*.xslt` | two display and three edit stylesheets |
| `XMLExporter\*.pas` | the local exporter, not part of `Source\` |
| `Model\OrgChart.mdl` | the original Rose model; the authoritative copy is in `MainDataModule.dfm` |
| `ORGCHART.GDB`, `OrgChart.exe` | a populated database and a built executable, both checked in |
