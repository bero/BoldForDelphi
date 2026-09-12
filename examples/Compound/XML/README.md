# Budget over XML

A spreadsheet-shaped Bold model served to a web browser as XML, rendered to
HTML by an XSL stylesheet, and edited back through an ordinary HTML form. The
round trip works because every cell in the XML carries its Bold id, so the
browser posts back a list of ids and values that Bold can apply directly.

The demo is two programs. One owns the Bold system and speaks XML over COM; the
other is a stateless ISAPI extension that knows nothing about Bold beyond how to
ask.

## The projects

| Project | Kind | What it does |
| --- | --- | --- |
| `XMLDispatcher.dpr` | VCL application, also a COM server | owns the budget model, the database, and a `TBoldXMLDispatcher` with two actions |
| `WebApplicationDll.dpr` | ISAPI library | receives HTTP, builds an XML request, calls the COM server, applies `Budget.xsl`, returns HTML |

`XMLDispatcher` is the only project in `examples\Compound` with a real `.dproj`
(Win32, Debug and Release).

## Running it

1. Build and run `XMLDispatcher.exe`. It registers itself as a COM server under
   the class id `{DCDA95FC-5719-4A65-B04E-89DE2DCC6DE5}`, name
   `XMLDispatcher.XMLDispatcher`, through `TBoldComServerConnectionFactory` in
   `ServerCode.pas`.
2. The database is Interbase. `BoldAppDataModUnit.dfm` names it
   `XMLDISPATCHER.GDB`, and `IBDatabase1BeforeConnect` rewrites that at runtime
   into `localhost:<exe directory>XMLDISPATCHER.GDB`. Press **Create DB**, then
   **Open system**. The IBX adapter is under `Source\Deprecated\Persistance\IBX`
   and is not in the maintained package set.
3. Create budgets, rows and columns from the popup menu on each grid, then press
   **Create cells** to fill in the grid of cells, and **Update DB**.
4. Type a budget name into the edit box and press the button next to it. The
   memo fills with the raw XML that the dispatcher would return, which is the
   fastest way to see what the producer emits without involving a web server.
5. For the browser half, deploy `WebApplicationDll.dll` to a web server and put
   `Budget.xsl` in the same directory as the DLL. Request
   `.../WebApplicationDll.dll/home?who=<budget name>`.

## The model

| Class | Members |
| --- | --- |
| `Budget` | `aName: String`, `aNumber: Integer` |
| `Row` | `aName: String`, `aNumber: Integer` |
| `Col` | `aName: String`, `aNumber: Integer` |
| `ACell` | `aName: String`, `aValue: String`, plus two derived integers |

`ACell.rownum` is derived as `row.aNumber` and `ACell.colnum` as `col.aNumber`.
Those two exist to serve the qualified role:

```
Budget.ACell[colnum, rownum]
```

which is how `AddRow` and `CreateCells` look a cell up by position rather than
by searching. A budget also has plain multi-roles `row` and `col`, and each cell
has single roles to its `Row`, its `Col` and its `Budget`.

`CompleteCreate` on `TBudget` and `TACell` seeds random names and values, so a
new budget is never empty.

## The producer: OCL in, DOM out

`TBoldXMLProducer.OnProduce` is where the XML is built, in
`dmXMLProducerForBudget.pas`. It is hand-written DOM construction, not a generic
export:

```pascal
Budget.EvaluateExpression('col->OrderBy(aNumber)', BoldElement);
```

then one `COL` element per column, the same again for `ROWS`, and for each row a
walk across the columns pulling `Budget.ACell[col.aNumber, row.aNumber]`.

The important call is this one:

```pascal
BudgetProducer.AddDomElementForBoldElement(ParentElement, Cell, 'CELL', 'CELL');
```

`AddDomElementForBoldElement` stamps the element with the element's Bold id,
taken from the attached `TBoldManipulator`. That id is what makes the round trip
possible.

## The manipulator: ids in, values out

`TBoldManipulator` maps between Bold elements and string ids, and between
element values and strings. This demo registers one mapper, named `CELL`:

```pascal
function ...Mappers0Get(Element: TBoldElement): String;
begin
  Result := Format('%s', [(Element as TACell).aValue]);
end;

procedure ...Mappers0Set(Element: TBoldElement; const NewValue: String);
begin
  (Element as TACell).aValue := NewValue;
end;
```

`IdStringRepresentation` is `isrVerbose`, so the ids in the XML are readable.

## The stylesheet closes the loop

`Budget.xsl` renders the `BUDGET` document as an HTML table inside a form, and
every `CELL` becomes a text input **named after its Bold id**:

```xml
<xsl:element name="input">
  <xsl:attribute name="type">text</xsl:attribute>
  <xsl:attribute name="name"><xsl:value-of select="@BoldID"/></xsl:attribute>
  <xsl:attribute name="value"><xsl:value-of select="."/></xsl:attribute>
</xsl:element>
```

So when the browser posts the form, the body is literally a list of Bold ids and
new values. The `UpdateBudget` action feeds that straight back:

```pascal
XMLRequest.SetIdentifiedValues(Request.ContentFields);
```

and on the server:

```pascal
for i := 0 to request.IdentifiedValues.Count - 1 do
  if Assigned(BoldManipulator1.ElementForIdString(...Names[i])) then
    BoldManipulator1.SetValue(...Names[i], ...Values[...]);
dmMain.BoldSystemHandle1.UpdateDatabase;
```

and then returns the freshly produced document, so the browser sees its own
edit.

## The two dispatcher actions

`TBoldXMLDispatcher` on `dmXmlProducer` is registered as the COM object
`BudgetDispatcher` under server class `XMLDispatcher`:

| Action | Handled by |
| --- | --- |
| `GetBudget` | `BudgetProducer`, the `TBoldXMLProducer` above |
| `UpdateBudget` | `MainDispatcherActions1Action`, the write-back shown above |

An action backed by a producer needs no code at all; the dispatcher calls the
producer and returns the document.

## The ISAPI side

`webModule.pas` holds no Bold system. It has a `TBoldComConnectionHandle` and a
`TBoldComClientObjectHandle` named `BudgetDispatcher`, and it turns an HTTP
request into an XML request:

| HTTP method | Action | Parameters |
| --- | --- | --- |
| POST | `UpdateBudget` | `BudgetName` from the query string, plus the whole content field list as identified values |
| GET, PUT, HEAD, anything else | `GetBudget` | `BudgetName` from the query string |

then calls the server through the SOAP-shaped interface
`IBoldSOAPService.Get(requestXml, out responseXml)`, loads `Budget.xsl` from the
DLL's own directory, applies it with MSXML's `transformNode`, and returns the
result as the response content.

## Things worth knowing before you rely on this

- `Budget.xsl` declares `xmlns:xsl="http://www.w3.org/TR/WD-xsl"`, the 1998
  working-draft XSL dialect, not XSLT 1.0. MSXML supports it only in legacy
  modes. The stylesheets in the sibling `XML2` demo use the real
  `http://www.w3.org/1999/XSL/Transform` namespace.
- The whole XML and COM layer is built on `MSXML_TLB` and Bold's COM object
  space, both Win32 and both dated.
- `TdmMain.GetElementById` and `GetIdForElement` in `BoldAppDataModUnit.pas`
  implement a second, hand-rolled id scheme of the form
  `<object id>:<member index>`. Nothing in the demo calls either of them; the
  manipulator does that job. `GetElementById` also creates its
  `TBoldDefaultId` twice, leaking the first one.
- `__history\` and `__recovery\` folders from the Delphi IDE are checked in
  alongside the source.

## Files

| File | What it holds |
| --- | --- |
| `XMLDispatcher.dpr` / `.dproj` | the server application |
| `BoldAppDataModUnit.pas` / `.dfm` | the model, the system and persistence handles, the IB database, the list handles and comparers |
| `BoldAppMain.pas` / `.dfm` | the administration form, `CreateCells`, and the raw-XML memo |
| `dmXMLProducerForBudget.pas` / `.dfm` | the dispatcher, the producer, the manipulator and the COM server handle |
| `ServerCode.pas` | registers the COM server factory |
| `WebApplicationDll.dpr`, `webModule.pas` / `.dfm` | the ISAPI extension |
| `Budget.xsl` | XML to HTML, and the reason the round trip works |
| `Budget.bld`, `Budget.mdl` | exported model and the original Rose model |
| `BudgetClasses.inc` | `CompleteCreate` seeding and three `CompareToAs` implementations |
