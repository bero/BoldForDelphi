# Simple Tree View

`TBoldTreeView` builds its tree from a list of *node descriptions*, each of
which says: for this type of element, what text and icon to show, and which OCL
expressions produce the children. Nothing walks the object graph in code.

Two trees on this form share one handle and produce two entirely different
shapes from the same objects, which is the clearest way to see that the shape
lives in the node descriptions rather than in the data.

## Running it

There is no `.dproj`, only `BE_SimpleTreeView.dpr`. Open it in Delphi and let the
IDE create a project file. Persistence is IBX (`TBoldDatabaseAdapterIB`,
`TBoldIBDatabaseAction`, `TIBDatabase`), whose units live under
`Source\Deprecated\Persistance\IBX` and are in no current Bold package, so put
that folder on the unit search path.

`IBDatabase1` has credentials but **no `DatabaseName`** - set it, then press
**Create DB** and **Open system**. Nothing seeds any data, so the trees start
empty: use the three navigators to add a company, then departments, then
employees, and both trees fill in as you type. **Save** calls `UpdateDatabase`.

There is no generated business-classes unit here at all. The model lives inside
`SimpleTreeViewForm.dfm` on `BoldModel1` and every object is a plain
`TBoldObject`, which is why the `.pas` file is barely a hundred lines.

## The model and the handle chain

`Company` and `Department` each have a `Name`; `Employee` has `FirstName` and
`LastName`. `Company.consistsOf` leads to `Department`, `Department.employs` to
`Employee`. The three grids form a master-detail chain, each handle rooted on
the one above:

| Handle | RootHandle | Expression |
| --- | --- | --- |
| `blhCompanyList` | `BoldSystemHandle1` | `Company.allInstances` |
| `blhDepartmentList` | `blhCompanyList` | `consistsOf` |
| `blhEmployeeList` | `blhDepartmentList` | `employs` |

## Tree one: follow the associations

`btvCompanyDepartments` has `BoldHandle = blhCompanyList` and one part:

```
ElementExpression    = 'consistsOf'
ControllerExpression = 'self.oclType'
InterpretAsList      = True
```

`ControllerExpression = 'self.oclType'` means "look up the node description
whose `ContextTypeName` matches this element's class". So each department found
by `consistsOf` is rendered by the `Department` description, whose own part is
`employs`, whose elements land on the `Employee` description. Two descriptions
and three expressions produce the whole tree.

`Employee` has an empty `ListController.Parts`, which is how a leaf is declared,
and its text is an expression rather than an attribute:
`lastName + ' ' + firstName`.

## Tree two: nodes that are not objects

`btvCompanyEmployees` follows the **same** handle but has three parts, and none
of them names an element expression:

```
ControllerExpression = '''A - H'''
ControllerExpression = '''I - P'''
ControllerExpression = '''Q - Z'''
```

A `ControllerExpression` that evaluates to a string selects a node description
*by name* instead of by type. So every company gets three child nodes whose
whole existence is a caption, each with a constant `TextController.Expression`
and its own filter:

```
consistsOf.employs->select(lastName.subString(1, 1).toUpper >= 'A')
                  ->select(lastName.subString(1, 1).toUpper < 'I')
                  ->orderBy(lastName)
```

That expression reaches across two associations, so the grouping nodes flatten
the department level away entirely. OCL `subString` counts from 1, so
`subString(1, 1)` is the first character. The `Q - Z` variant has only a lower
bound.

`HideNodeWithNoChildren` is `False` on all of them, so an empty letter range
still shows.

## Files

| File | What it holds |
| --- | --- |
| `SimpleTreeViewForm.dfm` | the model, both trees, the grids and the IBX chain |
| `SimpleTreeViewForm.pas` | a Save button and a hard-coded form size |
| `SimpleTreeView.mdl` | the Rational Rose model the DFM copy came from |

`BoldUMLRoseLink1.FileName` still points at
`D:\bold\bfd\examples\Delphi\Simple\GUIControls\SimpleTreeView\SimpleTreeView.mdl`,
an absolute path from the original author's machine. The link is only needed to
re-import the `.mdl`; the model the program runs on is the copy in the DFM.
