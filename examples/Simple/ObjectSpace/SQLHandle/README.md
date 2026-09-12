# SQL Handle

`TBoldSQLHandle` fetches a list of objects by handing a WHERE clause and an
ORDER BY clause straight to the database, in the database's own SQL against
Bold's generated tables. It is the deliberate way out of OCL, for when you want
the server to do the selecting.

This demo puts the whole `Person` extent in one grid and an SQL-selected subset
in another, with the clause assembled from combo boxes so you can see what goes
in and what comes back.

## Running it

There is no `.dproj`. Open `BE_SQLHandle.dpr` in the IDE and let Delphi create
one.

It needs a database: InterBase through `TBoldDatabaseAdapterIB`, `sysdba` /
`masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it first. Then
**Create DB** and **Open system**; ten people, Adam to Jacob with assets 1 to
10 and last names J down to A, are created on first open. Press **Update DB**,
then **Execute SQL**.

`Mainform.dfm` is a binary form file that the repository's
`*.dfm text eol=crlf` rule inflates on checkout (12826 bytes against 12778 in
the object store), so it will not stream until you restore it with
`git show HEAD:<path> > <path>`.

## The handle

```
bsqlhPersons : TBoldSQLHandle
  StaticSystemHandle  = BoldSystemHandle1
  ClassExpressionName = 'Person'

blhSelectedPersons : TBoldListHandle
  RootHandle = bsqlhPersons
```

`ClassExpressionName` tells the handle which class's table to query and which
class to instantiate from the ids that come back. A plain `TBoldListHandle`
rooted on it turns the result into something `grdSQLResult` can show, so
everything downstream is ordinary Bold.

`ExecuteSQL` is explicit. Unlike a list handle over an OCL expression, this one
does not re-run itself when the objects change; the button is the trigger:

```pascal
bsqlhPersons.SQLOrderByClause := OrderByClause;
bsqlhPersons.SQLWhereClause := WhereClause;
bsqlhPersons.ExecuteSQL;
```

## What you are writing

`btExecSQLClick` formats the clause from three combo boxes offering
`=`, `<>`, `>`, `<`, `>=`, `<=` and three edits, and joins the parts with
` and`:

```pascal
WhereClause := Format('Assets %s %s', [cmbAssetsOperator.Text, edAssetsExpr.Text]);
...
WhereClause := WhereClause + Format('%s Firstname %s ''%s''', [Conjunction, ...]);
```

`Assets`, `Firstname` and `Lastname` here are **column names in the generated
table**, not OCL attribute names. They happen to match the attribute names in
this model, which is convenient and slightly misleading: rename an attribute in
the model and the SQL keeps working only because the column keeps its old name,
or stops working because it does not. That is the trade. The order-by combos
offer the same three names.

## The warning it repeats twice

There is a label reading *Do not forget to update database before executing
SQL*, and the handler opens with

```pascal
if BoldSystemHandle1.System.BoldDirty then
  ShowMessage('There are changes that are not saved to the database. Results may not be as expected');
```

The query runs in the database. Objects you created but have not saved are not
there to be found, objects you deleted but have not saved still match, and an
edited value is matched at its old value. OCL evaluated in memory has none of
these problems, which is the other half of the trade.

## The model

`Person` with `Assets`, `FirstName` and `LastName` (`Bold.Length=25`),
`Building` with `Address` and `TotalRent`, `Ownership` as an association class
between them, and a `Residence` association. Only `Person` is queried.

## Files

| File | What it holds |
| --- | --- |
| `BE_SQLHandle.dpr` | creates `TForm1` and runs |
| `Mainform.pas` / `.dfm` | the SQL handle, the clause builder, the embedded model |
| `SQLHandleClasses*.pas/.inc` | generated `Person`, `Building`, `Ownership` |
