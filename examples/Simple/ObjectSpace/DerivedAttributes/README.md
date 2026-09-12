# Derived Attributes

A derived attribute has no column in the database. Bold computes it from an OCL
expression, and because the evaluation is subscribed, recomputes it whenever
anything the expression read changes. Marking an attribute `Derived=True` and
`persistence=Transient` is the whole of it.

The model here is two classes: `Product` with `prodName` and `net_Price`, and
`GlobalSettings` with a single `vat`.

## Running it

There is no `.dproj`. Open `BE_DerivedAttribDemo.dpr` in the IDE and let Delphi
create one.

It needs a database: InterBase through `TBoldDatabaseAdapterIB`, `sysdba` /
`masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it first. Then
**Create DB** and **Open system**, add a product or two with **Add New**, and
edit the VAT box. The label next to it says what to watch for: *Change this
value and see the derived attribute 'Retail Price' recalculate.*

## The derivation

`Product.retail_Price` is a Currency attribute tagged
`Derived=True,persistence=Transient` with

```
if global_VAT > 0 then net_price * (100 + global_VAT) / 100 else net_Price endif
```

`global_VAT` is not in the model. It is an OCL variable registered into the
system's evaluator when the system opens:

```pascal
AGlobalSetting := BoldSystemHandle1.System.ClassByExpressionName['GlobalSettings'][0]
                    as TGlobalSettings;
if Assigned(AGlobalSetting.M_vat) then
  BoldSystemHandle1.System.Evaluator.DefineVariable('global_VAT',
    AGlobalSetting.M_vat, AGlobalSetting.M_vat.BoldType, False);
```

The variable is bound to `M_vat`, the member itself, not to a copy of its
value. That is why editing `btxtVAT`, a `TBoldEdit` bound to
`blhGlobalSettings` and the expression `vat`, repaints every Retail Price cell
in `BoldGrid1` with no code in between. The grid column is simply
`retail_Price`; it does not know the VAT exists.

`BoldActivateSystemAction1SystemOpened` also guarantees the single
`GlobalSettings` instance exists, creating and saving one on a fresh database.

## The same problem solved differently

The `OclVariables` demo next door computes exactly the same retail price
without a derived attribute. It puts the arithmetic in the grid column and
supplies `global_VAT` from a `TBoldOclVariables` component in the DFM. Reading
the two side by side is the quickest way to see what a derived attribute buys:
the expression lives in the model, so every handle, grid, report and OCL query
in the application sees `retail_Price`, not only this one column.

## Files

| File | What it holds |
| --- | --- |
| `BE_DerivedAttribDemo.dpr` | creates `TForm1` and runs |
| `fDerivedAttrMain.pas` / `.dfm` | the handles, the grid, and the embedded model |
| `DerivedAttrExampleClasses*.pas/.inc` | generated `Product` and `GlobalSettings` |
