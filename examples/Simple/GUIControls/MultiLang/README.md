# Multi-Language Attributes

Bold can store one string per language in a single attribute. `TBAMLString`
holds a value per language; `TBAMLValueSet` is an enumerated attribute whose
captions are per-language; and which language is "current" is a setting on the
`TBoldSystem`, so changing it re-renders every bound control at once.

This demo models the languages themselves as ordinary persistent objects, which
is what makes the set of languages editable at runtime rather than compiled in.

## Running it

There is no `.dproj`, only `BE_MultiLang.dpr`. Open it in Delphi and let the IDE
create a project file. Persistence is IBX (`TBoldDatabaseAdapterIB`,
`TBoldIBDatabaseAction`, `TIBDatabase`), whose units live under
`Source\Deprecated\Persistance\IBX` and are in no current Bold package, so put
that folder on the unit search path.

`IBDatabase1` has credentials but **no `DatabaseName`** - set it, then press
**Create DB** and **Open system**. Opening the system seeds English, Swedish and
Norweigan, three person categories, two house kinds and a few people and
buildings, with Swedish translations written through
`M_Description.AsStringByLanguage['Swedish']`. Then switch the primary and
secondary language from the **Language** tab and watch the other two tabs change.

The three `MLTestAttributesD4/D5/D6.dpk` files are Delphi 4, 5 and 6 design-time
packages requiring `Bold30D4` and `Bold31D6`, neither of which exists here. They
are historical and are not needed to run the demo.

## Telling Bold how languages are modelled

`MainForm`'s initialization section is the configuration:

```pascal
BoldMLLanguageClassName := 'LanguageClass';
BoldMLLanguageNameAttributeName := 'LanguageName';
BoldMLLanguageNumberAttributeName := 'LanguageNumber';
```

`BoldMLAttributes` then builds its language list from that class, so the
language with `LanguageNumber = 0` is the default. The two buttons call
`BoldSetPrimaryLanguageByName` and `BoldSetSecondaryLanguageByName`, and
`brhPrimaryLanguage` / `brhSecondaryLanguage` are `TBoldReferenceHandle`s
pointing at the result, rendered by `BoldLabel1` and `BoldLabel2` with the
expression `'Primary Language: '+self.asstring`. `BoldTypeNameHandle1` maps the
model's attribute type names onto Delphi classes: `MLString` to `TBAMLString`,
`Language` to `TBALanguage`, and `HouseKind` / `PersonCategory` to the two
classes in `MLTestVSAttributes.pas`.

## Value sets whose values are rows

`MLTestVSAttributes.pas` is short and is the mechanism worth reading:

```pascal
_HouseKinds := TBAMLValueSetValueList.Create(
  TBoldSystem.DefaultSystem, 'HouseKindClass', 'IntValue',
  ['Description', 'ShortDescription']);
```

The legal values of the `HouseKind` attribute are the instances of the
persistent class `HouseKindClass`, keyed by its `IntValue`, with `Description`
and `ShortDescription` supplying the `brDefault` and `brShort` representations -
each of which is itself a `TBAMLString`. So a value set is editable data, in
every language, from the **Value Sets** tab.

The combo boxes on the first tab ask the *attribute type* for its values:

| Control | BoldListHandle expression | BoldProperties.Expression |
| --- | --- | --- |
| `bcbPersonCategory` | `PersonCategory.allInstances` | `personCategoryAttr` |
| `bcbMotherTounge` | `Language.allinstances` | `mothertounge` |
| `bcbHouseType` | `HouseKind.allInstances` | `houseTypeAttr` |

`allInstances` applied to a value-set attribute type returns its possible values
rather than objects. All three use `bdcsSetValue` with a matching
`BoldSetValueExpression`.

## Pinning one column to one language

`EnglishRenderer` is a `TBoldAsMLStringRenderer` with a single property set:

```
Language = 'English'
```

Three grid columns use it - `personCategoryAttr` in `grdPersons`, and
`description` in both `grdHouseKind` and `grdPersonCategory` - so those columns
stay English while everything beside them follows the current primary language.
That is the whole reason the renderer exists.

`grdBuildings` shows the other axis. Two of its columns share the expression
`houseTypeAttr`; the second adds `BoldProperties.Representation = 2`, which is
`brShort`, so it renders `ShortDescription` in the current language.

## A note on ordering

The three value-set list handles pair an ordered expression with a plain one,
for example `'HouseKindClass.allInstances->orderby(intvalue)'` as `Expression`
and `'HouseKindClass.allInstances'` as `MutableListExpression`. An `orderBy`
result is a derived list and cannot be inserted into; `MutableListExpression`
names the list the navigator should add to instead, which is what keeps the
insert button working on a sorted grid.
