# Bold ComboBox

`TBoldComboBox` has two independent follower channels: `BoldHandle` with
`BoldProperties` renders the edit area, and `BoldListHandle` with
`BoldRowProperties` fills the drop-down. `BoldSelectChangeAction` decides what
picking a row actually does to the object.

This form puts four combo boxes side by side, each combining those three
settings differently, which is the shortest way to see what each one is for.

## Running it

There is no `.dproj`, only `BE_ComboBox.dpr`. Open it in Delphi and let the IDE
create a project file. The persistence chain is IBX
(`TBoldDatabaseAdapterIB`, `TBoldIBDatabaseAction`, `TIBDatabase`); those units
live under `Source\Deprecated\Persistance\IBX` and belong to no current Bold
package, so add that folder to the unit search path.

`IBDatabase1` has `user_name=sysdba` and `password=masterkey` but **no
`DatabaseName`** - fill it in first. Then press **Create DB** and **Open
system**. On `OnSystemOpened` the form reads `Food.txt` and `School.txt` from
the working directory and creates one object per line, so the drop-downs are not
empty. Press **Add** under Person to create people; `TPerson.CompleteCreate`
in `ComboBoxClasses.inc` gives each a random name.

## The four combo boxes

All four share `BoldHandle = blhPerson`, a `TBoldListHandle` on
`Person.allInstances`. What differs is where the list comes from and what
selection does.

| Control | BoldListHandle | BoldProperties.Expression | SelectChangeAction |
| --- | --- | --- | --- |
| `bcboPreferredFood` | `blhFood` | `name + ' likes ' + preferredfood.name` | `bdcsSetValue` |
| `bcboSchool` | `blhSchool` | `attendsSchool` | `bdscSetText` |
| `bcboMajorsIn` | `bchMajorTopic` | `major` | `bdscSetText` |
| `bcboFavouriteMusic` | `bchMusic` | `favouriteMusic` | `bdscSetText` |

### Setting a value versus setting text

`bcboPreferredFood` is the only one that changes a link. `Person.preferredFood`
is an association to `Food`, and the combo carries

```
BoldSetValueExpression = 'preferredFood'
BoldSelectChangeAction = bdcsSetValue
```

Under `bdcsSetValue` the control evaluates `BoldSetValueExpression` against the
current object to find *what to assign to*, then assigns the selected element to
it. Because that target is a `TBoldObjectReference`, the selected `Food` object
itself is stored, not its name. Its edit area meanwhile renders a whole
sentence, which shows that `BoldProperties.Expression` need not be the thing
being edited.

The other three use `bdscSetText`: the row's text is applied as a string. That
matches the model, because `Person.attendsSchool`, `major` and `favouriteMusic`
are all plain `String` attributes. `bcboSchool` still lists real `School`
objects through `BoldRowProperties.Expression = 'name'`, so you pick an object
and store its name.

`bcboSchool` also has `BoldSetValueExpression = 'attendsSchool'` set, but only
`bdcsSetValue` reads that property, so it has no effect here.

### Lists that are not in the database

`bcboMajorsIn` and `bcboFavouriteMusic` are fed by a pair of components rather
than a class extent:

```
bvhMajorTopic: TBoldVariableHandle
  ValueTypeName = 'Collection(String)'
  InitialValues = ('Psychology' 'History' 'Litterature'
                   'Business Economics' 'Computer Science')
bchMajorTopic: TBoldCursorHandle
  RootHandle = bvhMajorTopic
```

A `TBoldVariableHandle` holds a value that is typed by the model but stored
nowhere, and a `TBoldCursorHandle` gives it the current-element behaviour a
combo needs. `bvhMusic` and `bchMusic` do the same for Pop, Country, Rock,
Raggae and Rap.

## The rest of the form

`BoldListBox1`, `blstFood` and `blstSchool` are `TBoldListBox` controls with
`BoldRowProperties.Expression = 'name'` over `blhPerson`, `blhFood` and
`blhSchool`. The **Add** and **Delete** buttons next to Food and School call
`List.AddNew` and `Delete` on those handles, so you can watch the drop-downs
follow along without touching the combos.
