# Reverse-Derived Attributes

A derived attribute is normally read-only: it is computed, so there is nothing
to write to. Tagging it `Bold.ReverseDerive=True` and implementing
`_<name>_ReverseDerive` makes it writable. Bold hands you the new value and you
decide what to change so that the derivation would produce it.

That decision is the whole subject. A derivation rarely has an inverse, so a
reverse derivation is a policy you choose, not a function you invert. This demo
shows two of those policies and one attribute that deliberately has none.

## Running it

There is no `.dproj`. Open `BE_ReverseDerivedAttribDemo.dpr` in the IDE and let
Delphi create one.

It needs a database: InterBase through `TBoldDatabaseAdapterIB`, `sysdba` /
`masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it first. Then
**Create DB** and **Open system**, add a person, and type a full name into the
first grid column.

`fRDExamMain.dfm` is a binary form file that the repository's
`*.dfm text eol=crlf` rule inflates on checkout, 13366 bytes against 13306 in
the object store, so it will not stream as checked out.

Restoring it from `HEAD` is NOT enough for this particular file. It is one of
three binary form files in the repository that also lost a byte permanently when
line endings were normalised: the original import holds 13307 bytes, and the
normalising commit collapsed the one genuine `0D 0A` pair inside the binary
stream to `0A`. Recover it from the import instead:

```
git show e4dd87f:examples/Delphi/Simple/ObjectSpace/ReverseDerivedAttributes/fRDExamMain.dfm > fRDExamMain.dfm
```

The other affected files are `GUIControls/MultiLang/MainForm.dfm` and
`GUIControls/Renderers/fMain.dfm`. For every other binary form file in the
repository the object store is intact and `git show HEAD:<path> > <path>` is
correct.

## Person.fullName

Derived from a stored `firstName` and the family at the other end of a link:

```pascal
if assigned(family) then
  M_FullName.AsString := firstname + ' ' + family.familyName
else
  M_fullName.AsString := firstName;

M_FirstName.DefaultSubscribe(subscriber);
M_family.DefaultSubscribe(subscriber, breReSubscribe);
if assigned(family) then
  family.M_familyName.DefaultSubscribe(subscriber);
```

`breReSubscribe` on the link is what makes moving to a different family
re-derive rather than keep watching the old one.

Writing to it splits at the first space and then has to decide what the family
half means. `_fullName_ReverseDerive` works down a ladder:

1. the person is already in a family with that name, so keep it
2. some other family has that name, so join it (`TFamily.FindByName`)
3. the person's current family has exactly one member, so rename it
4. otherwise create a new family

and afterwards, if the old family is left with one member and is not the new
one, delete it. Rung 3 is the one that shows this is policy: renaming a
one-person family is a guess that the user meant to correct a typo rather than
to move.

`BoldGrid1` shows `fullName` and `firstName` as neighbouring columns, so you
can type in one and watch the other follow.

## Font.noValues

The small case, and the clearer one:

```pascal
procedure TFont._noValues_DeriveAndSubscribe(...);
begin
  SubscribeToAttributes(Subscriber);
  m_NoValues.AsBoolean := not (blink or underline or bold);
end;

procedure TFont._noValues_ReverseDerive(DerivedObject: TObject);
begin
  if NoValues then
    ResetAll;
end;
```

Ticking **No Values** clears `blink`, `bold` and `underline`. Unticking it does
nothing, because there is no answer to "not none" that the code could pick. The
`if` is the honest way to say so.

## Font.resultString, for contrast

The same class carries `resultString`, derived as the blink/bold/underline
words concatenated, and **not** reverse-derived. It is the same kind of
expression over the same three attributes, so the only difference is the
`Bold.ReverseDerive` tag and the missing method. `BoldLabel1` renders it as

```
'< ' + resultString + ' >'
```

## Files

| File | What it holds |
| --- | --- |
| `BE_ReverseDerivedAttribDemo.dpr` | creates `TForm1` and runs |
| `ReverseDeriveExampleClasses.inc` | both derivations, both reverse derivations, `FindByName` |
| `fRDExamMain.pas` / `.dfm` | the grids, check boxes and the embedded model |
| `ReverseDeriveExampleClasses*.pas/.inc` | generated `Person`, `Family`, `Font` |
