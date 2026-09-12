# Document Versions

Bold's Object Versioning Extension stops the database overwriting an object
when it changes. Each save writes a new timestamped version and the old ones
stay, so an object has a history you can query and navigate: `atTime` takes a
timestamp and gives you the object as it was.

This demo is a document repository built on that. Authors edit the live
document; everyone else sees the last version the author published.

## Running it

**This demo needs a licence.** `FormCreate` opens with a message box saying so:
*This example needs a deployment key for Object Versioning Extension. If you
miss the key request it in the License Manager found in the About box.* Without
that key the versioning behaviour is not available, and the demo is worth
reading rather than running.

There is no `.dproj`. Open `DocVersions.dpr` in the IDE and let Delphi create
one. It needs InterBase through `TBoldDatabaseAdapterIB` on `dmMain`, `sysdba`
/ `masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it first.
Then **Create DB**, **Open system**, and create a user with the button, because
`TDocument.CompleteCreate` stamps the author from the current user and there
must be one.

## What is versioned

`Project`, `Document`, `Person` and `DocumentPart` all carry
`Bold.Versioned=True` in the model. That tag is what makes the persistence
mapper keep history instead of updating in place.

## Two views of the same document

The trick that makes the demo readable is that "which version you see" is
itself a derived attribute:

```
Document.ViewVersion   =  if author.context->isEmpty then lastPublished else timestampNow endif
Document.LastPublished =  version->select(isPublished).time->maxValue
```

`author.context->isEmpty` asks whether the document's author is the current
user. If not, the view version is the last published timestamp; if so, it is
now. The rule is stated once, in the model, and everything downstream inherits
it.

The derived role on `Project` then applies it:

```
Project.ViewContains = contains.atTime(viewVersion)->select(existing)
```

`contains` is the stored association of documents in a project;
`atTime(viewVersion)` rewinds each one to its own view version, and
`->select(existing)` drops the ones that had not been created yet.
`blhDocuments` navigates `viewContains`, `blhDocs` navigates the raw
`contains`, and comparing the two list boxes is the point of the layout.

## Publishing

A version is a bookmark into history, not a copy. `btnPublishClick` checks that
the current timestamp is not already recorded and then:

```pascal
NewVersion := doc.version.AddNew as TVersion;
NewVersion.Time := doc.BoldTime;
NewVersion.IsPublished := True;
```

`doc.BoldTime` is the timestamp of the document instance you are holding.

## Reading history out of the database

OCL cannot ask for "every version of this object", so two
`TBoldDerivedHandle`s go to the persistence layer directly with a
`TBoldChangePointCondition`:

| Handle | Condition |
| --- | --- |
| `bdhDocVersions` | one document id, one member id (`documentPart`), time 0 to `BOLDMAXTIMESTAMP` |
| `bdhPartVersions` | the ids of a list of `DocumentPart`s, same time range |

Both then call `dmMain.BoldSystemHandle1.system.GetAllWithCondition`, which
fills a `TBoldObjectList` with one object per change point. A change point is
a moment at which the named member actually changed, so the result is the
document's edit history rather than every timestamp in the database.

`blhDocVersions` merges the two and sorts:

```
self->union(partVersions.document)->orderby(boldTime)
```

which catches versions created by editing a part as well as versions of the
document itself. `grdOldVersions` renders each row with

```
title
self.boldTime.timestampToTime
version->select(time = self.boldTime)->first.name
```

so the third column is blank for an unpublished change point and named for a
published one.

## The current/history switch

`bvhDispOld` is a `TBoldVariableHandle` holding a Boolean, exposed to OCL as
the variable `dispOld` through `BoldOclVariables1`, alongside `currDoc`
(`blhDocuments`) and `oldDoc` (`blhDocVersions`). `behDisplayDoc` then reads

```
if dispOld then oldDoc else currDoc endif
```

with `RootTypeName = 'Document'`. Everything in the document panel, the parts
list, the memo, the header edit and the group box caption via
`BoldCaptionController1`, hangs off that one handle, so `Button1` flips the
whole panel between live and historical by toggling one boolean and retitling
itself **View history** / **View current**.

## Files

| File | What it holds |
| --- | --- |
| `DocVersions.dpr` | data module, then the main form |
| `MainDM.pas` / `.dfm` | system handle, persistence, and the versioned model |
| `MainForm.pas` / `.dfm` | the two derived handles, the variable switch, the UI |
| `DocumentClasses.inc` | `TDocument.CompleteCreate`, which stamps the author |
| `DocumentClasses*.pas/.inc` | generated classes |
| `Document.mdl` | Rational Rose model |
| `DocumentClasses.mpb` | ModelMaker project bundle |
