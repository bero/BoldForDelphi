# Derived Relations

Associations can be derived as well as attributes. A derived role is computed
by OCL over other roles, has no rows in a link table, and updates itself when
what it reads changes. Marking the association `Derived=True` with
`persistence=Transient` and giving each navigable role a `Bold.DerivationOCL`
is all there is to it.

The model is projects and people, and it stacks two derived associations on top
of two stored ones, so you can watch a derivation feed a derivation.

## Running it

There is no `.dproj`. Open `BE_DerivedRelations.dpr` in the IDE and let Delphi
create one.

It needs a database: InterBase through `TBoldDatabaseAdapterIB`, `sysdba` /
`masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it first. Then
**Create DB** and **Open system**, add a few projects and people, and drag
people onto **Participators**. Names are random; both classes override
`CompleteCreate` to pick from a fixed list.

`fDerivedRelationsMain.dfm` is a binary form file that the repository's
`*.dfm text eol=crlf` rule inflates on checkout (14165 bytes against 14095 in
the object store), so it will not stream until you restore it with
`git show HEAD:<path> > <path>`.

## What is stored and what is computed

Stored:

| Association | Roles |
| --- | --- |
| `ProjectParticipators` (an association class) | `Project.participator` / `Person.participatesIn` |
| `ProjectProjectLeader` | `Project.projectLeader` (0..1) / `Person.leadsProject` |

Derived:

```
Project.allMembers  =  participator->union(projectLeader)
Person.memberOf     =  participatesIn->union(leadsProject)
Person.associates   =  memberof.allmembers->excluding(self)
```

`ProjectAllMembers` derives both ends, each with its own expression, which is
what lets you navigate it in either direction. `->union` over a single-valued
role and a multi-valued one is the idiom for "the team, leader included".

`Associates` derives only one end. Its opposite role, `origin`, is marked
non-navigable in the model, so no reverse expression is needed and none is
generated: `TPerson` has an `associates` property and there is nothing named
`origin` anywhere in `DerivedHandleExampleClasses_Interface.inc`.

`associates` is the payoff. It reads `memberOf`, which is itself derived, and
`allMembers`, which is derived too, so it sits two derivation levels above the
stored data and still updates the moment you drop a person onto a project.
`->excluding(self)` keeps you out of your own colleague list.

## The handles

| Handle | Root | Expression |
| --- | --- | --- |
| `blhProject` | system | `Project.allInstances` |
| `blhPerson` | system | `Person.allInstances` |
| `blhParticipators` | `blhProject` | `participator` |
| `blhAllMembers` | `blhProject` | `allMembers` |
| `behProjectLeader` | `blhProject` | `projectLeader` |
| `blhAssociates` | `blhPerson` | `associates` |

Nothing in the form distinguishes a derived role from a stored one.
`blhAllMembers` and `blhParticipators` are the same component type with the
same kind of expression, and the list boxes over them are identical. That is
the point: the difference lives in the model.

`behProjectLeader` is a `TBoldExpressionHandle` rather than a list handle
because `projectLeader` is single-valued, and `BoldEdit1` renders its `name`.

The three list boxes are `dmAutomatic`, so linking is done by dragging.

## Files

| File | What it holds |
| --- | --- |
| `BE_DerivedRelations.dpr` | creates `TForm1` and runs |
| `fDerivedRelationsMain.pas` / `.dfm` | the handles and the embedded model |
| `DerivedHandleExampleClasses.inc` | `CompleteCreate` for both classes, random names |
| `DerivedHandleExampleClasses*.pas/.inc` | generated `Project`, `Person`, `ProjectParticipators` |

The generated unit shares its name with the one in the `DerivedHandle` demo but
holds a different model. They are not interchangeable.
