# Association Class

An association in UML can carry data of its own. Bold implements that as an
association class: a real, persistent class whose instances are the links.

The model here has `Company`, `Employee` and `Job`. `Job` is the association
class of the Company-Employee association and carries `title` and `salary`.
A person can work for several companies, each job with its own salary, and
there is nowhere else for that salary to live.

## Running it

There is no `.dproj`, only `BE_AssocClass.dpr`, a VCL program for Windows. Open
it in the IDE and let Delphi create a project file; this demo is not part of
`examples\DelphiExamples.groupproj`.

It needs a database. Persistence is `TBoldPersistenceHandleDB` to
`TBoldDatabaseAdapterIB` to `TIBDatabase`, that is InterBase, with
`user_name=sysdba` / `password=masterkey` hard-coded in the form.
**`IBDatabase1.DatabaseName` is empty**, so fill it in before running. Then
press **Create DB**, which is a `TBoldIBDatabaseAction`, and **Open system**.

The Bold IBX units, `BoldDatabaseAdapterIB` and `BoldIBDatabaseAction`, live
under `Source\Deprecated\Persistance\IBX` and are in no current Bold package,
so put that folder on the unit search path.

## Navigating an association class

One association gives three different things to navigate, and the form shows
all three at once so you can compare them.

| Handle | Root | Expression | Yields |
| --- | --- | --- | --- |
| `AllPersons` | system | `Employee.allInstances` | the people |
| `PersonEmployers` | `AllPersons` | `employer` | the companies, jumping over the link |
| `PersonJobs` | `AllPersons` | `job` | the link objects themselves |
| `CompanyEmplyees` | `AllCompanies` | `employee` | the employees of the current company |
| `CompanyJobs` | `AllCompanies` | `job` | that company's jobs |

`employer` and `employee` skip the link and land on the far end. `job` stops on
the link. From a `Job` you can go either way again: `JobGrid` has columns
`employer.name` and `employee.name`.

## Creating a link by dragging

`PersonEmployersListBox` and `CompanyEmployeesListBox` have `DragMode` set to
`dmAutomatic`, and their hints say what that is for: *Drop a company here to
create a job* and *Drop a person here to create a job*. Dropping across creates
the `Job` instance. There is no code behind it; the list boxes are bound to
role handles and Bold knows a link object is what the role needs.

## Aggregates over the link

The grid columns are the short argument for modelling the salary on the link.
`AllPersonsGrid` and `CompanyGrid` both sum and average the same attribute from
opposite ends:

```
job.salary->Sum
job.Salary->Average->Round
employer->Size
employee->Size
```

## Files

| File | What it holds |
| --- | --- |
| `BE_AssocClass.dpr` | creates `TfrmMain` and runs |
| `MainForm.pas` / `.dfm` | every handle, grid and the embedded model |
| `AssociationClassExampleClasses*.pas/.inc` | generated business classes |
| `Employer.mdl` | Rational Rose model, linked by `BoldUMLRoseLink1` |
| `AssociationClassExampleClasses.mpb` | ModelMaker project bundle |

`BoldUMLRoseLink1.FileName` still points at `C:\vss\dev\BfD\examples\...`, the
path on the machine that built the demo in 2002. The model stored in the `.dfm`
is the one that is actually used.
