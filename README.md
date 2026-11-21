# Bold for Delphi

[![FPC tests](https://github.com/bero/BoldForDelphi/actions/workflows/fpc-tests.yml/badge.svg?branch=develop)](https://github.com/bero/BoldForDelphi/actions/workflows/fpc-tests.yml)
[![FPC tests (Windows)](https://github.com/bero/BoldForDelphi/actions/workflows/fpc-tests-windows.yml/badge.svg?branch=develop)](https://github.com/bero/BoldForDelphi/actions/workflows/fpc-tests-windows.yml)
[![codecov](https://codecov.io/gh/bero/BoldForDelphi/branch/develop/graph/badge.svg)](https://codecov.io/gh/bero/BoldForDelphi)

<!-- --8<-- [start:intro] -->
Bold is a model-driven application framework and object-relational mapper for Object Pascal. You describe your
business domain once, as a UML class model. Bold generates the business classes from it, keeps the objects in an
in-memory object space, creates and evolves the database schema, and keeps every user interface that shows an
object up to date when that object changes. Bold works with **Delphi** and with **Lazarus / Free Pascal**, on
**Windows** and **Linux**.

Bold was created by Boldsoft (release 4.0, 2004) and open-sourced by Embarcadero in 2020 under the MIT license. This
repository is its community-maintained continuation.
<!-- --8<-- [end:intro] -->

## What Bold is used for

<!-- --8<-- [start:use] -->
Bold pays off in line-of-business applications with a rich domain: ERP-style systems, logistics and transport,
planning, finance, inventory, CRM. These are applications with dozens or hundreds of interrelated classes and many
screens that show the same data from different angles. Attracs, a transport management system with more than 200
classes, has run on Bold for two decades.

Your code works with `Customer.Invoices` and `Invoice.GrossTotal`, not with SQL, `TDataSet` code or field names. The
domain is written once, as a model, instead of three times: as tables, as datasets and as forms.

Bold is more machinery than you need for a small CRUD tool with a handful of tables, or for reporting over an
existing database that you do not own: Bold works best when it creates and owns the database schema.
<!-- --8<-- [end:use] -->

## Most useful features

<!-- --8<-- [start:features] -->
- **Model first, code generated.** Classes, attributes, associations, inheritance and constraints live in a UML
  model, edited in the Delphi IDE or in the standalone model editor. Bold generates typed Object Pascal classes
  from it, and the model is also available at run time.
- **Object-relational mapping with real navigation.** `Invoice.Customer.BillingAddress.City` just works. Objects
  are fetched lazily and cached in the object space. `UpdateDatabase` writes every changed object in one database
  transaction. Both ends of an association are kept consistent automatically.
- **OCL, a query language over objects.** `Customer.allInstances->select(invoices->size > 5)` is evaluated in
  memory or translated to SQL. The same expressions drive lists in the user interface, derived attributes,
  constraints and reports.
- **Derived attributes and associations that stay current.** A derived value is defined in OCL or in code. Bold
  subscribes to the values it depends on and recalculates it only when one of them changes.
- **Live user interface binding.** Controls bind to handles that hold an object, a list or an OCL expression. When
  an object changes anywhere in the application, every control that shows it refreshes, with no refresh code.
  On Windows there are Bold-aware VCL controls; FireMonkey (LiveBindings) and LCL (data-aware controls) bind through
  `TBoldDataSet`.
- **Schema creation and evolution.** Bold creates the database from the model, and generates the changes when the
  model changes.
- **Built in:** undo and redo, object-space transactions, optimistic and pessimistic locking, object versioning,
  and change propagation between clients.
- **Several databases.** SQLite, SQL Server, PostgreSQL, Firebird, Oracle and others through FireDAC, UniDAC or
  Zeos; XML file persistence for small programs and tests.
<!-- --8<-- [end:features] -->

## A short example

The business classes below are generated from the model of the Invoicing example
([`examples/Compound/Invoicing`](examples/Compound/Invoicing/Model/Invoicing.bld)), which has an FMX client, an LCL
client and console programs.

```pascal
var
  Customer: TCompany;
  Invoice: TInvoice;
  Balance: Currency;
  InvoiceCount: Integer;
begin
  Customer := TCompany.Create(BoldSystem);    // a new object in the object space
  Customer.Name := 'Acme Ltd';
  Invoice := TInvoice.Create(BoldSystem);
  Invoice.Customer := Customer;               // Customer.Invoices now holds the invoice too
  BoldSystem.UpdateDatabase;                  // all changes, in one database transaction

  // OpenBalance is derived in the model as invoices->select(isPosted).balance->sum,
  // and is recalculated when an invoice, line or payment it depends on changes.
  Balance := Customer.OpenBalance;
  InvoiceCount := Customer.EvaluateExpressionAsInteger('invoices->size');  // OCL at run time
end;
```

## Platforms

<!-- --8<-- [start:platforms] -->
| Toolchain | Versions | User interface | Windows | Linux |
|-----------|----------|----------------|---------|-------|
| Delphi | 11.3 Alexandria, 12.3 Athens, 13.1 Florence | VCL | Win64 | — |
| Delphi | same | FMX, console | Win64 | Linux64 (FMX through FmxLinux) |
| Lazarus / Free Pascal | Lazarus 4, Free Pascal 3.2.2 or later | LCL, console | 64-bit | 64-bit |

Programs (the examples, the tools and the test programs) are built and tested as 64-bit programs. Win32 still
compiles, but is no longer built or tested. The Delphi design-time package (`dclBold`) is built for the 32-bit IDE,
and for the 64-bit IDE of Delphi 12.3 and 13. macOS is planned
([#127](https://github.com/bero/BoldForDelphi/issues/127)).
<!-- --8<-- [end:platforms] -->

[Installation](docs/getting-started/installation.md) has the details per toolchain.

## Getting started

Install the Delphi design-time package with a package manager:

- [TMS Smart Setup](https://doc.tmssoftware.com/smartsetup/download): `tms install bero.boldfordelphi`
- [DPM](https://github.com/DelphiPackageManager/DPM/releases): `dpm install bero.BoldForDelphi <project>.dproj`

[Installation](docs/getting-started/installation.md) covers these, the manual route and the Lazarus packages. Then:

1. **[Quick Start](docs/quickstart.md)**: run a ready-made example in a few minutes.
2. **[Tutorial](examples/Tutorial/README.md)**: build an invoicing application in five steps, from an empty project
   to associations, OCL and inheritance (Delphi, VCL).
3. **[Examples](docs/examples/index.md)**: a catalogue of more than 40 example programs, one feature each.

## Documentation

The documentation site is **https://bero.github.io/BoldForDelphi/**. It holds:

- [Core concepts](docs/concepts/index.md): object space, OCL, derived attributes, persistence, subscriptions, logging
- [Class guides](docs/classes/index.md) for the classes you use most, and the
  [class reference](docs/reference/index.md) converted from the original Bold 4.0 help
- [FAQ](docs/faq.md), [Roadmap](ROADMAP.md) and [Changelog](CHANGELOG.md)
- [Development](docs/development/index.md): source layout, building, tests and code coverage

## Databases

| Toolchain | Adapter | Notes |
|-----------|---------|-------|
| Delphi | FireDAC | Recommended. Included with Delphi |
| Delphi | UniDAC | Optional. Needs a [UniDAC](https://www.devart.com/unidac/) license; set the `UniDAC` environment variable |
| Free Pascal | Zeos 8.0 | [ZeosLib](https://sourceforge.net/projects/zeoslib/), free; package `BoldZeos.lpk` |
| Both | XML file | No database server; `TBoldPersistenceHandleFileXML` |

The test suite runs on SQLite, SQL Server and PostgreSQL. Bold itself has **no runtime dependencies** beyond the
Delphi or Free Pascal runtime library and the database access library you choose.
[Persistence](docs/concepts/persistence.md) describes the adapters and the SQL dialect setting for each database.

## Tests and code coverage

Tests are in `UnitTest/`. Delphi runs them with DUnitX; Free Pascal runs the same test units with FPCUnit, and
GitHub Actions runs the Free Pascal suite on Linux and Windows on every push. Code coverage is tracked on
[Codecov.io](https://codecov.io/gh/bero/BoldForDelphi).

**Current Statistics** (as of 2026-11-14):
- **Tests**: 2405 total (2405 passing, 0 ignored, 0 failed)
- **Code Coverage**: 62.3% (39,328 / 63,143 lines covered, measured 2026-11-14)

[UnitTest/README.md](UnitTest/README.md) says how to set up, build and run the tests, on Delphi and Free Pascal,
against several database engines and with code coverage.

## Contributing

Bug reports, pull requests, documentation, examples and reports of how Bold works with your database, Delphi or
Lazarus version are all welcome. [Contributing](docs/contributing.md) describes the workflow. Plans and open items are in
the [Roadmap](ROADMAP.md).

## Versioning

Releases are tagged `Year.Month.build`, unpadded and without a prefix (for example `26.9.1`). The tag is the release
identity: TMS Smart Setup and DPM install from it. Each release is described in the [Changelog](CHANGELOG.md);
[`packaging/release-checklist.md`](packaging/release-checklist.md) is the release procedure.

## Community and resources

- **Documentation**: https://bero.github.io/BoldForDelphi/
- **Issues**: https://github.com/bero/BoldForDelphi/issues
- **Discord**: https://discord.gg/C6frzsn
- **Blog**: http://boldfordelphi.blogspot.com/
- **Wiki**: https://delphi.fandom.com/wiki/Bold_for_Delphi and the [GitHub wiki](https://github.com/bero/BoldForDelphi/wiki)
- **Original PDF guides** from Boldsoft: the [`Doc`](Doc) folder

## License

MIT License. The source code was made available by Embarcadero. The original Boldsoft release is version 4.0.1.0
(Bold for Delphi 4.0, 2004-04-23).
