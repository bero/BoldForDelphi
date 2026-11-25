# Development

This section is for people who work on Bold itself: its source layout, how to build it, and how it is tested.

| Topic | Description |
|-------|-------------|
| [Unit Testing](testing.md) | Writing tests that run under Delphi and Free Pascal |
| [Codecov Setup](codecov.md) | Uploading code coverage to Codecov.io |
| [Parallel SQL Execution](parallel-sql-execution.md) | A design proposal, not implemented: running persistence SQL in parallel |
| [Contributing](../contributing.md) | Pull requests, documentation and the style guide |

How to set up, build and run the test suites (DUnitX and Delphi-Mocks, Zeos for Free Pascal, the database engine
matrix, memory checks and code coverage) is described next to the scripts, in
[UnitTest/README.md](https://github.com/bero/BoldForDelphi/blob/develop/UnitTest/README.md).

## Source organization

The main folders under `Source/`:

```
Source/
├── BoldAwareGUI/          # Bold-aware VCL controls and their control packs
│   ├── BoldControls/      # TBoldGrid, TBoldEdit, TBoldComboBox, the other controls and TBoldDataSet
│   ├── BoldDevex/         # DevExpress integration (needs a DevExpress license; unmaintained)
│   ├── ControlPacks/      # Followers and renderers that bind a control to an element
│   └── Core/              # GUI base functionality (BoldGUI)
├── Common/                # Shared infrastructure
│   ├── Core/              # Base classes (BoldBase, BoldContainers, BoldDefs)
│   ├── Environment/       # BoldEnvironmentVCL, the VCL's hooks into Bold
│   ├── EnvironmentLCL/    # BoldEnvironmentLCL, the same for the LCL
│   ├── Include/           # bold.inc
│   ├── Logging/           # Logging
│   ├── Subscription/      # The subscription mechanism (BoldSubscription, BoldDeriver)
│   ├── Support/           # Utilities (BoldUtils, BoldGuard, BoldIndex)
│   └── IDE/, IDELazarus/  # Design-time support for Delphi and Lazarus
├── Handles/               # Handles: system, list, expression, actions, unloader
├── MoldModel/             # The model as the code generator sees it, and the code generator
├── ObjectSpace/           # The object space at run time
│   ├── BORepresentation/  # TBoldSystem, TBoldObject, the members and attribute types
│   ├── Core/              # BoldElements and the core constants
│   ├── Ocl/               # OCL parser and evaluator
│   ├── RTModel/           # Run-time type information
│   └── Undo/              # Undo and redo
├── Persistence/           # Persistence handles and database adapters
│   ├── Core/              # Persistence controllers and handles
│   ├── DB/                # Database persistence base, IBoldDatabase interfaces
│   ├── File/              # XML file persistence
│   ├── FireDAC/           # FireDAC adapter (Delphi, recommended)
│   ├── UniDAC/            # UniDAC adapter (Delphi, optional)
│   ├── Zeos/              # Zeos adapter (Free Pascal)
│   └── Propagation/       # Change propagation between clients
├── PMapper/               # Object-relational mapping
│   ├── SQL/               # SQL generation
│   ├── Default/           # Default mappers
│   └── DBEvolutor/        # Database schema evolution
├── UMLModel/              # UML metamodel, the model editor and its plugins
└── ValueSpace/            # Value interfaces and XML streaming
```

`Source/Deprecated/` holds database adapters that are no longer maintained (ADO, BDE, DBExpress and others); ignore
it. COM, SOAP and the propagator are Windows only.

## Building the packages

| Toolchain | Package | Notes |
|-----------|---------|-------|
| Delphi 11.3 | `packages/Delphi11.3/dclBold.dproj` | Win32 only |
| Delphi 12.3 | `packages/Delphi12.3/dclBold.dproj` | Win32 and Win64 |
| Delphi 13 | `packages/Delphi13/dclBold.dproj` | Win32 and Win64 |
| Lazarus | `packages/Lazarus/*.lpk` | `Bold`, `BoldLCL`, `BoldZeos`, and the design-time `dclBold`, `dclBoldZeos` |

`dclBold` is a single design-time package that contains the runtime units too. The Delphi packages build in the IDE
or with MSBuild; [Installation](../getting-started/installation.md) has the steps for both toolchains, and
[packages/Lazarus/README.md](https://github.com/bero/BoldForDelphi/blob/develop/packages/Lazarus/README.md)
describes the Lazarus packages.

## Compiler directives

`Source/Common/Include/bold.inc` is included by every unit. The defines that matter most:

| Define | Effect |
|--------|--------|
| `BOLD_VCL` | Compiles the VCL parts of the control packs and of `BoldGUI`. `bold.inc` sets it for Delphi on Windows unless the project defines `BOLD_NO_VCL`, as FMX projects do |
| `BOLD_XMLDOM` | Delphi XML streaming on `Xml.xmldom` (OmniXML) instead of MSXML. Set by `bold.inc` off Windows; define it to test that backend on Windows |
| `BoldJson` | JSON support (always on) |
| `BOLD_NO_QUERIES` | Turns off the query mechanism (always on) |
| `Attracs` | Defined by the Attracs application, not by this repository. Without it, `bold.inc` turns off `SpanFetch`, `CompareToOldValues`, `IDServer`, `NoNegativeDates` and `ConvertZeroDateToDateNil`, and turns on `NoMayUpdate` |
| `NoAutoSubscription` | Not defined by default. Turns AutoSubscription off, so a code derivation must subscribe to the members it reads with `DefaultSubscribe` itself; see [Derived Attributes](../concepts/derived-attributes.md#code-derivation) |
