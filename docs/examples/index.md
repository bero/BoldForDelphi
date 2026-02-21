# Examples

Bold for Delphi includes several example applications demonstrating different features.

## Location

Examples are in the `examples/` folder:

```
examples/
├── Simple/
│   ├── ObjectSpace/
│   │   └── MasterDetail/          # Basic CRUD operations (has .dproj)
│   ├── Tools/
│   │   └── SaveAndGenerateTest/   # Model save & code generation (has .dproj)
│   └── LogBridge/                 # Logging integration
└── Compound/
    ├── Building/                  # Associations demo (legacy .dpr only)
```

## Simple Examples

### MasterDetail

**Location**: `examples/Simple/ObjectSpace/MasterDetail/`

Basic CRUD application demonstrating:

- Creating and editing objects
- List handles and grids
- Database persistence with SQLite (preconfigured)
- Navigator controls

**Note**: Requires the Bold design-time package to be installed first. See the [Quick Start Guide](../../Doc/quickstart.md) Step 2.

```
examples\Simple\ObjectSpace\MasterDetail\MasterDetail.dproj
```

### SaveAndGenerateTest

**Location**: `examples/Simple/Tools/SaveAndGenerateTest/`

Tests the Bold model save and code generation workflow. Useful for verifying your Bold installation works correctly.

```
examples\Simple\Tools\SaveAndGenerateTest\TestSaveAndGenerate.dproj
```

### LogBridge

**Location**: `examples/Simple/LogBridge/`

Demonstrates integrating Bold with external logging frameworks.

## Compound Examples

### Building (legacy)

**Location**: `examples/Compound/Building/`

**Note**: This is a legacy Delphi 7 example with only a `.dpr` file (no `.dproj`). Use it as a code reference for learning about associations and derived attributes, but it cannot be opened directly in modern Delphi IDEs without creating a new project.

Demonstrates:

- Multiple related classes
- Associations between objects
- Derived attributes
- OCL queries

### XML

**Location**: `examples/Compound/XML/`

XML dispatcher example demonstrating Bold's XML capabilities.

## Example Tutorials

More tutorials coming soon:

- Buildings & Owners - Associations between objects
- MasterDetail - Basic CRUD operations
- Constraints - Model validation
- Transactions - Transaction handling
- OCL Variables - Query parameters
- Renderers - Custom display
- TreeView - Hierarchical display

See also [Derived Attributes](../concepts/derived-attributes.md) in Core Concepts.

## PDF Tutorials

In-depth tutorials are available in PDF format in the `Doc/` folder:

| Tutorial | Description |
|----------|-------------|
| [Starting Bfd - Part 1](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Starting%20Bfd%20-%20Part%201%20-%20Introducing%20the%20Basics.pdf) | Introducing the Basics |
| [Starting Bfd - Part 2](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Starting%20Bfd%20-%20Part%202%20-%20Extending%20Models.pdf) | Extending Models |
| [Starting Bfd - Part 3](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Starting%20Bfd%20-%20Part%203%20-%20OCL.pdf) | OCL Queries |
| [Delphi and Model Driven Architecture](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Delphi%20and%20model%20Driven%20Architecture/Delphi%20and%20Model%20Driven%20Architecture.pdf) | Complete MDA guide |
| [ad970808_UML11_OCL.pdf](https://github.com/bero/BoldForDelphi/blob/develop/Doc/ad970808_UML11_OCL.pdf) | Official OCL Language Reference |
| [Creating custom Bold-aware components.pdf](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Creating%20custom%20Bold-aware%20components.pdf) | Building Bold-aware controls |

More PDFs: [Doc folder on GitHub](https://github.com/bero/BoldForDelphi/tree/develop/Doc)

## Running Examples

1. Open the example project in Delphi
2. Build the project (Shift+F9)
3. Configure database connection in the INI file
4. Run (F9)

### Database Configuration

Most examples use an INI file for database configuration. Edit the INI to match your database:

```ini
[Database]
Persistence=FireDAC
Type=SQLite

[SQLite]
Database=example.db
```

See [First Application](../getting-started/first-app.md) for database configuration examples.
