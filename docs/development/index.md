# Development

This section covers development topics for Bold for Delphi contributors.

## Topics

| Topic | Description |
|-------|-------------|
| [Unit Testing](testing.md) | Running and writing unit tests |
| [Codecov Setup](codecov.md) | Code coverage tracking |

## Source Organization

```
Source/
├── BoldAwareGUI/          # GUI components and control packs
│   ├── BoldControls/      # Visual controls (Grid, Edit, ComboBox)
│   ├── ControlPacks/      # Renderer/follower pattern for UI binding
│   └── Core/              # GUI base functionality
├── Common/                # Shared infrastructure
│   ├── Core/              # Base classes (BoldBase, BoldContainers)
│   ├── Subscription/      # Observer pattern (BoldSubscription)
│   ├── Support/           # Utilities (BoldUtils, BoldGuard)
│   └── Logging/           # Log handling
├── ObjectSpace/           # Object model runtime
│   ├── BORepresentation/  # Business object representation
│   ├── Core/              # BoldElements, BoldSystem
│   ├── Ocl/               # OCL parser and evaluator
│   ├── RTModel/           # Runtime type information
│   └── Undo/              # Undo/redo mechanism
├── Persistence/           # Database persistence
│   ├── Core/              # Persistence controllers
│   ├── DB/                # Database persistence base
│   └── FireDAC/           # FireDAC adapter
├── PMapper/               # Object-relational mapping
│   ├── SQL/               # SQL generation
│   ├── Default/           # Default mappers
│   └── DBEvolutor/        # Database schema evolution
├── Handles/               # Handle system for UI binding
├── MoldModel/             # Model representation and code generation
├── UMLModel/              # UML metamodel and editor
└── ValueSpace/            # Value interfaces and streaming
```

## Building Packages

### Using PowerShell

```powershell
# Build for Delphi 12.3
DelphiBuildDPROJ.ps1 -Projectfile "packages\Delphi29.3\dclBold.dproj" -VerboseOutPut
```

### Supported Delphi Versions

| Delphi Version | Package Folder |
|----------------|----------------|
| Delphi 11.3 | `packages/Delphi28/` |
| Delphi 12.1 | `packages/Delphi29.1/` |
| Delphi 12.3 | `packages/Delphi29.3/` |
| Delphi 13 | `packages/Delphi30/` |

## Compiler Directives

Key defines in `Bold.inc`:

```pascal
{$DEFINE SpanFetch}           // Efficient batch fetching
{$DEFINE IDServer}            // External ID server
{$DEFINE CompareToOldValues}  // Skip unchanged values in updates
{$DEFINE BoldJson}            // JSON serialization support
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Write tests for new functionality
4. Submit a pull request

See [Contributing](../contributing.md) for documentation contribution guidelines.
