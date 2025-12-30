# BoldForDelphi

Bold is a Model Driven Architecture (MDA) framework and Object-Relational Mapping (ORM) tool for Delphi. It allows you to start with a UML model of your application and execute it with a sophisticated object-relational mapping layer, OCL query language, change synchronization, and IDE-integrated tools.

## Use Cases

Bold for Delphi is best suited for:

- **Complex Domain Models** - Applications with many interrelated business entities, inheritance hierarchies, and associations
- **Data-Intensive Business Applications** - ERP, logistics, CRM, inventory management, financial software
- **Rich Query Requirements** - OCL enables expressive queries like `orders->select(status = 'Pending' and total > 1000)`
- **Long-Lived Desktop Applications** - Automatic UI updates when data changes via the subscription system
- **Rapid Prototyping** - Define your model in UML, generate code, and have a working database-backed application quickly

## Getting Started

- **[Quick Start Guide](Doc/quickstart.md)** - Step-by-step instructions to install Bold, build your first model, and run the MasterDetail example application.
- **[Roadmap](ROADMAP.md)** - Development plans, supported features, and future directions for Bold for Delphi.
- **[Changelog](CHANGELOG.md)** - Version history and release notes.

## Supported Delphi Versions

- Delphi 13.0 Athens (packages/Delphi30/)
- Delphi 12.3 Athens (packages/Delphi29.3/)
- Delphi 12.1 Athens (packages/Delphi29.1/)
- Delphi 11.x Alexandria (packages/Delphi28/)

## Building

### Prerequisites
- Delphi 13.0, 12.3, 12.1 CE or 11.3 installed

### Build the Package

Or manually from PowerShell:
by using https://github.com/omonien/DelphiStandards/blob/master/DelphiBuildDPROJ.ps1

Adjust -DelphiVersion from what you use.

```powershell
`DelphiBuildDPROJ.ps1 -Projectfile "C:\BoldForDelphi\packages\Delphi30\dclBold30.dproj" -DelphiVersion "37.0" -VerboseOutPut`
```

## Unit Tests and Code Coverage

Unit tests are located in `UnitTest/` and use the DUnitX framework.

### Running Tests
```batch
cd UnitTest
build.bat
UnitTest.exe --exit:Continue
```

### Code Coverage

Code coverage reports can be generated using DelphiCodeCoverage. See `UnitTest/UnitTesting.md` for setup instructions.

```batch
cd UnitTest
run_coverage.bat
```

Results are generated in `UnitTest/coverage_report/`. Open `CodeCoverage_summary.html` to view the report.

## Source Organization

```
Source/
├── BoldAwareGUI/          # GUI components and control packs
│   ├── BoldControls/      # Visual controls (Grid, Edit, ComboBox, etc.)
│   ├── ControlPacks/      # Renderer/follower pattern for UI binding
│   ├── Core/              # GUI base functionality
├── Common/                # Shared infrastructure
│   ├── Core/              # Base classes (BoldBase, BoldContainers, BoldDefs)
│   ├── Subscription/      # Observer pattern (BoldSubscription, BoldDeriver)
│   ├── Support/           # Utilities (BoldUtils, BoldGuard, BoldIndex)
│   ├── COM/               # COM infrastructure
│   ├── Logging/           # Log handling
│   └── IDE/               # Common IDE support
├── ObjectSpace/           # Object model runtime
│   ├── BORepresentation/  # Business object representation
│   ├── Core/              # BoldElements, BoldSystem
│   ├── Ocl/               # OCL parser and evaluator
│   ├── RTModel/           # Runtime type information
│   └── Undo/              # Undo/redo mechanism
├── Persistence/           # Database persistence
│   ├── Core/              # Persistence controllers and handles
│   ├── DB/                # Database persistence base
│   ├── FireDAC/           # FireDAC adapter (recommended)
│   ├── SOAP/              # SOAP persistence
│   └── Propagation/       # Change propagation
├── PMapper/               # Object-relational mapping
│   ├── SQL/               # SQL generation
│   ├── Default/           # Default mappers
│   └── DBEvolutor/        # Database schema evolution
├── Handles/               # Handle system for UI binding
├── MoldModel/             # Model representation and code generation
├── UMLModel/              # UML metamodel and editor
└── ValueSpace/            # Value interfaces and streaming
```

## Key Packages

| Package | Type | Description |
|---------|------|-------------|
| dclBold.dpk | Design-time | Main Bold components package |

## Contributing

1. **Report issues** - Use GitHub Issues for bugs and feature requests
2. **Submit PRs** - Fork the repo and submit pull requests
3. **Documentation** - Help improve docs and examples
4. **Testing** - Report compatibility issues with different databases/Delphi versions
5. **Spread the word** - Blog posts, conference talks, social media

Have suggestions for the roadmap? Open an issue on GitHub or discuss on Discord.

## Versioning

Version format: `YY.MM.patch` (e.g., 25.12.0)

| Source | Purpose |
|--------|---------|
| dproj | Current version (shown in About dialog) |
| [CHANGELOG.md](CHANGELOG.md) | Version history |
| Git tags | Backup/release markers |

**Release workflow:**
1. Move `[Unreleased]` items in CHANGELOG.md to new version section
2. Update version in dproj (Project > Options > Version Info)
3. Create git tag: `git tag v25.12.1`

## Resources

- **GitHub**: https://github.com/bero/BoldForDelphi
- **Issues**: https://github.com/bero/BoldForDelphi/issues
- **Discord**: https://discord.gg/C6frzsn
- **Wiki**: https://delphi.fandom.com/wiki/Bold_for_Delphi
- **GitHub Wiki**: https://github.com/bero/BoldForDelphi/wiki
- **Blog**: http://boldfordelphi.blogspot.com/

## License

MIT License - Source code made available by Embarcadero.

Original Version: 4.0.1.0 Bold for Delphi, Release 4.0 (2004-04-23)
