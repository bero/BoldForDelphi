# BoldForDelphi

Bold is a Model Driven Architecture (MDA) framework and Object-Relational Mapping (ORM) tool for Delphi. It allows you to start with a UML model of your application and execute it with a sophisticated object-relational mapping layer, OCL query language, change synchronization, and IDE-integrated tools.

## Supported Delphi Versions

- Delphi 12.3 Athens (packages/Delphi29.3/)
- Delphi 12.1 Athens (packages/Delphi29.1/)
- Delphi 11.x Alexandria (packages/Delphi28/)

## Building

### Prerequisites
- Delphi 12.3 (or target version) installed
- MSBuild available via rsvars.bat

### Build the Package
```batch
cd packages\Delphi29.3
build.bat
```

Or manually:
```batch
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild dclBold.dproj /p:Config=Debug /p:Platform=Win32
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
│   ├── FormGen/           # Automatic form generation
│   └── IDE/               # Design-time property editors
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
├── ValueSpace/            # Value interfaces and streaming
└── Extensions/            # Optional extensions (OLLE, etc.)
```

## Key Packages

| Package | Type | Description |
|---------|------|-------------|
| dclBold.dpk | Design-time | Main Bold components package |

## Contributing

This is the develop branch for direct commits and feature branch merges.
External developers should fork the repository and submit pull requests.

## Resources

- Wiki: https://delphi.fandom.com/wiki/Bold_for_Delphi
- Blog: http://boldfordelphi.blogspot.com/
- Discord: https://discord.gg/C6frzsn

## License

MIT License - Source code made available by Embarcadero.

Original Version: 4.0.1.0 Bold for Delphi, Release 4.0 (2004-04-23)
