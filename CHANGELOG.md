# Changelog

All notable changes to Bold for Delphi will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

---

## [Unreleased]

### Added
- SQLite database support for MasterDetail demo
- Firebird database support for MasterDetail demo
- PostgreSQL database support for MasterDetail demo

### Changed
- Use `NativeInt` instead of `Integer` for TList-derived Items properties (fixes W1075 warnings)

### Fixed
- FireDAC SQLite catalog issue causing "near '.': syntax error"
- DateTime field handling for SQLite (stores as TEXT in ISO 8601 format)
- VendorHome changed to VendorLib for FireDAC connections

---

## [25.12.0] - 2025-12-22

### Added
- **Delphi 13 Support**: Full support for Delphi 13 (packages in `Delphi30` folder)
- **MasterDetail Demo**: Comprehensive example with configurable database persistence
- **Quick Start Guide**: New documentation at `Doc/quickstart.md`
- **Roadmap**: Project roadmap at `ROADMAP.md`
- **About Dialog**: Bold history and version info in IDE menu
- **Model Editor Enhancements**:
  - Save and Generate All plugin
  - Save prompt on close
  - TextHint support in TBoldEdit
- **Logging System**: Interface-based logging with pluggable sinks
- **LogBridge Example**: Demonstrates logging integration
- **XML Persistence**: Support for XML-based data storage
- **HTML Documentation**: Modern documentation for examples

### Changed
- Source files reorganized into categorized subfolders
- Use relative paths instead of environment variables
- Converted all source files to UTF-8
- Split `maan_UndoRedo.pas` into smaller unit files
- Removed CLX support from BoldEnvironment
- Simplified BoldGUIDUtils to use `TGUID.NewGuid.ToString`

### Removed
- Deprecated database adapters moved to `Source/Deprecated/` (ADO, BDE, DBExpress, Advantage, SQLDirect)
- C++Builder specific code
- Unused BoldMath unit (use `System.Math` instead)
- Duplicate functions from BoldUtils that exist in modern Delphi RTL
- Unused COM handles files
- Unused example projects (OLLE)

### Fixed
- Duplicate entries in `TBoldObjectList.InternalAddNew` (#33)
- Prevent manual modification of class lists (#33)
- Package compilation errors for Delphi 11.3, 12.1, 12.3
- Compiler warnings in various units
- Deprecated `TThread.Resume/Suspend` warnings
- Memory leak in test infrastructure

---

## [25.10.0] - 2025-11-01

### Added
- **DUnitX Test Framework**: Migration from legacy DUnit
- **Code Coverage**: DelphiCodeCoverage integration (28% coverage achieved)
- **Unit Tests**:
  - BoldSystem tests (creation, members, DefaultSystem)
  - BoldUtils tests
  - BoldGUIDUtils tests
  - FireDAC interface tests
  - Persistence tests
  - RTModel tests
- **Test Infrastructure**: Configurable database support for tests

### Changed
- Ported legacy DUnit tests to DUnitX
- Consolidated test code structure

### Removed
- Legacy test Framework folder

---

## [4.0.1.0] - 2004-04-01

### Notes
- Original release by Boldsoft AB
- Open-sourced by Embarcadero in 2020 under MIT license
- Base version for community development

---

## Version Numbering

- **Community versions**: `YY.MM.patch` (e.g., 25.12.0)
- **Original version**: 4.0.1.0 (Boldsoft release)

## Links

- [GitHub Repository](https://github.com/bero/BoldForDelphi)
- [Discord Community](https://discord.gg/C6frzsn)
- [Wiki](https://delphi.fandom.com/wiki/Bold_for_Delphi)
