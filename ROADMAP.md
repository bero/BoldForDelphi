# Bold for Delphi Roadmap

This roadmap outlines the development direction for Bold for Delphi. It is a living document that evolves based on community feedback and contributions. What each release contained is in the [Changelog](https://bero.github.io/BoldForDelphi/changelog/).

## Vision

Make Bold a modern, well-documented and reliable model-driven framework and ORM for Object Pascal developers, in Delphi and in Lazarus, on Windows, Linux and macOS.

## Status Legend

| Status | Meaning |
|--------|---------|
| Done | Completed |
| In Progress | Currently being worked on |
| Planned | Scheduled for future release |
| Considering | Under evaluation |

## 1. Platforms and Toolchains

### Delphi versions

| Status | Version |
|--------|---------|
| Done | Delphi 11.3 Alexandria |
| Done | Delphi 12.x Athens (one package for 12.1 Community Edition through 12.3) |
| Done | Delphi 13 Florence |
| Planned | Delphi 14+ |

**Goal**: Support each new Delphi version within 30 days of release.

### Cross-platform

| Status | Item |
|--------|------|
| Done | Framework-neutral runtime: the object space, OCL, handles and persistence build without the VCL (#117) |
| Done | Lazarus / Free Pascal: runtime packages `Bold`, `BoldLCL`, `BoldZeos` and design-time packages `dclBold`, `dclBoldZeos` (#117) |
| Done | Free Pascal test suite on Linux and Windows, run by GitHub Actions on every push (#117) |
| Done | FireMonkey clients through `TBoldDataSet` and LiveBindings (#117) |
| Done | Delphi on Linux64: FMX (through FmxLinux), console programs and the test suite (#117) |
| In Progress | Standalone model editor with LCL and FMX clients, `Tools/ModelEditor` (#128) |
| Planned | macOS (#127) |
| Planned | Bold-aware user interface components for FMX and LCL |

## 2. Documentation

| Status | Item |
|--------|------|
| Done | Quick Start Guide |
| Done | Example documentation |
| Done | About dialog in IDE |
| Done | Changelog |
| Done | Documentation website (GitHub Pages + MkDocs) |
| In Progress | API reference: class guides, plus the class reference converted from the Bold 4.0 help |
| Planned | OCL language reference |
| Planned | Component reference guide |
| Planned | Video tutorials |

## 3. Database Support

### Current Adapters

| Status | Adapter | Database |
|--------|---------|----------|
| Done | FireDAC | SQL Server |
| Done | FireDAC | PostgreSQL |
| Done | FireDAC | Firebird |
| Done | FireDAC | SQLite |
| Done | FireDAC | Oracle |
| Done | XML | File-based |
| In Progress | FireDAC | MySQL |
| In Progress | FireDAC | MariaDB |
| Done | UniDAC | SQL Server, SQLite and PostgreSQL (optional: UniDAC is commercial) |
| Done | Zeos (Free Pascal) | SQLite and PostgreSQL |

### Planned Improvements

- Improve PostgreSQL-specific optimizations
- Document database-specific configurations

## 4. Testing & Code Quality

### Testing Infrastructure

| Status | Item |
|--------|------|
| Done | DUnitX test framework migration |
| Done | Code coverage reporting with DelphiCodeCoverage |
| Done | Adapter x engine test matrix (`UnitTest\run_matrix.ps1`: FireDAC and UniDAC on SQLite, SQL Server and PostgreSQL) |
| Done | Memory checks: FastMM4 FullDebugMode for Delphi, heaptrc for Free Pascal |
| In Progress | Increase code coverage |
| In Progress | Performance benchmarks (id-list SQL benchmarks exist) |
| Done | GitHub Actions: the Free Pascal suite on every push and pull request, on Linux and Windows, SQLite and PostgreSQL |
| Considering | Delphi builds and tests in CI |

### Code Coverage Progress

**Live tracking**: [Codecov.io Dashboard](https://app.codecov.io/github/bero/BoldForDelphi)

### Test Focus Areas

| Status | Area |
|--------|------|
| Done | OCL parser and evaluator (117+ end-to-end evaluation tests) |
| In Progress | Persistence layer: FireDAC and UniDAC adapters, batch queries, DbCopy and validator end-to-end tests |
| In Progress | SQL generation from OCL (`collect(role)` added in 26.9.0) |
| Planned | Subscription system |

### Code Quality

| Status | Item |
|--------|------|
| Done | Remove deprecated database adapters (ADO, BDE, DBExpress) |
| Done | Remove C++Builder-specific code |
| In Progress | Fix warnings from Pascal Analyzer |
| Planned | Consolidate duplicate code |
| Planned | Use generics |

## 5. Examples & Demos

| Status | Item |
|--------|------|
| Done | MasterDetail demo (basic CRUD) |
| Done | XML persistence demo |
| Done | LogBridge demo (logging integration) |
| Done | Five-step tutorial (`examples/Tutorial`) |
| Done | Invoicing example with FMX, LCL and console clients |
| Done | A README for every example |
| In Progress | Building/Person demo (associations) |
| Planned | REST API integration example |
| Planned | Real-world application template |

## 6. Core Improvements

### Performance

| Status | Item |
|--------|------|
| Planned | Generate BOLD_ID from Windows service |
| Planned | Object synchronization from Windows service |
| Planned | SpanFetch for efficient batch loading |
| Planned | Lazy loading improvements |
| Planned | Query result caching |

### Model Editor

| Status | Item |
|--------|------|
| Done | Save and Generate All |
| Done | Save prompt on close |
| In Progress | Working SQL-script generator |
| In Progress | Search/filter in model tree |
| Planned | Model Editor v2 using DevExpress grid |

## 7. New Features (Under Consideration)

- JSON serialization for REST APIs
- Async/await database operations
- LINQ-style query syntax alternative to OCL
- Entity change tracking/auditing
- Soft delete support
- GraphQL integration

## 8. Distribution

| Status | Channel |
|--------|---------|
| Done | GitHub releases with the design packages for Delphi 11.3, 12.3 and 13 |
| Done | TMS Smart Setup: `tms install bero.boldfordelphi` (community registry) |
| Done | DPM: `dpm install bero.BoldForDelphi` from the public feed at delphi.dev |
| Considering | GetIt (Embarcadero submission) |
| Considering | Delphinus (requires a non-fork repository) |

## 9. Community & Ecosystem

| Status | Item |
|--------|------|
| Done | GitHub repository with issue tracking |
| Done | Discord community |
| Done | Contribution guidelines |
| Planned | Issue templates for bugs/features |
| Considering | boldfordelphi.org domain |

## How to Contribute

See [Contributing](https://bero.github.io/BoldForDelphi/contributing/).

## Release History

See the [Changelog](https://bero.github.io/BoldForDelphi/changelog/).

## Feedback

Have suggestions for the roadmap? Open an issue on GitHub or discuss on Discord.

- **GitHub**: [https://github.com/bero/BoldForDelphi/issues](https://github.com/bero/BoldForDelphi/issues)
- **Discord**: [https://discord.gg/C6frzsn](https://discord.gg/C6frzsn)
