# Bold for Delphi Roadmap

This roadmap outlines the development direction for Bold for Delphi. It is a living document that evolves based on community feedback and contributions.

**Current Version**: 26.9.0
**Status**: Active Development

## Vision

Make Bold for Delphi a modern, well-documented, and reliable ORM framework for Delphi developers who value model-driven architecture.

## Status Legend

| Status | Meaning |
|--------|---------|
| Done | Completed |
| In Progress | Currently being worked on |
| Planned | Scheduled for future release |
| Considering | Under evaluation |

## 1. Delphi Version Support

| Status | Version |
|--------|---------|
| Done | Delphi 11.3 Alexandria |
| Done | Delphi 12.x Athens (one package for 12.1 Community Edition through 12.3) |
| Done | Delphi 13 Florence |
| Planned | Delphi 14+ |

**Goal**: Support each new Delphi version within 30 days of release.

## 2. Documentation

| Status | Item |
|--------|------|
| Done | Quick Start Guide |
| Done | Example documentation |
| Done | About dialog in IDE |
| Done | Changelog |
| Done | Documentation website (GitHub Pages + MkDocs) |
| Planned | Complete API reference |
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
| Done | UniDAC | SQL Server and SQLite (optional: UniDAC is commercial; the adapter tests run on both engines) |

### Planned Improvements

- Improve PostgreSQL-specific optimizations
- Document database-specific configurations

## 4. Testing & Code Quality

### Testing Infrastructure

| Status | Item |
|--------|------|
| Done | DUnitX test framework migration |
| Done | Code coverage reporting with DelphiCodeCoverage |
| Done | Adapter x engine test matrix (`UnitTest\run_matrix.ps1`: FireDAC and UniDAC on SQLite and SQL Server) |
| In Progress | Increase code coverage |
| In Progress | Performance benchmarks (id-list SQL benchmarks exist) |
| Considering | Automated CI builds (GitHub Actions) |
| Considering | Automated test runs on pull requests |

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
| Planned | Contribution guidelines |
| Planned | Issue templates for bugs/features |
| Considering | boldfordelphi.org domain |

## How to Contribute

1. **Report issues** - Use GitHub Issues for bugs and feature requests
2. **Submit PRs** - Fork the repo and submit pull requests
3. **Documentation** - Help improve docs and examples
4. **Testing** - Report compatibility issues with different databases/Delphi versions
5. **Spread the word** - Blog posts, conference talks, social media

## Release History

| Version | Date | Highlights |
|---------|------|------------|
| 26.9.0 | 2026-09-06 | `collect(role)` in PS evaluation, UseBatchQueries fixed on both adapters, FireDAC PMCreate corruption fix, UniDAC leak fixes, package managers |
| 26.8.1 | 2026-08-16 | Parameterized ID-list SQL for SQL Server, read-transaction fix, SQL parameter logging |
| 26.8.0 | 2026-08-03 | XML roundtrip tests for OLW nodes and conditions |
| 26.1.0 | 2026-01-06 | Oracle support in the persistence mappers |
| 25.12.1 | 2025-12-30 | SQLite and Firebird support in the MasterDetail demo |
| 25.12.0 | 2025-12-22 | Delphi 13 support, improved examples, documentation |
| 25.10.0 | 2025-11-01 | DUnitX migration, code coverage |
| 4.0.1.0 | 2004-04-23 | Original Boldsoft release |

## Feedback

Have suggestions for the roadmap? Open an issue on GitHub or discuss on Discord.

- **GitHub**: [https://github.com/bero/BoldForDelphi/issues](https://github.com/bero/BoldForDelphi/issues)
- **Discord**: [https://discord.gg/C6frzsn](https://discord.gg/C6frzsn)
