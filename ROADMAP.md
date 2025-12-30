# Bold for Delphi - Roadmap

This roadmap outlines the development direction for Bold for Delphi. It is a living document that evolves based on community feedback and contributions.

**Current Version**: Delphi 13
**Status**: Active Development

---

## Vision

Make Bold for Delphi a modern, well-documented, and reliable ORM framework for Delphi developers who value model-driven architecture.

It should be easy to get a good understanding of what Bold is and in what cases it is useful.

---

## Status Legend

| Icon | Meaning |
|------|---------|
| :white_check_mark: | Completed |
| :white_large_square: | In Progress |
| :calendar: | Planned |
| :thinking: | Under Consideration |

---

## 1. Delphi Version Support

| Status | Version |
|--------|---------|
| :white_check_mark: | Delphi 11.3 Alexandria |
| :white_check_mark: | Delphi 12.1 Athens |
| :white_check_mark: | Delphi 12.3 Athens |
| :white_check_mark: | Delphi 13 |
| :calendar: | Delphi 14+ |

**Goal**: Support each new Delphi version within 30 days of release.

---

## 2. Documentation

| Status | Notes |
|--------|-------|
| :white_check_mark: | Quick Start Guide (`Doc/quickstart.md`) |
| :white_check_mark: | Example documentation in browser |
| :white_check_mark: | Add about dialog to Bold menu item in IDE |
| :white_check_mark: | Changelog (`CHANGELOG.md`) |
| :calendar: | Complete API reference |
| :calendar: |  OCL language reference |
| :calendar: | Component reference guide |
| :calendar: | Video tutorials |
| :calendar: | Searchable documentation website (GitHub Pages + MkDocs) |

---

## 3. Database Support

### Current Adapters

| Status | Adapter | Database |
|--------|---------|----------|
| :white_check_mark: | FireDAC | SQL Server |
| :white_check_mark: | FireDAC | PostgreSQL |
| :white_check_mark: | FireDAC | Firebird |
| :white_check_mark: | FireDAC | SQLite |
| :white_check_mark: | XML | Not used |
| :white_large_square: | FireDAC | Oracle |
| :white_large_square: | FireDAC | MySQL |
| :white_large_square: | FireDAC | MariaDB |
| UniDAC (all supported databases) | UniDAC | :calendar: Needs testing (Optional as UniDAC is commercial) |

### Planned

| Status | Comment |
|--------|---------|
| :calendar: | Improve PostgreSQL-specific optimizations |
| :calendar: | Document database-specific configurations |

---

## 4. Testing & Code Quality

### Testing Infrastructure

| Status | Comment |
|--------|---------|
| :white_check_mark: | DUnitX test framework migration |
| :white_check_mark: | Code coverage reporting with DelphiCodeCoverage for tests (now 28 %) |
| :white_large_square: | Increase code coverage |
| :thinking: | Automated CI builds (GitHub Actions) |
| :thinking: | Automated test runs on pull requests |
| :thinking: | Performance benchmarks |

### Code Quality

| Status | Comment |
|--------|---------|
| :white_check_mark: | Remove deprecated database adapters (ADO, BDE, DBExpress) |
| :white_check_mark: | Removed C++Builder-specific code |
| :white_large_square: | Fix warnings from Pascal Analyzer |
| :calendar: | Consolidate duplicate code |
| :calendar: | Improve error messages |
| :thinking: | Modernize to Delphi language features (inline vars, generics) |

### Test Focus Areas

| Status | Comment |
|--------|---------|
| :calendar: | OCL parser and evaluator |
| :calendar: | Persistence layer |
| :calendar: | SQL generation |
| :calendar: | Subscription system |

---

## 5. Examples & Demos

| Status | Comment |
|--------|---------|
| :white_check_mark: | MasterDetail demo (basic CRUD) |
| :white_check_mark: | XML persistence demo |
| :white_check_mark: | LogBridge demo (logging integration) |
| :white_large_square: | Building/Person demo (associations) |
| :calendar: | REST API integration example |
| :calendar: | Real-world application template |

---

## 6. Core Improvements

### Performance

| Status | Comment |
|---------|--------|
| :calendar: | Generate BOLD_ID from Windows service instead of database |
| :calendar: | Object synchronization from Windows service |
| :calendar: | SpanFetch for efficient batch loading |
| :calendar: | Lazy loading improvements |
| :calendar: | Query result caching |
| :calendar: | Parallel object loading |

### Model Editor

| Status | Comment |
|--------|---------|
| :white_check_mark: | Save and Generate All |
| :white_check_mark: | Save prompt on close |
| :white_large_square: | Working SQL-script generator |
| :white_large_square: | Search/filter in model tree |
| :calendar: | Model Editor v2 using DevExpress grid with improved search |

---

## 7. New Features (Under Consideration)

These features are being evaluated based on community interest:

| Status | Comment |
|---------|--------|
| :thinking: | JSON serialization for REST APIs |
| :thinking: | Async/await database operations |
| :thinking: | LINQ-style query syntax alternative to OCL |
| :thinking: | Entity change tracking/auditing |
| :thinking: | Soft delete support |
| :thinking: | GraphQL integration |

---

## 8. Community & Ecosystem

| Status | Comment |
|--------|---------|
| :white_check_mark: | GitHub repository with issue tracking |
| :white_check_mark: | Discord community |
|  :calendar: | Contribution guidelines (CONTRIBUTING.md) |
| :calendar: | Issue templates for bugs/features |
| :thinking: | Register boldfordelphi.org domain |
| :thinking: | Newsletter or blog updates |

---

## How to Contribute

1. **Report issues**: Use GitHub Issues for bugs and feature requests
2. **Submit PRs**: Fork the repo and submit pull requests
3. **Documentation**: Help improve docs and examples
4. **Testing**: Report compatibility issues with different databases/Delphi versions
5. **Spread the word**: Blog posts, conference talks, social media

---

## Release History

| Version | Date | Highlights |
|---------|------|------------|
| Community 25.12.0 | 2025-12-22 | Delphi 13 support, improved examples, documentation |
| Community 25.10.0 | 2025-11-01 | DUnitX migration, code coverage |
| Original 4.0.1.0 | 2004-04-01 | Initial Boldsoft release |

---

## Feedback

Have suggestions for the roadmap? Open an issue on GitHub or discuss on Discord.

- **GitHub**: https://github.com/bero/BoldForDelphi/issues
- **Discord**: https://discord.gg/C6frzsn
