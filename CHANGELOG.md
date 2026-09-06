# Changelog

All notable changes to Bold for Delphi will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

---

## [Unreleased]

---

## [26.9.0] - 2026-09-06

### Added
- **`collect(role)` evaluates in the persistence layer**: `TBSS_collect` is implemented and installed, so `self.child->collect(parent)` and the implicit `self.child.parent` translate to SQL instead of falling back to in-memory evaluation (#96)
- `IBoldDatabase.ReleaseAnotherDatabaseConnection` releases a connection obtained from `CreateAnotherDatabaseConnection` (#92)
- The UniDAC adapter tests compile through a stub-unit `DebugUniDAC` build configuration, so UniDAC is never required; they run on SQL Server and SQLite, against the vendored UniDAC 10.4 on Delphi 12 or UniDAC 11 on Delphi 13 (#89)
- Adapter-neutral persistence scenario tests run on every adapter (#88, #80, #89); end-to-end tests drive `TBoldDbCopy` and the database validator threads (#92, #93, #94)
- `BOLD_TEST_ENGINE` selects the test database engine without editing `UnitTest.ini`; `UnitTest\run_matrix.ps1` runs the adapter x engine matrix (FireDAC and UniDAC on SQLite and SQL Server) with one summary table
- `UnitTest\sync_gui_dpr.ps1` generates the GUI runner's unit list from the console runner, so TestInsight runs the whole suite (#84)
- DPM package specification, packed and published to the public feed at https://delphi.dev as `bero.BoldForDelphi` for Delphi 11, 12 and 13, plus a Boss manifest and a release checklist (#87)
- TMS Smart Setup definition (`tmsbuild.yaml`) and `version.txt`, so `tms install bero.boldfordelphi` builds and registers the design package for Delphi 11, 12 and 13; submitted to the community registry (#87)

### Fixed
- **Data corruption on FireDAC**: with `MultiRowInsertLimit = 1`, `PMCreate` wrote the second new object's attribute values into the third and later objects saved in one `UpdateDatabase`, because the per-row reset cleared a parameter snapshot instead of the query's real parameters (#88)
- `UseBatchQueries` was broken on both FireDAC and UniDAC (#80); it is now refused on engines where batching is unverified instead of failing at run time (#95)
- Undo: the redo value space was orphaned when undoing a block with no changes, and `DoUndoInTransaction` used a block after freeing it (#86)
- UniDAC: cached queries leaked when a connection wrapper was destroyed (#90); `CreateAnotherDatabaseConnection` created a `TUniConnection` nobody freed (#91); `GetDatabaseError` raised its own exception for providers without an error-code table, hiding the real database error (#98)
- `TBoldDbCopy` and the database validator dropped the extra connection wrappers they obtained (#92); validator worker threads were never freed (#93)
- `TBoldDbCopy` failed silently on FireDAC: `Prepare` ran before the parameter types were known, worker exceptions were swallowed, the row-count query leaked; failures are now logged and reported through `Errors` (#94)
- `FetchFromClassList` fetched every invalid single link of the other-end class one by one, one `SELECT` each, when deriving a multilink from a current class extent; it now falls back to the single database query and leaves invalid members alone (#97)
- The design packages could not be installed from the IDE after `LIBSUFFIX AUTO`: a leftover LIB version in the project files made the IDE look for `dclBold370.30.bpl` while the compiler produced `dclBold370.bpl` (#87)
- The two ignored `FetchRefetch` tests are rehabilitated and pin the classic `DbFetchOwningMember` path; the suite has no ignored tests (#83)
- Three `TestBoldOclEvaluation` tests assumed the creation order of unsaved objects in a class extent; they sort first
- The Delphi 11.3 design package was built without a version resource: its Debug/Win32 configuration overrode `VerInfo_IncludeVerInfo` to false
- The 26.8.1 Delphi 12.3 package had lost `NoObjectSpaceTransactions` again (the define restored for #85 was dropped by the version-stamping commit the same day); all three packages now declare it in the same project group (#85)

### Changed
- Design packages are named by compiler through `LIBSUFFIX AUTO` (`dclBold280.bpl`, `dclBold290.bpl`, `dclBold370.bpl`); the Delphi 12.1 CE package project is retired (#87)
- `CanEvaluateInPS` answers True for `collect(role)`, where it previously answered False on every engine (#43, #96)
- Test suite at 2182 tests (2182 passing, 0 ignored, 0 failed); code coverage 58.5%

---

## [26.8.1] - 2026-08-16

### Added
- **Parameterized ID-list SQL for SQL Server**: `MaxParamsInIdList` raised to 500 (#75), multilink fetch and `PMDelete` use parameterized lists with explicit `ParamCheck` (#76), and IN-lists are padded to fixed bucket sizes so the plan cache is reused (#77)
- Bound parameter values are now written with the SQL log, rendering NULL as `NULL` and blob payloads as a placeholder; an optional UnitLog-backed sink is selected by the new `BOLD_LegacyLog` / `BOLD_UnitLog` defines in `Bold.inc` (#82)
- `IBoldQuery` gains `ParamCount` and `Param[]` (#82)
- Unit-level helpers `GetSharedString` / `GetSharedAnsiString` in `BoldSharedStrings`, and `BoldNamesEqual` / `BoldAnsiEqual` / `BoldStrAnsiEqual` in `BoldUtils` (#82)
- Benchmarks for id-list SQL covering bucket collapse and the plan-cache bound (#79)
- First test coverage for `EmbeddedSingleLinks` stamping on the `DbFetchOwningMember` path (#83)
- `setup-tests.ps1` and `UnitTest/README.md` to make first-time setup work without hunting for dependencies

### Fixed
- **Orphaned read transactions on the shared connection**: `Clear` did not close, so releasing a query to the cache left its transaction open, and `ExecSQL` / `Open` discarded ownership of a transaction the query itself had started. Row locks then blocked other work until the connection was recycled. Fixed for both UniDAC and FireDAC (#81)
- `SetEmptyValue` left null non-nullable attributes null and raised on valuesets (#74)
- `SubscribeToHandle` no longer dereferences a variable list rebuilt during teardown (#61)
- The Delphi 12.3 design-time package had lost `NoObjectSpaceTransactions`, compiling a different `TBoldSystem` interface than the other three packages (#85)
- `UnitTestGUI` project could not build: missing `Code\Main` in the unit search path (#84)
- Documentation named Delphi 13 as Athens instead of Florence, pointed at package folders that never existed (`packages/Delphi30/`), and advertised a seven-month-old release as current

### Changed
- Release tags follow Year.Month.build with no zero padding, so they parse as semantic versions for dependency managers such as Boss
- Both test runners compile the default configuration; the `Attracs` define was removed from `UnitTestGUI.dproj` so console and GUI runs test the same code (#84)
- Version information normalized to 26.8.1.0 across all four design-time package projects, which had drifted to four different values
- Test suite at 2165 tests (2163 passing, 2 ignored)

---

## [26.8.0] - 2026-08-03

### Added
- 7 XML roundtrip tests for OLW node streaming (NodeList, Operation, TypeNode, ListCoercion, Member, Literals, OclCondition)
- 5 XML roundtrip tests for BoldCondition streaming (28.7% → 99.4% coverage)

### Fixed
- 4 memory leaks in OLW XML streamer `ReadObject` methods — `CreateObject` allocated internal lists that were orphaned when `ReadObject` overwrote field pointers (#34)
- Add try-finally blocks for Bindings cleanup in `TBoldXMLOCLConditionStreamer`
- Standardize Pascal casing and fix method name typo
- Correct field name casing from `flist` to `fList` in `TBoldOLWNodeList`

---

## [26.1.0] - 2026-01-06

### Added
- **Oracle Database Support**: Full Oracle support in Bold core persistence mappers (#10)
- `AsObjectList` helper for `TBoldAbstractListHandle` (#17)
- Codecov.io integration for code coverage tracking
- PowerShell scripts for unit tests and coverage

### Changed
- Refactored code to reduce duplication with extracted helper methods (#13, #15, #18, #19):
  - `IsValidIdentifier`, `SetPersistenceTaggedValue`, `EnsureConstraintListAndAdd`
  - `FreePublisherInstance`, `EnsurePublisher`, `BuildWCFOrQueryWithOperator`
  - `InitializeIndirectLinkFields`, `InitializeDirectLinkFields`, `InitializeTimestampFields`
  - `SystemHasDirtyObjects`, `DoPreChangeIfNeeded`
- Removed C++ support from `BoldUMLModelValidator` (#16)
- Refactored `TBANumeric.GetStringRepresentation` to extract common logic (#19)
- Refactored `TBABlob` validation (#14)
- Code coverage improved from 28% to 40%

### Fixed
- Missing `Result :=` in `TBABlobImageJPEG/BMP.GetStringRepresentation` (#20)
- String columns ignoring `AllowNull` when using `EmptyStringMarker` (#11)
- 64-bit `ACCESS_VIOLATION` in grid and edit controls (#9)

---

## [25.12.1] - 2025-12-30

### Added
- SQLite database support for MasterDetail demo
- Firebird database support for MasterDetail demo
- PostgreSQL database support for MasterDetail demo
- CHANGELOG.md with version history

### Changed
- Use `NativeInt` instead of `Integer` for TList-derived Items properties (fixes W1075 warnings)
- Simplified versioning: removed BoldVersionInfo.pas, version now in dproj only

### Fixed
- FireDAC SQLite catalog issue causing "near '.': syntax error"
- DateTime field handling for SQLite (stores as TEXT in ISO 8601 format)
- VendorHome changed to VendorLib for FireDAC connections

---

## [25.12.0] - 2025-12-22

### Added
- **Delphi 13 Support**: Full support for Delphi 13 (packages in `Delphi30` folder)
- **MasterDetail Demo**: Comprehensive example with configurable database persistence
- **Quick Start Guide**: New documentation at `quickstart.md`
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
