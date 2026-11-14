# Changelog

All notable changes to Bold for Delphi will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

---

## [Unreleased]

### Added
- Lazarus packages in `packages/Lazarus` (#117): `Bold.lpk`, the
  framework-neutral runtime that builds with Free Pascal; `BoldLCL.lpk` with
  `BoldEnvironmentLCL`, the LCL twin of `BoldEnvironmentVCL`; and
  `BoldZeos.lpk` with the Zeos database adapter. The Invoicing example shows
  them: an LCL client with `TDBGrid`s over `TBoldDataSet`
  (`examples/Compound/Invoicing/LCL`) and a console runner on SQLite through
  Zeos (`examples/Compound/Invoicing/Zeos`). GitHub Actions builds both with
  lazbuild and runs them on every push. The design-time package `dclBold.lpk`
  puts the handles, the model, the persistence handles and `TBoldDataSet` on
  the Lazarus component palette, with the icons of the Delphi palette, and
  `dclBoldZeos.lpk` adds `TBoldDatabaseAdapterZeos`. `Expression` of the
  handles and of a `TBoldDataSet` field has a property editor that checks
  the OCL expression against the model and shows its result type or the
  error; the other property and component editors of the Delphi package
  are not ported
- The Invoicing FMX client (`examples/Compound/Invoicing/FMX`) has the Linux64
  platform (#117): the project deploys `Invoicing.bld` next to the program,
  where the client now looks for the model first
- The Delphi test suite builds and runs on Linux64: `UnitTest\build.ps1
  -Platform Linux64` builds the console runner, and `UnitTest/README.md` says
  how to run it. Test units whose subject is Windows only are empty there, and
  the leak checks count through a wrapper around libc's `malloc` (#117)
- Memory checks for both test suites, each failing on a leak or a use of freed
  memory with the stack trace or line that caused it (#117):
  - `UnitTest\check_memory.ps1` runs the Delphi suite under FastMM4 in
    FullDebugMode (build configuration `DebugFastMM4`). FastMM4 is an optional
    dependency through the `FastMM4` environment variable, not distributed
    with Bold; normal builds do not need it.
  - `BOLD_HEAPTRC=1 sh UnitTest/FPC/build.sh` runs the Free Pascal suite under
    heaptrc, which the GitHub Actions job does on every push.
- XML streaming builds on Delphi's cross-platform `Xml.xmldom` with the OmniXML
  vendor for Delphi targets other than Windows, so the runtime core no longer
  needs MSXML or COM there. Windows keeps MSXML; defining `BOLD_XMLDOM` selects
  the new backend on Windows too, for example to test it (#117)
- `BoldClock`: one-shot alarms delivered on the main thread without a window
  handle or a message loop. A background thread hands each due alarm to the
  main thread with `TThread.Queue`, so it runs on the next `CheckSynchronize`,
  which VCL and FMX call from their idle loops and a console application calls
  itself. `BoldSharedClock` is the instance shared by the application (#119)

### Changed
- The parts of `BoldControlPack`, `BoldStringControlPack` and `BoldGUI` that
  need the VCL (`DrawOnCanvas`, the renderer's `OnSetFont`/`OnSetColor`, the
  popup menu, the lookup of the hosting form) are compiled only under the new
  define `BOLD_VCL`, which `bold.inc` sets for Delphi on Windows. Without it
  the followers and `TBoldDataSet` link no VCL unit, so `TBoldDataSet` now
  builds and passes its tests on Linux64 too. A Windows project defines
  `BOLD_NO_VCL` to leave the VCL parts out, for example an FMX application.
  Without the VCL the application drains the display queue itself
  (`TBoldQueueable.DisplayAll`), and `FindHostingForm` returns nil, so
  `ApplyAllMatching`/`DiscardChangeAllMatching` match no form. `BOLD_NO_VCL`
  was removed in 956a3ad, when the core stopped needing it; it is back with
  this narrower meaning (#117)
- `TBoldOcl` refreshes its `today` and `now` variables through `BoldSharedClock`
  instead of two VCL `TTimer`s, so `BoldOcl` no longer imports `ExtCtrls` (#119).
  An application that never calls `CheckSynchronize` keeps the values from
  construction time, as it did before without a message loop.
- `TBoldUnloaderHandle` ticks through `BoldSharedClock` instead of a VCL
  `TTimer`, so `BoldUnloaderHandle` no longer imports `ExtCtrls`, and the
  unloader now also runs in console programs and services that call
  `CheckSynchronize`, on Linux too (#117). The handle schedules a tick only
  while its unloader is active.
- `TBoldDataSet` refreshes after Bold-side changes through `BoldSharedClock`
  instead of a VCL `TTimer`, so `BoldDataSet` no longer imports `ExtCtrls` or
  `BoldEnvironmentVCL` (#117). As before, the first change re-reads the
  records at once, and the ones within the next 100 ms together after that;
  the delayed refresh now runs on `CheckSynchronize`, so also without a
  message loop.
- `TBoldPersistenceProgressNotifier`, which shows fetch and update progress in
  VCL controls, moved from `BoldPersistenceNotifier` to the new unit
  `BoldPersistenceProgressNotifier` (`Source\Persistence\UtilsGUI`, part of
  `dclBold`). `BoldPersistenceNotifier` with `TBoldPersistenceNotifier` imports
  no VCL unit now and compiles for every platform and for Free Pascal (#117).
  **Action needed:** a unit that declares a `TBoldPersistenceProgressNotifier`
  adds `BoldPersistenceProgressNotifier` to its uses clause; the IDE does so
  when the form is saved. Forms load unchanged, the class name is the same.
- `TBoldExternalObjectSpaceEventHandler` has its `PropagatorHandle` property
  only on Windows, where the COM propagator exists. So `BoldExternalObjectSpaceEventHandler`
  and `BoldExpressionSubscriber` (`TExpressionSubscriber` listens to the handler)
  no longer pull in the propagator's COM type library, and they compile for every
  platform and for Free Pascal (#117). The handler only kept that reference and
  never used it, so nothing changes on Windows; the UDP broadcaster feeds the
  handler without a propagator.
- `BoldEnvironment` no longer links the VCL environment into every application.
  A GUI framework unit registers its environment with
  `BoldRegisterFrameworkEnvironment`. `BoldEnvironmentVCL` does so, and the
  units that need it import it: the Bold-aware VCL controls, `BoldControlPack`
  (so the DevExpress Bold controls too), `BoldListenerHandle`,
  `BoldUDPModificationBroadcaster` and `BoldComEventQueue`. Without a framework
  environment the freestanding environment now provides a queue, which the
  application drains itself with
  `TBoldQueueable.DisplayAll` or `PerformPreDisplayQueue` (#118)
  **Action needed:** a VCL application that queues display work through its own
  `TBoldQueueable` descendants or `AddToPreDisplayQueue`/`AddToPostDisplayQueue`,
  and links none of those units, must add `BoldEnvironmentVCL` to one uses
  clause. `BoldInternalVCLConfiguration` is renamed
  `BoldInternalFrameworkConfiguration`.
- The Bold core links no GUI framework: `BoldComponentValidator` no longer imports
  `BoldEnvironmentVCL`, so handles, the model and persistence no longer pull the
  VCL into an application, and a console, service or FMX program built on them
  links no VCL unit (#117). In the IDE, saving a VCL form, frame or data module
  that holds a non-visual Bold component adds `BoldEnvironmentVCL` to its uses
  clause; Bold's VCL controls import it as before. The IDE treats a data module
  without a class group as VCL, so a data module that a console, service or FMX
  program shares declares `{%CLASSGROUP 'System.Classes.TPersistent'}` in its
  implementation section, as the Invoicing example's `dmInvoicing` now does.
  When a function needs a GUI framework and none is registered, the error now
  names the unit to link.
  **Action needed:** a VCL application with no Bold-aware control whose forms are
  not saved again in the IDE adds `BoldEnvironmentVCL` to one uses clause.
  Without it the application runs on the freestanding environment, where Bold's
  questions to the user (deleting tables when the database is created, the
  database validator) raise that error and informational messages go to the log.

### Removed
- `BoldEnvironmentAllowBothUseVCL`, which needed the removed CLX environment and
  did not compile (#118)

### Fixed
- `TBoldDataSet` no longer loses a Bold-side change of one row that is
  displayed while it reads another row and creates that row's cell followers
  (a log window that pumps messages during a fetch does that). The dataset
  ignored every follower in that moment; it now ignores only those of the
  row being read and refreshes for the others once the row is read (#117)
- `TBoldDataSet` no longer overflows the stack when the object space changes
  while its form is being destroyed, for example a `Discard` in the form's
  `OnDestroy`. The form's list handles then raise when asked for their list
  and tell again that their value changed, and the dataset asked again each
  time. It now leaves a handle alone that is being destroyed, as it does when
  it is being destroyed itself (#117)
- `TBoldDataSet` compiles again with Delphi 11-13. It still overrode the
  `TDataSet` methods from before Delphi 2009 and is now ported to the current
  API, with `Test.BoldDataSet` (Windows for now). The port also fixes text
  fields that wrote past their record buffer and lost non-ASCII text, object
  references cut to 32 bits on Win64, date and time fields that were read
  back wrong, `AppendRecord`/`InsertRecord` creating two objects, the dataset
  reopening on its first record after the first scroll, and an empty list that
  could not be opened. Text fields are now `ftWideString` (were `ftString`),
  and object-reference fields `ftLargeint` (were `ftInteger`). It is still in
  no package (#117)
- `TBoldDataSet` shows a changed attribute. A value changed on the Bold side,
  with no row added, removed or moved, stayed as it was in the dataset and in
  every control bound to it until something else refreshed it. The control
  packs create the followers of a row's cells only when a control asks for
  them, and the dataset never did, so nothing subscribed to the attributes.
  It now creates them for the rows it reads into its record buffers, and only
  for those, so a long list costs no more than the rows shown (#117)
- The Zeos adapter wrote a currency attribute to SQLite 10,000 times too
  large (120.50 as 1205000): Zeos binds a currency parameter for SQLite as its
  scaled integer, which suits a column declared CURRENCY or MONEY, and Bold's
  column is DOUBLE PRECISION. The adapter now binds it as a Double for SQLite,
  which stores the same value as FireDAC does under Delphi, also when a
  currency is copied from a field or another parameter, as DbCopy does (#117)
- On Delphi for Linux, a date and time that Bold wrote did not parse back with
  the US English locale: setting `AsString` of a `TBADateTime` or `TBATime`,
  and the OCL operations `strToDateTime` and `strToTime`, raised "is not a
  valid time". Delphi's RTL there takes its formats from ICU, whose locale
  data (CLDR 42 and later) puts U+202F NARROW NO-BREAK SPACE before AM/PM, and
  its `StrToTime` accepts only an ordinary space. Bold now changes U+202F and
  U+00A0 NO-BREAK SPACE to a space before it parses, through
  `BoldNoBreakSpacesToSpaces` in `BoldUtils` (#117)
- The FireDAC unit tests on SQLite use a temporary database file per run,
  deleted when the run ends, instead of the URI
  `file:memdb1?mode=memory&cache=shared`. FireDAC's SQLite on Windows does not
  read URIs, so that was a file named `file` next to `UnitTest.exe` (an NTFS
  stream) that kept its tables from one run to the next. The system SQLite on
  Linux does read them, and its in-memory database was gone whenever Bold
  closed its connection. The UniDAC tests keep the URI, which UniDAC's SQLite
  reads as an in-memory database. Two empty `file` blobs that a test run had
  left in the repository are removed (#120, #117)
- `TBoldDirectoryTraverser` missed files without an extension on Linux and macOS:
  its default `Mask` was `*.*`, which there matches only names with a dot. The
  default is now `*`, which matches every name on every platform, as `*.*` does
  on Windows (#117)
- `TBoldExternalObjectSpaceEventHandler` reported an access violation for an
  event about a class that the local model does not have, for example from a
  client with a newer model. It now raises `EOSS` "Cannot find the class ... in
  the system" for that event, as its own handlers already did (#117)
- `TBoldExternalObjectSpaceEventHandler` raised an access violation instead of
  the real error when an event in a message failed and `OnFailedMessage` was not
  assigned. Without the event, the error reaches the caller again, as it did
  before the per-event error handling came in 2024; with it, the handler reports
  the failing event and goes on with the next one (#117)
- `TExpressionSubscriber` could crash when another client changed a member that
  its expression does not use: `MemberChanged` ended by reading an
  uninitialized local variable. It also read the class and member names of the
  handler's events from the wrong `TVarRec` field, which worked on Delphi only
  because of the record layout and gave wrong names under Free Pascal (#117)
- `TBoldPersistenceNotifier` and `TBoldPersistenceProgressNotifier` received no
  persistence events. Since the 2024 merge,
  `TBoldPersistenceHandle.AddPersistenceSubscription` subscribed only the
  handle itself to the controller, never the caller. It subscribes the caller
  again, to all persistence events or only to those in its `Events` argument,
  which it ignored before. So the notifiers report fetches and updates, and
  the progress notifier moves its progress bar again (#117)
- The FireDAC adapter lowercased the connection's `Database` parameter on every
  `Open`. For a file database such as SQLite that is a path, so on Linux and
  macOS Bold worked on a second file while the application used the first one.
  `Open` now leaves the name as it is, like the UniDAC and Zeos adapters (#117)
- `TBoldDirectoryTraverser` never closed its directory and file searches,
  leaking a find handle on Windows and heap memory elsewhere for each search.
  It also did not compile for Delphi off Windows, because of a drive-letter
  check through `DiskSize`, which is now Windows-only (#117)
- The runtime core did not compile for Delphi's Linux 64-bit target.
  `BoldUnicodeCompareLen` called the Windows API on every Delphi target. Several
  methods were declared with `Longint` and implemented with `Integer`, which on
  64-bit POSIX Delphi are different types, because `Longint` is 64-bit there.
  The cross-platform units (the OCL parser, the database interfaces and their
  adapters, the default PMapper, the thread-safe log) use `Integer` and
  `Cardinal` now, so every platform has the Windows sizes; `IBoldParameter`'s
  `AsInteger`, `AsSmallInt` and `AsWord` are `Integer` (#117)
- When a PreUpdate subscriber deleted a never persisted object of the batch,
  `UpdateDatabase` read that object's locator after it had been freed. It
  worked only because the freed memory still held the old values. Object
  destruction now waits until the batch has been filtered again (#62)
- `TBoldXMLDocument.LoadXML` raised its parse error without the parser's reason;
  the message now ends with it. Under Free Pascal, `TBoldXMLDocument.RootXML`
  started with a line break, and `LoadXML` leaked the partly read document of
  XML that did not parse (#117)
- `TBoldPersistenceOperation.RemoteExecute`, the SOAP client of the persistence
  operations, leaked the XML nodes of a reply that held a SOAP fault or no
  response. It now frees them on every path, and builds its request and reply
  through `TBoldXMLDocument` instead of MSXML directly (#117)
- Saving a form or data module in the IDE wrote an empty model and three default
  `Boldify` settings into the form file for a `TBoldModel` that holds no model,
  such as one whose model is loaded at run time. `TBoldModel` now stores its
  model and its `Boldify` settings only when they differ from the defaults; form
  files that contain them still load (#117)
- A subclass extent filled from an already loaded superclass extent came back
  empty: `TBoldClassListController.FillFromClassList` added through the list's
  `AddLocator`, which has been a no-op for class lists since 25.12.1. The fill
  now goes through the controller's internal add (#102)
- `TBoldSystem.DiscardPersistent` looped forever when an object was re-dirtied
  while it was being discarded. It now stops after three passes without the
  dirty count reaching a new low: once by escalating to a full discard, then by
  logging the remaining objects through `BoldLog` and giving up (#103)
- `objectTimeStamp` and `boldTime` applied behind a nil single-valued role
  raised an Access Violation instead of yielding null: both are OCL operations,
  so they bypass the nil handling that member navigation gets. `TBOS_ObjectTimeStamp`
  and `TBOS_BoldTime` now return null for a nil object. `boldTime` was also
  registered twice; the shadowed duplicate `TBOS_ObjectTime` is removed (#105)

---

## [26.9.1] - 2026-09-16

### Added
- Win64 packages for Delphi 12 and 13 in the DPM specification. DPM derives the
  platforms a package supports from its build and design entries rather than from
  `targetPlatforms`, so declaring Win64 there is what `dpm install
  -platforms=Win64` needed (#87)
- An OCL workbench demo, and a Database menu that lets the demos switch engine
  with their preconditions checked first
- A generated class reference converted from the Bold 4.0 help file
- A README for every example, recording which ones no longer run as shipped
- Conway gained a usable interface: load, save, draw and help

### Fixed
- Bold's HTTP transport sends and receives its payload as bytes rather than
  characters, so non-ASCII content survives the round trip (#99)
- An invalid `TBoldObjectList` cast in `cxGridBoldSupportUnit` that crashed any
  DevExpress grid bound to a list of types
- Conway no longer crashes on a board containing blank lines
- The ASP demo client's main form, which had been a byte-for-byte copy of the
  data module's form since the 2020 source release and could not open

### Changed
- The ASP example is ported off IBX to FireDAC with SQLite, so it builds with
  nothing installed; its README now documents hosting the ISAPI extension under
  IIS
- The OCL Explorer is ported off DevExpress onto `TBoldGrid`, taking the
  executable from 26.7 MB to 10.6 MB
- The GUI uses Segoe UI 9 pt throughout
- Releases ship the Win32 design BPLs only, with the reasoning recorded in the
  release checklist (#87)
- Bold is listed on the TMS Smart Setup community registry; installing with
  Smart Setup and with DPM is documented (#87)
- Documentation is consolidated: ROADMAP.md is the single roadmap, CHANGELOG.md
  is included into the docs site rather than duplicated, and the landing page
  explains what Bold is for

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
