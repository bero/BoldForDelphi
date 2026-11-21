# Unit Testing

Bold has one test suite that runs under two compilers: Delphi runs it with DUnitX, Free Pascal with FPCUnit. This page
is about writing tests. How to install the test frameworks, build the runners and run the suite (on Windows, on
Linux, against several database engines, under memory checks and with code coverage) is in
[UnitTest/README.md](https://github.com/bero/BoldForDelphi/blob/develop/UnitTest/README.md), next to the scripts.

## Layout

```
UnitTest/
├── UnitTest.dpr / .dproj      # Delphi console runner (DUnitX)
├── UnitTestGUI.dpr / .dproj   # Delphi GUI runner (DUnitX VCL logger)
├── FPC/UnitTestFPC.lpr        # Free Pascal runner (FPCUnit), built by FPC/build.sh
├── Code/
│   ├── Common/                # BoldTestFramework.pas and tests of Source/Common
│   ├── ObjectSpace/           # tests of the object space, OCL and attributes
│   ├── Persistence/           # tests that need a database; one base per adapter
│   ├── Mocks/                 # Delphi-Mocks tests of the database interfaces
│   └── ...                    # one folder per area of Source/
├── UnitTest.ini               # the database engine for the persistence tests
└── build.ps1, run_matrix.ps1, run_coverage.ps1, check_memory.ps1, check_no_vcl.ps1
```

## Writing a test

A test unit uses `BoldTestFramework` instead of DUnitX or FPCUnit directly. That unit is the only one that differs
between the two compilers, so the same test unit compiles for both:

```pascal
{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit Test.BoldSomething;

interface

uses
  SysUtils,
  BoldTestFramework;

type
  TTestBoldSomething = class(TBoldTestCase)
  protected
    procedure SetUp; override;      // optional; also TearDown, SetUpOnce, TearDownOnce
    procedure TearDown; override;
  published
    procedure TestTheValueIsKept;   // every published method is a test
  end;

implementation

procedure TTestBoldSomething.SetUp;
begin
  inherited;
  // runs before each test
end;

procedure TTestBoldSomething.TearDown;
begin
  // runs after each test
  inherited;
end;

procedure TTestBoldSomething.TestTheValueIsKept;
begin
  Assert.AreEqual(42, 6 * 7, 'the product');
end;

initialization
  BoldRegisterTest(TTestBoldSomething);

end.
```

Rules that keep a test portable:

- Every published method is a test. Put helpers and hooks in a `private` or `protected` section, never under
  `published`.
- `Assert` has the DUnitX method names on both compilers (`AreEqual`, `IsTrue`, `IsNull`, `Contains`,
  `WillRaise`, ...). `Assert.WillRaise` takes a method of the test class, not an anonymous method.
- Free an object that the test also uses through an interface with `FreeAfterTest(Obj)` instead of `Obj.Free`:
  Free Pascal keeps the result of an `as` cast in a hidden temporary until the end of the routine.
- Under Free Pascal each test runs on its own instance, so `SetUpOnce` and `TearDownOnce` must keep fixture-wide
  state in globals or class variables, not in fields.
- A test of something that exists only under one compiler or platform goes under `{$IFDEF FPC}`, `{$IFDEF MSWINDOWS}`
  and so on, in both the declaration and the implementation.

### Adding the unit to the runners

1. Put the unit in the `Code/` folder of the area it tests.
2. Add it to the `uses` clause of `UnitTest.dpr` and to `UnitTest.dproj`.
3. Run `sync_gui_dpr.ps1`, which copies the unit list to `UnitTestGUI.dpr`.
4. Add it to the `uses` clause of `FPC/UnitTestFPC.lpr`. `FPC/build.sh` finds every folder under `Code/` by itself.

### Tests that need a database

The persistence tests derive from the test base of their adapter (FireDAC under Delphi, Zeos under Free Pascal,
UniDAC in the `DebugUniDAC` build) and get the engine that `UnitTest.ini` or the `BOLD_TEST_ENGINE` environment
variable selects; SQLite is the default. Look at an existing unit in `Code/Persistence` before writing a new one.

## Mocking database interfaces

Under Delphi on Windows, the database interfaces (`IBoldDatabase`, `IBoldQuery`, `IBoldField`, in `BoldDBInterfaces`)
can be replaced by [Delphi-Mocks](https://github.com/VSoftTechnologies/Delphi-Mocks) test doubles. That makes it
easy to test error handling, such as a failed connection or a failing commit, without a database. Delphi-Mocks does
not compile for Linux64 or with Free Pascal, so these tests are Windows only. They are in
`UnitTest/Code/Mocks/Test.BoldDBInterfacesMock.pas`.

A query that returns a field:

```pascal
var
  MockQuery: TMock<IBoldQuery>;
  MockField: TMock<IBoldField>;
begin
  MockQuery := TMock<IBoldQuery>.Create;
  MockField := TMock<IBoldField>.Create;

  MockField.Setup.WillReturn(42).When.GetAsInteger;
  MockQuery.Setup.WillReturn(TValue.From<IBoldField>(MockField.Instance))
    .When.FieldByName(It0.IsAny<string>);

  Assert.AreEqual(42, MockQuery.Instance.FieldByName('Id').AsInteger);
end;
```

Expectations on calls, checked by `Verify`:

```pascal
var
  MockDb: TMock<IBoldDatabase>;
begin
  MockDb := TMock<IBoldDatabase>.Create;
  MockDb.Setup.Expect.Once.When.StartTransaction;
  MockDb.Setup.Expect.Once.When.Commit;
  MockDb.Setup.Expect.Never.When.RollBack;

  PerformDatabaseOperation(MockDb.Instance);   // the code under test

  MockDb.Verify('one transaction, committed');
end;
```

## TestInsight

[TestInsight](https://bitbucket.org/sglienke/testinsight/downloads/) is a Delphi IDE plugin that runs and debugs
single tests from the IDE. Open `UnitTest.dproj`, enable TestInsight for the project in its context menu in the
Projects pane, which defines `TESTINSIGHT`, build, and run the tests from **View → TestInsight Explorer**. With
`TESTINSIGHT` defined, the runner (`Code/Main/TestRunner.pas`) reports to TestInsight instead of the console.

## Code coverage

`run_coverage.ps1` runs the Delphi suite under
[DelphiCodeCoverage](https://github.com/DelphiCodeCoverage/DelphiCodeCoverage) and writes an HTML report to
`UnitTest/coverage_report/`; [UnitTest/README.md](https://github.com/bero/BoldForDelphi/blob/develop/UnitTest/README.md#code-coverage)
says how to run it. [Codecov Setup](codecov.md) describes the upload to Codecov.io.

When you add tests for coverage, target the lines the report shows as not covered, usually error paths and edge
cases, instead of adding more tests of code that is already covered.
