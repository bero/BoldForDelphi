# Unit Testing

This document describes how to set up, run, and develop unit tests for Bold for Delphi.

## Overview

Bold for Delphi uses the **DUnitX** testing framework. Tests are located in the `UnitTest/` folder:

```
UnitTest/
├── UnitTest.dpr           # Main test project
├── UnitTest.dproj
├── run_coverage.ps1       # Coverage script
├── Code/
│   ├── Common/            # Tests for Common units
│   ├── ObjectSpace/       # Tests for ObjectSpace units
│   └── Main/              # Test model classes
└── coverage_report/       # Generated coverage reports
```

## Prerequisites

### DUnitX Environment Variable

The `$(DUNITX)` system environment variable must be set to the DUnitX installation folder:

- **Windows**: System Properties > Environment Variables
- **Delphi IDE**: Tools > Options > IDE > Environment Variables

Example: `DUNITX=C:\DUnitX`

### Delphi-Mocks Environment Variable

The `$(DelphiMocks)` system environment variable must be set to the Delphi-Mocks installation folder:

Example: `DelphiMocks=C:\Delphi-Mocks\Source`

## Mocking with Delphi-Mocks

Bold uses the **Delphi-Mocks** framework to create mock objects for unit testing. This is particularly valuable for testing database operations without requiring an actual database connection.

### Why Mock Database Operations?

Testing Bold persistence code against a real database has several problems:

| Problem | Impact |
|---------|--------|
| **Slow tests** | Database connections and queries add significant time |
| **Test isolation** | Tests can interfere with each other through shared data |
| **Setup complexity** | Need to create/maintain test databases and data |
| **Error simulation** | Hard to test error handling (connection failures, deadlocks) |
| **CI/CD environments** | Build servers may not have database access |

Mocking solves these by replacing database interfaces with controlled test doubles.

### Bold Database Interfaces

Bold uses interfaces for database abstraction, making them ideal for mocking:

- `IBoldDatabase` - Database connection and transactions
- `IBoldQuery` - SQL query execution
- `IBoldField` - Field value access

### Example: Mocking a Database Connection

```pascal
uses
  Delphi.Mocks,
  BoldDBInterfaces;

procedure TMyTest.TestWithMockedDatabase;
var
  MockDb: TMock<IBoldDatabase>;
  MockQuery: TMock<IBoldQuery>;
begin
  // Create mocks
  MockDb := TMock<IBoldDatabase>.Create;
  MockQuery := TMock<IBoldQuery>.Create;

  // Setup: Database returns our mock query
  MockDb.Setup
    .WillReturn(TValue.From<IBoldQuery>(MockQuery.Instance))
    .When.GetQuery;

  // Setup: Simulate connected state
  MockDb.Setup.WillReturn(True).When.GetConnected;

  // Setup: Query returns expected data
  MockQuery.Setup.WillReturn(42).When.FieldByName('Id').AsInteger;

  // Test code using MockDb.Instance as IBoldDatabase
  var Db: IBoldDatabase := MockDb.Instance;
  Assert.IsTrue(Db.Connected);

  var Query := Db.GetQuery;
  Assert.AreEqual(42, Query.FieldByName('Id').AsInteger);
end;
```

### Testing Error Conditions

Mocks excel at simulating errors that are hard to reproduce with real databases:

```pascal
procedure TMyTest.TestConnectionFailure;
var
  MockDb: TMock<IBoldDatabase>;
begin
  MockDb := TMock<IBoldDatabase>.Create;

  // Simulate connection failure
  MockDb.Setup.WillReturn(False).When.GetConnected;
  MockDb.Setup
    .WillRaise(EBoldDatabaseError, 'Connection timeout')
    .When.StartTransaction;

  // Test that your code handles the error correctly
  Assert.WillRaise(
    procedure begin
      SomeCodeThatNeedsDatabase(MockDb.Instance);
    end,
    EBoldDatabaseError
  );
end;
```

### Verifying Expectations

Mocks can verify that expected methods were called:

```pascal
procedure TMyTest.TestTransactionCommitted;
var
  MockDb: TMock<IBoldDatabase>;
begin
  MockDb := TMock<IBoldDatabase>.Create;

  // Set expectations
  MockDb.Setup.Expect.Once.When.StartTransaction;
  MockDb.Setup.Expect.Once.When.Commit;
  MockDb.Setup.Expect.Never.When.Rollback;

  // Run code under test
  PerformDatabaseOperation(MockDb.Instance);

  // Verify expectations were met
  MockDb.Verify;  // Fails if expectations not satisfied
end;
```

### Mock Test Location

Mock-based tests are in `UnitTest/Code/Mocks/`. See `Test.BoldDBInterfacesMock.pas` for complete examples.

## Running Tests

### Command Line

```batch
cd UnitTest
UnitTest.exe --exit:Continue
```

Use `--exit:Pause` to pause before exit:

```batch
UnitTest.exe --exit:Pause
```

### From Delphi IDE

1. Open `UnitTest.dproj` in Delphi
2. Build (Shift+F9)
3. Run (F9)

## TestInsight

**TestInsight** is a Delphi IDE plugin that enables running and debugging individual tests directly from the IDE.

### Installing TestInsight

1. Download from: [https://bitbucket.org/sglienke/testinsight/downloads/](https://bitbucket.org/sglienke/testinsight/downloads/)
2. Run the installer
3. Restart Delphi IDE
4. TestInsight panel appears in **View > TestInsight Explorer**

### Using TestInsight

1. Open `UnitTest.dproj` in Delphi
2. Open **View > TestInsight Explorer**
3. Build the project (Shift+F9)
4. Tests appear in the TestInsight panel
5. **Run single test**: Click the green arrow next to a test
6. **Debug test**: Right-click > Debug Selected Tests
7. **Run all**: Click Run All in the toolbar

### TestInsight Features

- Run/debug individual tests without running the entire suite
- See test results inline in the IDE
- Double-click failures to jump to the failing line
- Filter tests by name or status
- Re-run failed tests only

### TESTINSIGHT Conditional Define

| TESTINSIGHT | Behavior |
|-------------|----------|
| **Defined** | Tests run via TestInsight plugin only |
| **Undefined** | Tests run as console application |

Toggle via: Right-click project > **Enable TestInsight** (or Disable)

## Writing Tests

### Test Structure

```pascal
unit Test.MyUnit;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestMyUnit = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSomething;
  end;

implementation

procedure TTestMyUnit.Setup;
begin
  // Initialize before each test
end;

procedure TTestMyUnit.TearDown;
begin
  // Cleanup after each test
end;

procedure TTestMyUnit.TestSomething;
begin
  Assert.AreEqual(Expected, Actual, 'Description');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMyUnit);

end.
```

### Adding Tests to the Project

1. Create test file in appropriate `Code/` subfolder
2. Add to `UnitTest.dpr` uses clause:

```pascal
Test.MyUnit in 'Code\SubFolder\Test.MyUnit.pas';
```

### Common Assertions

```pascal
Assert.AreEqual(Expected, Actual);
Assert.IsTrue(Condition);
Assert.IsFalse(Condition);
Assert.IsNull(Value);
Assert.IsNotNull(Value);
Assert.WillRaise(procedure begin ... end, EExpectedException);
```

## Code Coverage

Code coverage analysis helps identify untested code paths. Bold uses **DelphiCodeCoverage**.

### Running Coverage

```powershell
cd UnitTest
.\run_coverage.ps1
```

Options:

| Flag | Description |
|------|-------------|
| `-SkipBuild` | Skip compilation, use existing executable |
| `-OpenReport` | Open HTML report in browser |
| `-Upload` | Upload to Codecov.io |

### Viewing Results

Open `coverage_report\CodeCoverage_summary.html` in a browser.

- **Green lines** - Executed during tests
- **Red lines** - Not executed during tests

### Tips for Improving Coverage

1. **Focus on uncovered lines** - Check the report and target specific uncovered code paths
2. **Test edge cases** - Error handling, boundary conditions, exception paths
3. **Use persistence tests** - Many code paths require database operations
4. **Test different configurations** - Transient vs persistent mode

## See Also

- [Codecov Setup](codecov.md) - Upload coverage to Codecov.io
- [Contributing](../contributing.md) - Contribution guidelines
- [DUnitX](https://github.com/VSoftTechnologies/DUnitX) - Unit testing framework
- [Delphi-Mocks](https://github.com/VSoftTechnologies/Delphi-Mocks) - Mocking framework
