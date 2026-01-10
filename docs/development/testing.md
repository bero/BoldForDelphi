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
