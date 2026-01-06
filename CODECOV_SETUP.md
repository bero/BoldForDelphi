# Codecov.io Integration for Bold for Delphi

## Status: Complete

Codecov.io integration is fully working. Coverage data is uploaded in Codecov's native JSON format with line-level detail.

**Dashboard**: https://app.codecov.io/gh/bero/BoldForDelphi

## Usage

### Upload coverage manually

```powershell
# Set token (once per session)
$env:CODECOV_TOKEN = "your-token-here"

# Run tests with coverage and upload
powershell -ExecutionPolicy Bypass -File "C:\Attracs\Run_coverage.ps1" -Upload
```

### Flags

| Flag | Description |
|------|-------------|
| `-SkipBuild` | Skip compilation, use existing UnitTest.exe |
| `-OpenReport` | Open HTML report in browser after completion |
| `-Upload` | Upload coverage to Codecov.io (requires CODECOV_TOKEN) |

## Files

| File | Purpose |
|------|---------|
| `UnitTest/Convert-ToCodecovJson.ps1` | Parses HTML coverage reports to Codecov JSON format |
| `UnitTest/codecov.json` | Output file for Codecov upload (gitignored) |
| `UnitTest/codecov.exe` | Codecov uploader (downloaded automatically, gitignored) |

## How It Works

```
Run_coverage.ps1
      |
      v
+---------------------+
| Build UnitTest.exe  |
| with MAP file       |
+----------+----------+
           |
           v
+---------------------+
| DelphiCodeCoverage  |
| runs tests          |
+----------+----------+
           |
           v
+---------------------+     +-------------------------+
| HTML coverage       |---->| Convert-ToCodecovJson   |
| reports (per file)  |     | .ps1                    |
+---------------------+     +------------+------------+
                                         |
                                         v
                            +-------------------------+
                            | codecov.json            |
                            | (line-level coverage)   |
                            +------------+------------+
                                         |
                                         v
                            +-------------------------+
                            | codecov.exe upload      |
                            +------------+------------+
                                         |
                                         v
                            +-------------------------+
                            | Codecov.io              |
                            | - Dashboard             |
                            | - Trend graphs          |
                            | - PR comments           |
                            | - Badge                 |
                            +-------------------------+
```

## Token Security

- Never commit the token to git
- Use environment variable `CODECOV_TOKEN`
- For GitHub Actions, use repository secrets

## Troubleshooting

### "Unusable report" error
The Codecov JSON format requires line-level data. The converter parses HTML reports which contain this detail. If you see this error:
1. Ensure coverage HTML reports exist in `UnitTest/coverage_report/`
2. Check that source files in the report match paths in the git repository

### Decimal separator issues
The converter uses proper number formatting regardless of system locale.
