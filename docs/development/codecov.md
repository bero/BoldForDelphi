# Codecov.io Integration

Codecov.io integration for tracking code coverage over time.

**Dashboard**: [https://app.codecov.io/gh/bero/BoldForDelphi](https://app.codecov.io/gh/bero/BoldForDelphi)

## Usage

### Upload Coverage Manually

```powershell
# Set token (once per session)
$env:CODECOV_TOKEN = "your-token-here"

# Run tests with coverage and upload
.\UnitTest\run_coverage.ps1 -Upload
```

The other options of `run_coverage.ps1`, and what it needs on the machine, are in
[UnitTest/README.md](https://github.com/bero/BoldForDelphi/blob/develop/UnitTest/README.md#code-coverage).

## Files

| File | Purpose |
|------|---------|
| `UnitTest/Convert-ToCodecovJson.ps1` | Converts HTML coverage to Codecov JSON format |
| `UnitTest/codecov.json` | Output file for upload (gitignored) |
| `UnitTest/codecov.exe` | Codecov uploader (downloaded automatically) |

## How It Works

```
run_coverage.ps1
      │
      ▼
┌─────────────────────┐
│ Build UnitTest.exe  │
│ with MAP file       │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ DelphiCodeCoverage  │
│ runs tests          │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐     ┌─────────────────────────┐
│ HTML coverage       │────▶│ Convert-ToCodecovJson   │
│ reports (per file)  │     │ .ps1                    │
└─────────────────────┘     └───────────┬─────────────┘
                                        │
                                        ▼
                            ┌─────────────────────────┐
                            │ codecov.json            │
                            │ (line-level coverage)   │
                            └───────────┬─────────────┘
                                        │
                                        ▼
                            ┌─────────────────────────┐
                            │ codecov.exe upload      │
                            └───────────┬─────────────┘
                                        │
                                        ▼
                            ┌─────────────────────────┐
                            │ Codecov.io              │
                            │ - Dashboard             │
                            │ - Trend graphs          │
                            │ - PR comments           │
                            │ - Badge                 │
                            └─────────────────────────┘
```

## Token Security

- Never commit the token to git
- Use environment variable `CODECOV_TOKEN`
- For GitHub Actions, use repository secrets

## Download Codecov Uploader

The uploader is downloaded automatically, or manually:

```powershell
Invoke-WebRequest -Uri "https://uploader.codecov.io/latest/windows/codecov.exe" -OutFile codecov.exe
```

## Troubleshooting

### "Unusable report" error

The Codecov JSON format requires line-level data. If you see this error:

1. Ensure coverage HTML reports exist in `UnitTest/coverage_report/`
2. Check that source files match paths in the git repository

### Decimal separators

The converter writes numbers the same way whatever the Windows locale, so a comma as decimal separator does not
break the JSON.

## See Also

- [Unit Testing](testing.md): writing tests
- [UnitTest/README.md](https://github.com/bero/BoldForDelphi/blob/develop/UnitTest/README.md): running the tests and
  the coverage locally
