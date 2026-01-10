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

### Flags

| Flag | Description |
|------|-------------|
| `-SkipBuild` | Skip compilation, use existing UnitTest.exe |
| `-OpenReport` | Open HTML report in browser after completion |
| `-Upload` | Upload coverage to Codecov.io (requires CODECOV_TOKEN) |

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

### Decimal separator issues

The converter uses proper number formatting regardless of system locale.

## See Also

- [Unit Testing](testing.md) - Running tests and coverage locally
