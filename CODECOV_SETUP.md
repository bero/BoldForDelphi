# Codecov.io Setup Guide for Bold for Delphi

## Current Status

- [x] DelphiCodeCoverage running and generating XML output
- [x] Cobertura converter created: `UnitTest\Convert-ToCobertura.ps1`
- [x] Converter tested successfully (40.32% coverage, 22944/56901 lines)
- [ ] Codecov.io account setup
- [ ] Repository token configured
- [ ] Upload integrated into coverage script
- [ ] README badge added

## Files Created

| File | Purpose |
|------|---------|
| `UnitTest\Convert-ToCobertura.ps1` | Converts DelphiCodeCoverage XML to Cobertura format |
| `UnitTest\cobertura.xml` | Output file for Codecov upload |

## Next Steps

### Step 1: Create Codecov Account

1. Go to https://codecov.io
2. Click "Sign up with GitHub"
3. Authorize Codecov to access your repositories
4. Select the `BoldForDelphi` repository

### Step 2: Get Repository Token

1. In Codecov dashboard, go to your repository settings
2. Copy the "Repository Upload Token"
3. Save it securely (you'll need it for uploads)

### Step 3: Test Manual Upload

```powershell
cd C:\Attracs\BoldForDelphi\UnitTest

# Generate coverage
..\Run_coverage.ps1

# Convert to Cobertura
.\Convert-ToCobertura.ps1

# Download Codecov uploader
Invoke-WebRequest -Uri "https://uploader.codecov.io/latest/windows/codecov.exe" -OutFile "codecov.exe"

# Upload (replace YOUR_TOKEN with actual token)
.\codecov.exe -t YOUR_TOKEN -f cobertura.xml -r owner/BoldForDelphi
```

### Step 4: Integrate into Run_coverage.ps1

Add this to the end of `C:\Attracs\Run_coverage.ps1` (before the `finally` block):

```powershell
# Optional: Upload to Codecov
if ($env:CODECOV_TOKEN) {
    Write-Host "[CODECOV] Converting and uploading coverage..." -ForegroundColor Yellow

    # Convert to Cobertura format
    & "$ProjectDir\Convert-ToCobertura.ps1" -OutputFile "$ProjectDir\cobertura.xml"

    # Upload to Codecov
    $codecovExe = "$ProjectDir\codecov.exe"
    if (-not (Test-Path $codecovExe)) {
        Invoke-WebRequest -Uri "https://uploader.codecov.io/latest/windows/codecov.exe" -OutFile $codecovExe
    }
    & $codecovExe -t $env:CODECOV_TOKEN -f "$ProjectDir\cobertura.xml"
}
```

Then run with:
```powershell
$env:CODECOV_TOKEN = "your-token-here"
.\Run_coverage.ps1
```

### Step 5: Add Badge to README

Add to `README.md`:

```markdown
[![codecov](https://codecov.io/gh/OWNER/BoldForDelphi/branch/develop/graph/badge.svg?token=YOUR_TOKEN)](https://codecov.io/gh/OWNER/BoldForDelphi)
```

Replace `OWNER` with the GitHub username/org and `YOUR_TOKEN` with the badge token from Codecov.

## Coverage Flow Diagram

```
Run_coverage.ps1
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
┌─────────────────────┐     ┌─────────────────────┐
│ CodeCoverage_       │────▶│ Convert-ToCobertura │
│ Summary.xml         │     │ .ps1                │
└─────────────────────┘     └──────────┬──────────┘
                                       │
                                       ▼
                            ┌─────────────────────┐
                            │ cobertura.xml       │
                            └──────────┬──────────┘
                                       │
                                       ▼
                            ┌─────────────────────┐
                            │ codecov.exe upload  │
                            └──────────┬──────────┘
                                       │
                                       ▼
                            ┌─────────────────────┐
                            │ Codecov.io          │
                            │ - Dashboard         │
                            │ - Trend graphs      │
                            │ - PR comments       │
                            │ - Badge             │
                            └─────────────────────┘
```

## Troubleshooting

### Decimal separator issues
The converter uses `InvariantCulture` to ensure decimals use dots (0.40) not commas (0,40).

### Large XML file
The CodeCoverage_Summary.xml is ~2.8MB with all method-level detail. This is normal.

### Token security
- Never commit the token to git
- Use environment variable `CODECOV_TOKEN`
- For GitHub Actions, use repository secrets
