# Installation

## Prerequisites

- Delphi 11.3 Alexandria, 12.x Athens, or 13+
- Git (for cloning the repository)

## Clone the Repository

```bash
git clone https://github.com/ArtDabbler/BoldForDelphi.git
```

## Build the Packages

Bold uses relative paths, so no environment variables are required for basic setup.

### Using PowerShell Build Script

```powershell
# Build for Delphi 12.3
C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1 `
  -Projectfile "packages\Delphi29.3\dclBold.dproj" `
  -DelphiVersion "23.0" `
  -VerboseOutPut
```

### Manual Build

1. Open Delphi IDE
2. Open the package group for your Delphi version:
   - Delphi 11.3: `packages\Delphi28\`
   - Delphi 12.1: `packages\Delphi29.1\`
   - Delphi 12.3: `packages\Delphi29.3\`
3. Build and install `dclBold.dpk`

## Package Structure

| Package | Type | Purpose |
|---------|------|---------|
| `dclBold.dpk` | Design-time | Core Bold components |
| `dclBoldUniDAC.dpk` | Design-time | UniDAC database adapter |
| `dclBoldDevEx.dpk` | Design-time | DevExpress integration |

## Optional: UniDAC Support

For UniDAC database adapter support, set the `UniDAC` environment variable:

1. In Delphi: **Tools > Options > Environment Variables**
2. Add `UniDAC` pointing to your UniDAC installation root

## Verify Installation

After installation, you should see Bold components in the Delphi component palette:

- **Bold Handles** - TBoldSystemHandle, TBoldListHandle, etc.
- **Bold Controls** - TBoldGrid, TBoldEdit, TBoldComboBox, etc.
- **Bold Persistence** - TBoldPersistenceHandleDB, etc.
