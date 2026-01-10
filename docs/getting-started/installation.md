# Installation

## Prerequisites

- **Delphi 11.3, 12.1 CE, 12.3, or 13**
- **Database**: SQLite (easiest), SQL Server, PostgreSQL, Firebird, MariaDB/MySQL, or Oracle
- **Git** (for cloning the repository)

## Step 1: Get the Source

```bash
git clone https://github.com/bero/BoldForDelphi.git
```

Or download and extract to a folder like `C:\BoldForDelphi`.

## Step 2: Install the Packages

### Option A: Download Pre-built Binaries (Recommended)

1. Download the binary package for your Delphi version from:
   [https://github.com/bero/BoldForDelphi/releases/](https://github.com/bero/BoldForDelphi/releases/)

2. Extract to `packages\Bin\`

3. In Delphi: **Component → Install Packages...**

4. Click **Add** and select the BPL file for your Delphi version

### Option B: Build from Source

Building from source gives you the latest version or lets you use Bold with unsupported Delphi versions.

1. Open the package file for your Delphi version:

| Delphi Version | Package Path |
|----------------|--------------|
| Delphi 11.3 | `packages\Delphi28\dclBold.dpk` |
| Delphi 12.1 | `packages\Delphi29.1\dclBold.dpk` |
| Delphi 12.3 | `packages\Delphi29.3\dclBold.dpk` |
| Delphi 13 | `packages\Delphi30\dclBold.dpk` |

2. Build the package (Shift+F9)

3. Right-click the BPL file in the Project Manager and choose **Install**

4. Verify via **Component → Install Packages...**

### Using an Unsupported Delphi Version

If your Delphi version is not listed:

1. Copy the folder of the closest supported version (e.g., copy `Delphi30` for Delphi 14)
2. Rename the folder to match the compiler version (e.g., `Delphi31`)
3. Open Project Options and update the Lib version in Description
4. Build and install

!!! tip
    When you verify everything works, please submit a pull request to include the new package in the repository!

## Verify Installation

After installation, you should see Bold components in the Delphi Tool Palette:

- **Bold Handles** - TBoldSystemHandle, TBoldListHandle, TBoldExpressionHandle
- **Bold Controls** - TBoldGrid, TBoldEdit, TBoldComboBox, TBoldNavigator
- **Bold Persistence** - TBoldPersistenceHandleDB, TBoldDatabaseAdapterFireDAC
- **Bold Actions** - TBoldActivateSystemAction, TBoldUpdateDBAction

## Troubleshooting

### "Bold.inc not found"

Add `Source\Common\Include` to your project's search path.

### Components not showing in palette

- Verify the BPL is installed: **Component → Install Packages...**
- Check that all dependent packages are loaded

### Package won't compile

- Ensure you're using the correct package for your Delphi version
- Check that no older Bold packages are installed
