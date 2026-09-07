# Installation

## Prerequisites

- **Delphi 11.3, 12.3, or 13**
- **Database**: SQLite (easiest), SQL Server, PostgreSQL, Firebird, MariaDB/MySQL, or Oracle
- **Git** (for cloning the repository)

## Installing with a Package Manager (Recommended)

Both package managers download Bold's source, build the design-time package
`dclBold` for your Delphi version and register it, so Steps 1 and 2 below are
not needed. Bold is available for Delphi 11, 12 and 13, Win32.

### TMS Smart Setup

1. Get Smart Setup (free) from
   [doc.tmssoftware.com/smartsetup/download](https://doc.tmssoftware.com/smartsetup/download):
   unzip the portable `tmssmartsetup.zip` or run the installer; both give you `tms.exe`
   and the GUI `tmsgui.exe`.
2. Run:

   ```shell
   tms install bero.boldfordelphi
   ```

   Smart Setup clones the latest release tag, builds `dclBold` for every installed
   Delphi it supports, registers the package in the IDE and puts Bold's compiled
   units on the library path (the sources go on the browsing path). Later
   `tms update` moves to newer releases; `tms install bero.boldfordelphi:26.9.0`
   pins a version.

### DPM

1. Get DPM (free) from
   [github.com/DelphiPackageManager/DPM/releases](https://github.com/DelphiPackageManager/DPM/releases).
   The installer adds `dpm.exe` to the PATH and an IDE plugin.
2. DPM installs per project. In the IDE, right-click the project in the Projects
   pane and choose **Manage DPM Packages**, then install `bero.BoldForDelphi`; or
   from the command line:

   ```shell
   dpm install bero.BoldForDelphi C:\path\to\YourProject.dproj -compiler=13.0 -platforms=Win32
   ```

   DPM builds the design package into its package cache once, loads it in the IDE
   while the project is open, and adds Bold's compiled units to that project's
   search path (`$(DPMSearch)`). Nothing is registered IDE-wide. The package is
   published on the public feed at [delphi.dev](https://delphi.dev), so no source
   configuration is needed.

## Step 1: Get the Source

```bash
git clone https://github.com/bero/BoldForDelphi.git
```

Or download and extract to a folder like `C:\BoldForDelphi`.

## Step 2: Install the Packages

### Option A: Download Pre-built Binaries

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
| Delphi 11.3 Alexandria | `packages\Delphi11.3\dclBold.dpk` |
| Delphi 12.3 Athens | `packages\Delphi12.3\dclBold.dpk` |
| Delphi 13 Florence | `packages\Delphi13\dclBold.dpk` |

2. Build the package (Shift+F9)

3. Right-click the BPL file in the Projects pane (Project Manager) and choose **Install**

4. Verify via **Component → Install Packages...**

### Using an Unsupported Delphi Version

If your Delphi version is not listed:

1. Copy the folder of the closest supported version (e.g., copy `packages\Delphi13` for Delphi 14)
2. Rename the folder after the Delphi version (e.g., `packages\Delphi14`)
3. Build and install; the package uses `LIBSUFFIX AUTO`, so the produced file is named
   after the compiler automatically (`dclBold380.bpl` for Delphi 14) and no version setting needs editing

!!! tip
    When you verify everything works, please submit a pull request to include the new package in the repository!

## Verify Installation

After installation, you should see Bold components in the Delphi Tool Palette:

- **Bold Handles** - [TBoldSystemHandle](../classes/TBoldSystemHandle.md), [TBoldListHandle](../classes/TBoldListHandle.md), [TBoldExpressionHandle](../classes/TBoldExpressionHandle.md)
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
