# Installation

## Prerequisites

- **Delphi 11.3, 12.3, or 13**, or **Lazarus 4** with Free Pascal 3.2.2 (see
  [Lazarus and Free Pascal](#lazarus-and-free-pascal))
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
   `tms update` moves to newer releases; `tms install bero.boldfordelphi:26.9.1`
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

## Lazarus and Free Pascal

Bold's Lazarus packages are in `packages\Lazarus`; its
[README](https://github.com/bero/BoldForDelphi/blob/develop/packages/Lazarus/README.md)
says what each package holds. They need Lazarus 4 with Free Pascal 3.2.2 or later; the CI
builds them with Lazarus 4.4 on Linux, and the steps below install them in Lazarus 4.8 on
64-bit Windows. The design-time package puts Bold's non-visual components on the palette
and has the OCL editor; Bold's visual controls and the model editor are Delphi only.

1. **Install Lazarus.** On Windows, the installer from
   [lazarus-ide.org](https://www.lazarus-ide.org/index.php?page=downloads) includes Free
   Pascal and the Lazarus source, which the rebuild in step 5 compiles. With winget:

    ```shell
    winget install --id Lazarus.Lazarus --exact
    ```

    Add `--version 4.4` for the version the CI uses. On Debian and Ubuntu, install
    `lazarus-src` as well: the design-time package needs the source of `BuildIntf`, which
    those distributions ship as source only.

2. **English menus (optional).** Lazarus starts in the language of the operating system.
   In **Tools → Options → Environment → General → Language**, choose **English [en]**, not
   **Automatic (or English)**, which follows the system, and restart Lazarus. The steps
   below use the English names.

3. **Get the source**, as in [Step 1](#step-1-get-the-source).

4. **Open the design-time package.** Choose **Package → Open Package File (.lpk) ...** and
   open `packages\Lazarus\dclBold.lpk`. Lazarus finds the two runtime packages it requires,
   `Bold.lpk` and `BoldLCL.lpk`, in the same folder.

5. **Install it.** In the package window, choose **Use → Install**. Lazarus asks three
   questions:

    1. **Automatically installed packages**: installing `dclBold` installs `Bold` and
       `BoldLCL` too. Choose **OK**.
    2. **Suspicious include path**: "The package Bold adds the path
       `..\..\Source\Common\Include` to the include path of the IDE". Choose **Continue**,
       or **Continue and do not ask again**. The path is intended: a project that requires
       Bold finds `bold.inc` through it, and the folder holds only Bold's include files.
       Lazarus asks this about every package that passes an include path on.
    3. **Rebuild Lazarus?**: choose **Yes**. Lazarus compiles itself with the three packages
       and restarts.

6. **Check the palette.** It has three new pages: **Bold Handles** (`TBoldModel`,
   `TBoldSystemHandle`, the list and expression handles and the others), **Bold
   Persistence** (`TBoldPersistenceHandleDB`, `TBoldPersistenceHandleFileXML`) and **Bold
   Misc** (`TBoldDataSet` among others). The "..." button of an `Expression` property in
   the Object Inspector opens the OCL editor.

A project requires `Bold`, or `BoldLCL` for an LCL application, and never `dclBold`, so it
links no registration code. The Zeos database adapter has its own design-time package,
`dclBoldZeos.lpk`, which is installed the same way once Lazarus knows Zeos's packages (see
the README).

The command-line builds of the repository, the `build.sh` scripts of the Free Pascal tests,
the Lazarus packages' checks and the Invoicing examples, also run in Git Bash on Windows;
they find Lazarus in `C:\lazarus`, or in the folder that `LAZARUS_DIR` names.

## Verify Installation

After installation, you should see Bold components in the Delphi Tool Palette:

- **Bold Handles** - [TBoldSystemHandle](../classes/TBoldSystemHandle.md), [TBoldListHandle](../classes/TBoldListHandle.md), [TBoldExpressionHandle](../classes/TBoldExpressionHandle.md)
- **Bold Controls** - TBoldGrid, TBoldEdit, TBoldComboBox, TBoldNavigator
- **Bold Persistence** - TBoldPersistenceHandleDB, TBoldDatabaseAdapterFireDAC
- **Bold Actions** - TBoldActivateSystemAction, TBoldUpdateDBAction

## Troubleshooting

### "bold.inc not found"

Add `Source\Common\Include` to your project's search path.

### Components not showing in palette

- Verify the BPL is installed: **Component → Install Packages...**
- Check that all dependent packages are loaded

### Package won't compile

- Ensure you're using the correct package for your Delphi version
- Check that no older Bold packages are installed
