# Installation

This page installs Bold in the Delphi IDE or in Lazarus. The quickest route in Delphi is a package manager; the
manual route builds the design-time package from source.

## Requirements

<!-- The platform table is maintained once, in README.md, and included here. Edit it there. -->

--8<-- "README.md:platforms"

- **Delphi**: one package folder per compiler. `packages\Delphi13` serves 13.0 and 13.1 (Community Edition
  included), `packages\Delphi12.3` serves every 12.x, and `packages\Delphi11.3` is for 11.3.
- **Lazarus**: Lazarus 4 with Free Pascal 3.2.2 or later. SQL databases are reached through
  [Zeos](https://sourceforge.net/projects/zeoslib/) 8.0.
- **A database**: SQLite needs no server and is the easiest start. The test suite also runs on SQL Server and
  PostgreSQL; Firebird, Oracle and MariaDB/MySQL have SQL dialects too, but less testing. See
  [Persistence](../concepts/persistence.md).
- **Git**, to clone the repository (or download the source as a ZIP from GitHub).

## Delphi: with a package manager (recommended)

Both package managers download Bold's source, build the design-time package `dclBold` for your Delphi version and
register it, so the manual steps below are not needed.

### TMS Smart Setup

1. Get Smart Setup (free) from
   [doc.tmssoftware.com/smartsetup/download](https://doc.tmssoftware.com/smartsetup/download): unzip the portable
   `tmssmartsetup.zip` or run the installer. Both give you `tms.exe` and the GUI `tmsgui.exe`.
2. Run:

    ```shell
    tms install bero.boldfordelphi
    ```

    Smart Setup clones the latest release tag, builds `dclBold` for every installed Delphi version it supports
    (11, 12 and 13, for the 32-bit IDE), registers the package in the IDE and puts Bold's compiled units on the
    library path (the sources go on the browsing path). `tms update` moves to newer releases;
    `tms install bero.boldfordelphi:26.9.1` pins a version.

### DPM

1. Get DPM (free) from
   [github.com/DelphiPackageManager/DPM/releases](https://github.com/DelphiPackageManager/DPM/releases). The
   installer adds `dpm.exe` to the PATH and an IDE plugin.
2. DPM installs per project. In the IDE, right-click the project in the Projects pane, choose
   **Manage DPM Packages** and install `bero.BoldForDelphi`. From the command line:

    ```shell
    dpm install bero.BoldForDelphi C:\path\to\YourProject.dproj -compiler=13.0 -platforms=Win32
    ```

    DPM builds the design package into its package cache once, loads it in the IDE while the project is open, and
    adds Bold's compiled units to that project's search path (`$(DPMSearch)`). Nothing is registered IDE-wide. The
    packages are Win32 for Delphi 11, and Win32 and Win64 for Delphi 12 and 13. They are published on the public
    feed at [delphi.dev](https://delphi.dev), so no source configuration is needed.

## Delphi: manual installation

### Step 1: Get the source

```shell
git clone https://github.com/bero/BoldForDelphi.git
```

Or download the source as a ZIP from GitHub and extract it, for example to `C:\BoldForDelphi`.

### Step 2: Build and install the design-time package

1. Open the package project for your Delphi version with **File → Open Project**:

    | Delphi version | Package project |
    |----------------|-----------------|
    | Delphi 11.3 Alexandria | `packages\Delphi11.3\dclBold.dproj` |
    | Delphi 12.3 Athens | `packages\Delphi12.3\dclBold.dproj` |
    | Delphi 13 Florence | `packages\Delphi13\dclBold.dproj` |

2. Build it with **Project → Build** (Shift+F9). The BPL goes to `packages\Bin` (Win32) or `packages\Bin64`
   (Win64, for the 64-bit IDE of Delphi 12.3 and 13).
3. In the Projects pane, right-click the package and choose **Install**.
4. Check **Component → Install Packages...**: "Bold for Delphi" is in the list.

Each [GitHub release](https://github.com/bero/BoldForDelphi/releases/) also has the three Win32 BPLs
(`dclBold280.bpl` for 11.3, `dclBold290.bpl` for 12.x, `dclBold370.bpl` for 13). To use one, install it with
**Component → Install Packages... → Add**. You still need the source from Step 1, because your projects compile
Bold's units.

### Step 3: Let your projects find Bold's source

A Bold application compiles Bold's units from source, so the project's search path must name Bold's source folders
(**Project → Options → Building → Delphi Compiler → Search path**), including `Source\Common\Include` for
`bold.inc`. The easiest way is to copy the search path of an example project, such as
`examples\Tutorial\Step1_NewProject\Step1.dproj`, and adjust its relative paths to where your project is. A package
manager does this for you: Smart Setup through the library path, DPM through the project's search path.

### A Delphi version that is not listed

1. Copy the folder of the closest supported version (for example `packages\Delphi13` for Delphi 14).
2. Name the copy after the new version (for example `packages\Delphi14`).
3. Build and install it. The package uses `LIBSUFFIX AUTO`, so the BPL is named after the compiler
   (`dclBold380.bpl` for Delphi 14) and no version setting needs editing.

!!! tip
    When it works, please send a pull request that adds the new package folder to the repository.

## Lazarus and Free Pascal

Bold's Lazarus packages are in `packages\Lazarus`; its
[README](https://github.com/bero/BoldForDelphi/blob/develop/packages/Lazarus/README.md) says what each package
holds. They need Lazarus 4 with Free Pascal 3.2.2 or later. The CI builds them with Lazarus 4.4 on Linux, and the
steps below install them in Lazarus 4.8 on 64-bit Windows. The design-time package puts Bold's non-visual components
on the palette and has the OCL editor. Bold's visual controls are VCL only; an LCL application shows Bold objects
through `TBoldDataSet` and the LCL's data-aware controls. Models are edited with the standalone model editor
(`Tools/ModelEditor`, which has an LCL client) or in Delphi, and loaded from their `.bld` file.

1. **Install Lazarus.** On Windows, the installer from
   [lazarus-ide.org](https://www.lazarus-ide.org/index.php?page=downloads) includes Free Pascal and the Lazarus
   source, which the rebuild in step 5 compiles. With winget:

    ```shell
    winget install --id Lazarus.Lazarus --exact
    ```

    Add `--version 4.4` for the version the CI uses. On Debian and Ubuntu, install `lazarus-src` as well: the
    design-time package needs the source of `BuildIntf`, which those distributions ship as source only.

2. **English menus (optional).** Lazarus starts in the language of the operating system. In
   **Tools → Options → Environment → General → Language**, choose **English [en]**, not
   **Automatic (or English)**, which follows the system, and restart Lazarus. The steps below use the English names.

3. **Get the source**, as in [Step 1](#step-1-get-the-source).

4. **Open the design-time package.** Choose **Package → Open Package File (.lpk) ...** and open
   `packages\Lazarus\dclBold.lpk`. Lazarus finds the two runtime packages it requires, `Bold.lpk` and
   `BoldLCL.lpk`, in the same folder.

5. **Install it.** In the package window, choose **Use → Install**. Lazarus asks three questions:

    1. **Automatically installed packages**: installing `dclBold` installs `Bold` and `BoldLCL` too. Choose **OK**.
    2. **Suspicious include path**: "The package Bold adds the path `..\..\Source\Common\Include` to the include
       path of the IDE". Choose **Continue**, or **Continue and do not ask again**. The path is intended: a project
       that requires Bold finds `bold.inc` through it, and the folder holds only Bold's include files. Lazarus asks
       this about every package that passes an include path on.
    3. **Rebuild Lazarus?**: choose **Yes**. Lazarus compiles itself with the three packages and restarts.

6. **Check the palette.** It has three new pages: **Bold Handles** (`TBoldModel`, `TBoldSystemHandle`, the list and
   expression handles and the others), **Bold Persistence** (`TBoldPersistenceHandleDB`,
   `TBoldPersistenceHandleFileXML`) and **Bold Misc** (`TBoldDataSet` among others). The "..." button of an
   `Expression` property in the Object Inspector opens the OCL editor.

A project requires `Bold`, or `BoldLCL` for an LCL application, and never `dclBold`, so it links no registration
code. The Zeos database adapter has its own design-time package, `dclBoldZeos.lpk`, which is installed the same way
once Lazarus knows Zeos's packages (see the
[Lazarus README](https://github.com/bero/BoldForDelphi/blob/develop/packages/Lazarus/README.md)).

The command-line builds in the repository (the `build.sh` scripts of the Free Pascal tests, of the Lazarus packages'
checks and of the Invoicing examples) also run in Git Bash on Windows. They find Lazarus in `C:\lazarus`, or in the
folder that `LAZARUS_DIR` names.

## Check the installation

After installing in Delphi, the Tool Palette has these pages:

- **Bold Handles**: `TBoldModel`, [TBoldSystemHandle](../classes/TBoldSystemHandle.md),
  `TBoldSystemTypeInfoHandle`, [TBoldListHandle](../classes/TBoldListHandle.md),
  [TBoldExpressionHandle](../classes/TBoldExpressionHandle.md) and more
- **Bold Controls**: `TBoldGrid`, `TBoldEdit`, `TBoldComboBox`, `TBoldNavigator` and more (VCL)
- **Bold Persistence**: `TBoldPersistenceHandleDB`, `TBoldDatabaseAdapterFireDAC`, `TBoldPersistenceHandleFileXML`
- **Bold Misc**: `TBoldDataSet` and other helpers

Bold's actions (`TBoldActivateSystemAction`, `TBoldUpdateDBAction`, `TBoldCreateDatabaseAction` and others) are not
on the palette: add them in the action list editor with **New Standard Action**, category **Bold Actions**.

## Troubleshooting

### "bold.inc not found"

The project's search path lacks `Source\Common\Include`. See
[Step 3](#step-3-let-your-projects-find-bolds-source).

### The components are not on the palette

- Check that the package is installed and enabled in **Component → Install Packages...**.
- Check that the packages it requires are loaded.

### The package does not compile

- Make sure you opened the package for your Delphi version.
- Make sure no older Bold package is installed.
