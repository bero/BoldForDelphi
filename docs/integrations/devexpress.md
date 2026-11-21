# DevExpress Integration

Bold for Delphi includes Bold-aware versions of DevExpress VCL controls: cxGrid views, editors and a bar navigator.
They are VCL only.

!!! warning "Unmaintained"

    The DevExpress integration is not part of the release packages and is currently unmaintained. Its design-time
    package is `NotworkingProjects/dclBoldDevEx.dpk`; build it yourself if you need it. The sources are in
    `Source/BoldAwareGUI/BoldDevex/`. Reports and fixes are welcome on GitHub.

## Requirements

- **DevExpress VCL subscription** with **full source code**
- The DevExpress source code must be on the Delphi library path

The Bold DevExpress integration uses DevExpress internal APIs and access classes, which requires the DevExpress
source code to compile.

## Available Components

### Grid Views

`cxGridBoldSupportUnit` registers these views with cxGrid:

| Component | Description |
|-----------|-------------|
| `TcxGridBoldTableView` | Bold-aware table view ("Bold Table") |
| `TcxGridBoldBandedTableView` | Bold-aware banded table view ("Bold Banded Table") |
| `TcxGridBoldCardView` | Bold-aware card view ("Bold Card") |
| `TcxGridBoldLayoutView` | Bold-aware layout view ("Bold Layout") |
| `TcxGridBoldDataController` | Data controller connecting a view to a Bold list handle |

### Editors

| Component | Description |
|-----------|-------------|
| `TcxBoldTextEdit` | Text editor |
| `TcxBoldComboBox` | Combo box |
| `TcxBoldLookupComboBox` | Lookup combo box |
| `TcxBoldExtLookupComboBox` | Extended lookup combo box, with a grid view as drop-down |
| `TcxBoldCheckBox` | Check box |
| `TcxBoldDateEdit` | Date editor |
| `TcxBoldTimeEdit` | Time editor |
| `TcxBoldSpinEdit` | Spin editor |
| `TcxBoldMemo` | Memo editor |
| `TcxBoldImage` | Image editor |

The palette has more: `TcxBoldCurrencyEdit`, `TcxBoldMaskEdit`, `TcxBoldButtonEdit`, `TcxBoldHyperLinkEdit`,
`TcxBoldProgressBar`, `TcxBoldLabel`, `TcxBoldRichEdit`, `TcxBoldListBox`, `TcxBoldCheckListBox`,
`TcxBoldSelectionCheckListBox`, `TcxBoldListView` and the non-bound lookup combo boxes `TcxBoldNBLookupComboBox` and
`TcxBoldNBExtLookupComboBox`.

### Navigation

| Component | Description |
|-----------|-------------|
| `TdxBarBoldNavigator` | Bold-aware DevExpress bar navigator |

### Repository Items

The `cxBoldEditRepositoryItems` unit provides repository items for cxGrid columns:
`TcxEditRepositoryBoldStringItem`, `TcxEditRepositoryBoldComboBoxItem`, `TcxEditRepositoryBoldLookupComboBoxItem`
and `TcxEditRepositoryBoldExtLookupComboBoxItem`.

## Installation

1. Install DevExpress VCL with source code.
2. Open `NotworkingProjects/dclBoldDevEx.dpk`. Its `requires` clause names the DevExpress packages with the
   suffix `RS27`; change them to match your DevExpress and Delphi versions.
3. Build and install the package after `dclBold`.

The components appear on the palette page **Express BoldEditors**.

## Basic Usage

### Using TcxGridBoldTableView

```pascal
uses
  cxGridBoldSupportUnit;

// Create a Bold-aware grid view
var
  GridView: TcxGridBoldTableView;
  Col: TcxGridBoldColumn;
begin
  GridView := cxGrid1.CreateView(TcxGridBoldTableView) as TcxGridBoldTableView;

  // Connect to a BoldListHandle
  GridView.DataController.BoldHandle := BoldListHandle1;

  // Add columns
  Col := GridView.CreateColumn as TcxGridBoldColumn;
  Col.DataBinding.BoldProperties.Expression := 'name';
  Col.Caption := 'Name';
end;
```

### Using Bold Editors in cxGrid

```pascal
uses
  cxBoldEditors, cxBoldEditRepositoryItems;

// In the grid column properties
Column.PropertiesClass := TcxBoldTextEditProperties;

// Or use repository items
var
  RepoItem: TcxEditRepositoryBoldStringItem;
begin
  RepoItem := cxEditRepository1.CreateItem(TcxEditRepositoryBoldStringItem) as TcxEditRepositoryBoldStringItem;
  Column.RepositoryItem := RepoItem;
end;
```

### Standalone Bold Editors

```pascal
uses
  cxBoldEditors;

// Place a TcxBoldTextEdit on the form
cxBoldTextEdit1.DataBinding.BoldHandle := BoldExpressionHandle1;
cxBoldTextEdit1.DataBinding.BoldProperties.Expression := 'name';
```

## Key Features

### Automatic UI Updates

Like the standard Bold controls, the DevExpress Bold controls update when the underlying Bold objects change. The
[subscription system](../concepts/subscriptions.md) keeps the UI synchronized with the object space.

### OCL Expressions

Use OCL expressions for data binding:

```pascal
// Display a derived value
Column.DataBinding.BoldProperties.Expression := 'orders->size';

// Display a formatted value
Column.DataBinding.BoldProperties.Expression :=
  'firstName + '' '' + lastName';
```

### Lookup Support

The lookup combo boxes take their list from a Bold list handle:

```pascal
cxBoldLookupComboBox1.Properties.BoldLookupListHandle := BoldListHandle1;
```

## Source Files

The DevExpress integration is in `Source/BoldAwareGUI/BoldDevex/`:

| Unit | Purpose |
|------|---------|
| `cxGridBoldSupportUnit` | cxGrid views and data controller |
| `cxBoldEditors` | Bold-aware editors |
| `cxBoldEditRepositoryItems` | Repository items for grid columns |
| `cxBoldLookupEdit` | Base classes of the lookup editors |
| `cxBoldLookupComboBox` | Lookup combo box |
| `cxBoldExtLookupComboBox` | Extended lookup combo box |
| `cxLookupBoldGrid` | Lookup grid |
| `dxBarBoldNav` | DevExpress bar navigator |
| `BoldToCxConverterUnit`, `BoldToCxGridConverterUnit` | Converters from Bold controls to DevExpress controls |
| `BoldAFPCxGridProviderUnit` | Auto-form provider for cxGrid |
| `cxBoldRegUnit` | Component registration |

## Troubleshooting

### Compilation Errors

If you get "unit not found" errors for DevExpress units:

1. Check that the DevExpress source code is on the Delphi library path.
2. Check that the package names in the `requires` clause match your DevExpress version.
3. Check that the DevExpress source code option was selected during installation.

### Missing Properties

If the Bold-specific properties do not appear in the Object Inspector:

1. Check that `dclBoldDevEx` is installed.
2. Look for the **Express BoldEditors** page on the component palette.
3. Rebuild and reinstall the package if needed.

## Version Compatibility

The integration was last tested with DevExpress VCL 24.2.4. Report problems with other DevExpress versions on GitHub.
