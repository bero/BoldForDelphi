# DevExpress Integration

Bold for Delphi includes components that integrate with the DevExpress VCL component suite, providing Bold-aware versions of popular DevExpress controls.

## Requirements

- **DevExpress VCL Subscription** with **full source code** access
- DevExpress source code must be accessible to the Delphi compiler

The Bold DevExpress integration uses DevExpress internal APIs and access classes, which requires the DevExpress source code to compile.

## Available Components

### Grid Components

| Component | Description |
|-----------|-------------|
| `TcxGridBoldTableView` | Bold-aware table view for cxGrid |
| `TcxGridBoldBandedTableView` | Bold-aware banded table view |
| `TcxGridBoldDataController` | Data controller connecting cxGrid to Bold object lists |
| `TcxLookupBoldGrid` | Bold-aware lookup grid |

### Editors

| Component | Description |
|-----------|-------------|
| `TcxBoldTextEdit` | Bold-aware text editor |
| `TcxBoldComboBox` | Bold-aware combo box |
| `TcxBoldLookupComboBox` | Bold-aware lookup combo box |
| `TcxBoldExtLookupComboBox` | Extended lookup combo box with Bold support |
| `TcxBoldCheckBox` | Bold-aware check box |
| `TcxBoldDateEdit` | Bold-aware date editor |
| `TcxBoldTimeEdit` | Bold-aware time editor |
| `TcxBoldSpinEdit` | Bold-aware spin editor |
| `TcxBoldMemo` | Bold-aware memo editor |
| `TcxBoldImage` | Bold-aware image editor |

### Navigation

| Component | Description |
|-----------|-------------|
| `TdxBarBoldNavigator` | Bold-aware DevExpress bar navigator |

### Repository Items

The `cxBoldEditRepositoryItems` unit provides repository item versions of all Bold editors for use in cxGrid columns.

## Installation

1. Ensure DevExpress VCL is installed with source code
2. Build the `dclBoldDevEx.dpk` package for your Delphi version
3. Install the package in the IDE

## Basic Usage

### Using TcxGridBoldTableView

```pascal
uses
  cxGridBoldSupportUnit;

// Create a Bold-aware grid view
var
  GridView: TcxGridBoldTableView;
begin
  GridView := cxGrid1.CreateView(TcxGridBoldTableView) as TcxGridBoldTableView;

  // Connect to a BoldListHandle
  GridView.DataController.BoldHandle := BoldListHandle1;

  // Add columns
  var Col := GridView.CreateColumn;
  Col.DataBinding.BoldProperties.Expression := 'name';
  Col.Caption := 'Name';
end;
```

### Using Bold Editors in cxGrid

```pascal
uses
  cxBoldEditRepositoryItems;

// In the grid column properties
Column.PropertiesClass := TcxBoldTextEditProperties;

// Or use repository items
var
  RepoItem: TcxEditRepositoryBoldTextItem;
begin
  RepoItem := cxEditRepository1.CreateItem(TcxEditRepositoryBoldTextItem) as TcxEditRepositoryBoldTextItem;
  Column.RepositoryItem := RepoItem;
end;
```

### Standalone Bold Editors

```pascal
uses
  cxBoldEditors;

// Place TcxBoldTextEdit on form
cxBoldTextEdit1.DataBinding.BoldHandle := BoldExpressionHandle1;
```

## Source Files

The DevExpress integration is located in `Source/BoldAwareGUI/BoldDevex/`:

| File | Purpose |
|------|---------|
| `cxGridBoldSupportUnit.pas` | cxGrid integration and data controller |
| `cxBoldEditors.pas` | Bold-aware editor components |
| `cxBoldEditRepositoryItems.pas` | Repository items for grid columns |
| `cxBoldLookupComboBox.pas` | Lookup combo box component |
| `cxBoldExtLookupComboBox.pas` | Extended lookup combo box |
| `cxLookupBoldGrid.pas` | Lookup grid component |
| `dxBarBoldNav.pas` | DevExpress bar navigator |
| `BoldToCxConverterUnit.pas` | Converter utilities |
| `BoldAFPCxGridProviderUnit.pas` | Auto-form provider for cxGrid |
| `cxBoldRegUnit.pas` | Component registration |

## Key Features

### Automatic UI Updates

Like standard Bold controls, DevExpress Bold controls automatically update when the underlying Bold objects change. The subscription system ensures the UI stays synchronized with the object model.

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

The lookup combo boxes support OCL-based list expressions:

```pascal
cxBoldLookupComboBox1.Properties.ListHandle := BoldListHandle1;
cxBoldLookupComboBox1.Properties.LookupExpression := 'name';
```

## Troubleshooting

### Compilation Errors

If you get "unit not found" errors for DevExpress units:

1. Verify DevExpress source code is in the Delphi library path
2. Check that you have the correct DevExpress version installed
3. Ensure the DevExpress source code option was selected during installation

### Missing Properties

If Bold-specific properties don't appear in the Object Inspector:

1. Verify `dclBoldDevEx.dpk` is installed
2. Check the component palette for "Bold DevEx" tab
3. Rebuild and reinstall the package if needed

## Version Compatibility

The DevExpress integration is tested with recent DevExpress VCL versions. If you encounter compatibility issues with a specific DevExpress version, please report them on GitHub.
