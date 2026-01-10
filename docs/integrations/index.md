# Integrations

Bold for Delphi can integrate with various third-party component libraries to provide Bold-aware versions of popular controls.

## Available Integrations

| Integration | Description | License Required |
|-------------|-------------|------------------|
| [DevExpress](devexpress.md) | Bold-aware cxGrid, editors, and navigation | DevExpress VCL with source |
| UniDAC | Universal database connectivity | UniDAC license |

## How Integrations Work

Bold integrations wrap third-party components to add Bold-specific functionality:

- **Data Binding** - Connect controls directly to Bold handles and objects
- **OCL Expressions** - Use OCL to define what data is displayed
- **Automatic Updates** - Controls update automatically when objects change
- **Type Safety** - Leverage the Bold type system for validation

## Building Integration Packages

Each integration has its own design-time package:

| Integration | Package |
|-------------|---------|
| DevExpress | `dclBoldDevEx.dpk` |
| UniDAC | `dclBoldUniDAC.dpk` |

Build and install these packages after the main Bold package (`dclBold.dpk`).
