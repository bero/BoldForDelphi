# Integrations

Bold for Delphi can work with third-party component libraries: Bold-aware versions of their controls, and database
adapters for their data access components.

## Available Integrations

| Integration | Kind | Description | License required | Status |
|-------------|------|-------------|------------------|--------|
| [DevExpress](devexpress.md) | UI controls (VCL) | Bold-aware cxGrid views, editors and bar navigator | DevExpress VCL with source | Not in the release packages; unmaintained |
| UniDAC | Database adapter (Delphi) | `TBoldDatabaseAdapterUniDAC` for Devart's UniDAC | UniDAC license | Used from code; covered by the test suite |
| Zeos | Database adapter (Free Pascal) | `TBoldDatabaseAdapterZeos` for ZeosLib 8.0 | Free | Lazarus packages `BoldZeos` and `dclBoldZeos` |

FireDAC, the recommended database access library for Delphi, is part of the main package. [Persistence](../concepts/persistence.md)
describes all database adapters.

## How UI Integrations Work

A Bold-aware control wraps a third-party control and adds:

- **Data binding**: connect the control to Bold handles and objects
- **OCL expressions**: OCL defines what the control displays
- **Automatic updates**: the control updates when objects change, through [subscriptions](../concepts/subscriptions.md)
- **Type safety**: the Bold type system validates the values

## Integration Packages

| Integration | Package | Location |
|-------------|---------|----------|
| DevExpress | `dclBoldDevEx.dpk` | `NotworkingProjects/` |
| UniDAC | `dclBoldUniDAC.dpk` | `NotworkingProjects/` |
| Zeos | `BoldZeos.lpk`, `dclBoldZeos.lpk` | `packages/Lazarus/` |

The packages in `NotworkingProjects/` are not part of the release packages. Build and install an integration
package after the main Bold package (`dclBold`).
