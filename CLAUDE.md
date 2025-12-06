# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Bold for Delphi is a Model-Driven Architecture (MDA) framework and Object-Relational Mapping (ORM) tool. It enables UML-based model development with OCL (Object Constraint Language) queries, automatic code generation, and sophisticated database persistence. Originally released by Boldsoft in 2004, it was open-sourced by Embarcadero in 2020 under MIT license.

**Current Version**: 4.0.1.0 (community-maintained develop branch)
**Target Platform**: Delphi 12.x Athens (Win32/Win64)

## Claude Code instructions
By default never guess to generate the answer. If information is missing ask for clarification.
Only guess if the prompt actually tell this.

## Build Commands

```batch
# Build main runtime package (must be built first)
msbuild Bold.dpk /p:Config=Debug /p:Platform=Win32

# Build design-time IDE package (requires Bold.dpk)
msbuild dclBold.dpk /p:Config=Debug /p:Platform=Win32

# Build unit tests
msbuild UnitTest\UnitTest.dproj /p:Config=Debug /p:Platform=Win32
```

### Database-Specific Packages
```batch
# Build specific database adapter packages as needed:
msbuild BoldFireDAC.dpk   # FireDAC (recommended)
msbuild BoldUniDAC.dpk    # UniDAC
msbuild BoldAdo.dpk       # ADO
msbuild BoldIB.dpk        # InterBase/IBX
```

## Environment Setup

The core Bold packages use **relative paths** and require no environment variable setup. Simply clone/copy the repository to any location and build.

Output directories:
- BPL files: `Bin\` (relative to repository root)
- DCU files: Default Delphi output locations

### Optional: UniDAC Support

To build `dclBoldUniDAC.dpk`, set the `UniDAC` environment variable:
- In Delphi: Tools > Options > Environment Variables
- Example: `UniDAC=C:\Path\To\UniDAC` (points to UniDAC installation root)

**Note**: Projects that consume Bold from external repositories may still need to configure search paths to reference the Bold source location.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    UML Model Editor                             │
│         (BoldUMLModel, XMI/Rose import/export)                 │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                Code Generator (MoldModel)                       │
│    Generates BusinessClasses.pas from UML model                │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│              ObjectSpace (Runtime Object Model)                 │
│  TBoldSystem, TBoldObject, TBoldAttribute, TBoldObjectList     │
│         OCL Query Engine (BoldOcl, BoldOclEvaluator)           │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│           Persistence Layer (PMapper, Persistence)             │
│     Object-Relational Mapping, SQL Generation, DB Evolution    │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│              Database Adapters (FireDAC, UniDAC, etc.)         │
│         SQL Server, PostgreSQL, InterBase, Oracle, etc.        │
└─────────────────────────────────────────────────────────────────┘
```

## Source Code Organization

### Core Framework (`Source/`)

| Directory | Purpose |
|-----------|---------|
| `Common/Core/` | Base classes: BoldBase, BoldContainers, BoldStreams, BoldDefs |
| `Common/Subscription/` | Observer pattern: BoldSubscription, BoldDeriver |
| `Common/Support/` | Utilities: BoldUtils, BoldGuard, BoldMath, BoldIndex |
| `Common/Include/` | Compiler directives: Bold.inc, BoldDelphiVer.inc |

### Object Model (`Source/ObjectSpace/`)

| Directory | Purpose |
|-----------|---------|
| `Core/` | TBoldSystem - central object space manager |
| `BORepresentation/` | BoldAttributes, BoldElements, BoldSystem |
| `Ocl/` | OCL parser and evaluator |
| `RTModel/` | Runtime type information (BoldSystemRT) |
| `Undo/` | Undo/redo mechanism |

### Persistence (`Source/Persistence/`, `Source/PMapper/`)

| Directory | Purpose |
|-----------|---------|
| `Persistence/Core/` | Persistence controllers and handles |
| `Persistence/DB/` | Database persistence base |
| `Persistence/FireDAC/` | FireDAC adapter (recommended) |
| `Persistence/UniDAC/` | UniDAC adapter |
| `PMapper/SQL/` | SQL generation: BoldSqlNodes, BoldSqlQueryGenerator |
| `PMapper/DbEvolutor/` | Database schema evolution |

### GUI Components (`Source/BoldAwareGUI/`)

| Directory | Purpose |
|-----------|---------|
| `BoldControls/` | Data-aware VCL: BoldGrid, BoldEdit, BoldComboBox, BoldTreeView |
| `ControlPacks/` | Renderer/follower pattern for UI binding |
| `Core/` | BoldGUI base functionality |

### Handles System (`Source/Handles/`)

The Handles system provides the observer pattern connecting UI to object space:
- `BoldSystemHandle` - Connection to TBoldSystem
- `BoldListHandle` - Observable list binding
- `BoldExpressionHandle` - OCL expression evaluation

### Model & Code Generation (`Source/MoldModel/`, `Source/UMLModel/`)

| Directory | Purpose |
|-----------|---------|
| `MoldModel/Core/` | Model representation (Mold = Model of Objects) |
| `MoldModel/CodeGenerator/` | Delphi code generation |
| `UMLModel/Core/` | UML metamodel (BoldUMLModel.pas) |
| `UMLModel/Editor/` | Model editor UI |
| `UMLModel/ModelLinks/` | XMI, Rose98, ModelMaker integration |

## Key Packages

| Package | Type | Purpose |
|---------|------|---------|
| `Bold.dpk` | Runtime | Main framework (~490 units) |
| `dclBold.dpk` | Design-time | IDE integration (~150 units) |
| `BoldFireDAC.dpk` | Runtime | FireDAC database adapter |
| `BoldUniDAC.dpk` | Runtime | UniDAC database adapter |
| `BoldOLLE.dpk` | Runtime | Object Lending Library Extension |
| `BoldUml.dpk` | Runtime | UML model editor |

## Compiler Directives (Bold.inc)

Key defines that affect framework behavior:

```pascal
{$DEFINE Attracs}              // Enable Attracs-specific optimizations
{$DEFINE SpanFetch}            // Efficient batch fetching
{$DEFINE IDServer}             // External ID server (improves write perf)
{$DEFINE CompareToOldValues}   // Skip unchanged values in updates
{$DEFINE NoNegativeDates}      // Restrict date range validation
{$DEFINE BoldJson}             // JSON serialization support
```

## Testing

Tests use DUnitX framework:
```batch
# Run console tests
UnitTest\Win32\Debug\UnitTest.exe

# Run GUI tests (TestInsight)
UnitTest\Win32\Debug\UnitTestGUI.exe
```

## Key Concepts

### Object Constraint Language (OCL)
OCL is used for queries and constraints. Examples:
```
self.allInstances                    // All instances of a class
self.customers->select(age > 30)     // Filter collection
self.orders->collect(total)->sum     // Aggregate
```

### Subscription Pattern
Bold uses a sophisticated subscription system for automatic UI updates when objects change. Components subscribe to objects/attributes and receive notifications on changes.

### Persistence Handles
- `TBoldPersistenceHandleDB` - Database connection
- `TBoldSystemHandle` - Object space management
- `TBoldListHandle` - Observable list for UI binding

## Resources

- Wiki: https://delphi.fandom.com/wiki/Bold_for_Delphi
- Blog: http://boldfordelphi.blogspot.com/
- Discord: https://discord.gg/C6frzsn
