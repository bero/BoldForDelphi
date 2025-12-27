# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Bold for Delphi is a Model-Driven Architecture (MDA) framework and Object-Relational Mapping (ORM) tool. It enables UML-based model development with OCL (Object Constraint Language) queries, automatic code generation, and sophisticated database persistence. Originally released by Boldsoft in 2004, it was open-sourced by Embarcadero in 2020 under MIT license.

**Current Version**: 4.0.1.0 (community-maintained develop branch)
**Target Platforms**: Delphi 11.3, 12.1, 12.3 and 13 (Win32/Win64)

## Claude Code Instructions

By default never guess to generate the answer. If information is missing ask for clarification. Only guess if the prompt actually tells you to.

### Important Paths (REMEMBER THESE!)

- **UnitTest.exe**: `C:\Attracs\BoldForDelphi\UnitTest\UnitTest.exe`
- **rsvars.bat**: `C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat`
- **UnitTest project folder**: `C:\Attracs\BoldForDelphi\UnitTest`

### Git Commit Messages

Do not include Claude references in git commit messages. No "Generated with Claude", no "Co-Authored-By: Claude", and no similar attributions.

### Writing Unit Tests

When adding unit tests to improve code coverage:

1. **Focus on uncovered lines** - Check the coverage report first. If a unit has 85/100 lines covered, write tests targeting the remaining 15 uncovered lines, not the already-covered code.

2. **Minimal tests for coverage** - Write the minimum test code needed to exercise uncovered paths. Don't add redundant tests for code already covered by existing tests.

3. **Check what's already tested** - Before writing tests, understand why existing coverage exists. Often, internal functions are exercised indirectly through other code paths.

4. **Target exception paths** - Uncovered lines are often error handling or edge cases. These typically require specific test scenarios (invalid input, null values, etc.).

5. **One test per gap** - If only one function/path is uncovered, one focused test is sufficient.

Example: If `BoldFoo.pas` has 97% coverage with only an exception handler uncovered, write a single test that triggers that exception - don't write additional tests for the already-covered happy paths.

## Project Structure

```
BoldForDelphi/
├── packages/
│   ├── Delphi28/                ← Delphi 11.3 Alexandria
│   ├── Delphi29.1/              ← Delphi 12.1 Athens
│   ├── Delphi29.3/              ← Delphi 12.3 Athens
│   │   ├── dclBold.dpk          (design-time package)
│   │   └── dclBold.dproj
│   └── Bin/                     ← Compiled BPL output
├── Source/
│   ├── Common/
│   │   ├── Core/                ← Base classes: BoldBase, BoldContainers, BoldDefs
│   │   ├── Subscription/        ← Observer pattern: BoldSubscription, BoldDeriver
│   │   ├── Support/             ← Utilities: BoldUtils, BoldGuard, BoldIndex
│   │   └── Include/             ← Compiler directives: Bold.inc
│   ├── ObjectSpace/
│   │   ├── Core/                ← TBoldSystem, BoldElements
│   │   ├── BORepresentation/    ← BoldAttributes, BoldSystem
│   │   ├── Ocl/                 ← OCL parser and evaluator
│   │   ├── RTModel/             ← Runtime type information (BoldSystemRT)
│   │   └── Undo/                ← Undo/redo mechanism
│   ├── Persistence/
│   │   ├── Core/                ← Persistence controllers and handles
│   │   ├── DB/                  ← Database persistence base
│   │   └── FireDAC/             ← FireDAC adapter (recommended)
│   ├── PMapper/
│   │   ├── SQL/                 ← SQL generation: BoldSqlNodes, BoldSqlQueryGenerator
│   │   └── DbEvolutor/          ← Database schema evolution
│   ├── BoldAwareGUI/
│   │   ├── BoldControls/        ← Data-aware VCL: BoldGrid, BoldEdit, BoldComboBox
│   │   ├── ControlPacks/        ← Renderer/follower pattern for UI binding
│   │   └── Core/                ← BoldGUI base functionality
│   ├── Handles/                 ← TBoldSystemHandle, TBoldListHandle, TBoldExpressionHandle
│   ├── MoldModel/
│   │   ├── Core/                ← Model representation (Mold = Model of Objects)
│   │   └── CodeGenerator/       ← Delphi code generation
│   └── UMLModel/
│       ├── Core/                ← UML metamodel (BoldUMLModel.pas)
│       ├── Editor/              ← Model editor UI
│       └── ModelLinks/          ← XMI, Rose98 integration
└── UnitTest/                    ← DUnitX tests
```

### Folders to Ignore

- **Source/Deprecated/** - Contains deprecated database adapters (ADO, BDE, Advantage, DBExpress). Not used, not maintained. Ignore when searching for cleanup opportunities.

## Build Commands

### Building Delphi projects

RAD Studio 13 is installed at: `C:\Program Files (x86)\Embarcadero\Studio\37.0`
Use rsvars.bat before msbuild. Don't guess version numbers.

```batch
# Example: Build a project from Windows command prompt
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat" && msbuild Project.dproj /p:Config=Debug /p:Platform=Win64
```

### Building Bold packages (from Windows cmd)

```batch
cd packages\Delphi30
build.bat
```

Output files (e.g. `dclBold.30.bpl`) go to `packages\Bin\`.

## Environment Setup

Core Bold packages use **relative paths** and require no environment variable setup. Clone/copy the repository to any location and build.

**Optional**: For UniDAC support, set the `UniDAC` environment variable in Delphi (Tools > Options > Environment Variables) pointing to the UniDAC installation root.

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

## Key Packages

| Package | Type | Purpose |
|---------|------|---------|
| `dclBold.dpk` | Design-time | Main Bold components for IDE integration |
| `dclBoldUniDAC.dpk` | Design-time | UniDAC database adapter components |
| `dclBoldDevEx.dpk` | Design-time | DevExpress integration components |

## Compiler Directives (Bold.inc)

The `{$DEFINE Attracs}` block at end of Bold.inc enables optimizations used by Attracs. When `Attracs` is NOT defined, several features are disabled for broader compatibility.

Key defines (enabled when `Attracs` is defined):
```pascal
{$DEFINE SpanFetch}                          // Efficient batch fetching
{$DEFINE IDServer}                           // External ID server (improves write perf)
{$DEFINE CompareToOldValues}                 // Skip unchanged values in updates
{$DEFINE NoNegativeDates}                    // Restrict date range validation
{$DEFINE NoTransientInstancesOfPersistentClass}  // Performance optimization
```

Other notable defines:
```pascal
{$DEFINE BoldJson}           // JSON serialization support (always on)
{$DEFINE BOLD_NO_QUERIES}    // Turns off query mechanism (always on)
```

## Key Concepts

### Object Constraint Language (OCL)
OCL is used for queries and constraints:
```
self.allInstances                    // All instances of a class
self.customers->select(age > 30)     // Filter collection
self.orders->collect(total)->sum     // Aggregate
```

### Subscription Pattern
Bold uses a sophisticated subscription system for automatic UI updates when objects change. Components subscribe to objects/attributes and receive notifications on changes.

## Resources

- Wiki: https://delphi.fandom.com/wiki/Bold_for_Delphi
- Blog: http://boldfordelphi.blogspot.com/
- Discord: https://discord.gg/C6frzsn
