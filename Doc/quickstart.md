# Bold for Delphi - Quick Start Guide 🚀

Get your first Bold application running in 15 minutes! ⏱️

## What is Bold? 💡

Bold for Delphi is a Model-Driven Architecture (MDA) framework that:
- Lets you define your domain model in UML
- Auto-generates strongly-typed Delphi classes from your model
- Handles object-relational mapping (ORM) to any database
- Provides OCL (Object Constraint Language) for queries
- Includes data-aware VCL components with automatic UI updates

**The core idea**: You work with objects, Bold handles the database.

```
UML Model → Code Generator → Business Classes → Bold Runtime → Database
```

---

## Prerequisites ✅

- **Delphi 11.3, 12.x, or 13**
- **Database**: Any FireDAC-supported database should work. SQL Server is tested
- **Bold packages installed** (see Installation below)

---

## Installation 📦

### Step 1: Get the Source

```
git clone https://github.com/bero/BoldForDelphi.git
```

Or download and extract to a folder like `C:\BoldForDelphi`.

### Step 2: Get the Packages

**Option A: Download Pre-built Binaries (Recommended)**

Download the binary package for your Delphi version from:
https://github.com/bero/BoldForDelphi/releases/

Extract to `packages\Bin\`.
Add the new package from menu Component/Install packages...
Then press Add and select your bpl-file

**Option B: Build from Source** 🔧

Compiling from source gives you the latest version, or lets you support unsupported Delphi versions.

Open the package file:
```
C:\BoldForDelphi\packages\<Delphi version>\dclBold.dpk
```

Replace `<Delphi version>` with your version:

| Delphi Version | Folder |
|----------------|--------|
| Delphi 11.3 | Delphi28 |
| Delphi 12.1 | Delphi29.1 |
| Delphi 12.3 | Delphi29.3 |
| Delphi 13 | Delphi30 |

1. Build the package
2. Right-click on the bpl-file in project panel and choose **Install**
3. The bpl should now be active in `packages\Bin\`
4. Verify via menu **Component → Install Packages...**

You should now see Bold components in the Tool Palette. 🎉

---

## Your First Bold Application 🏗️

First option is to look at ready app in `C:\BoldForDelphi\examples\Compound\Building`. Just compile it.
Edit the ini-file according your database. If you don't have installed a database try XML.

Second option is to build app from scratch. More fun and more learning 😊
We'll build a simple app with **Person** and **Building** objects, where persons can own buildings.

### Step 1: Create a New VCL Application

1. **File → New → VCL Forms Application**
2. Save the project (e.g. `Building.dproj`)

### Step 2: Add a Data Module

1. **File → New → Other → Delphi Files → Data Module**
2. Save as `DataModule1.pas`

### Step 3: Add Core Bold Components

Drop these components on your DataModule from the Bold palette:

| Component | Purpose |
|-----------|---------|
| `TBoldModel` | Holds your UML model |
| `TBoldSystemHandle` | Main runtime system |
| `TBoldSystemTypeInfoHandle` | Type information for design-time |
| `TBoldPersistenceHandleDB` | Database persistence |

Connect them:
- `BoldSystemHandle1.BoldModel` → `BoldModel1`
- `BoldSystemHandle1.PersistenceHandle` → `BoldPersistenceHandleDB1`
- `BoldSystemTypeInfoHandle1.BoldModel` → `BoldModel1`

### Step 4: Add Database Connection

1. Drop a `TFDConnection` (FireDAC) on the DataModule
2. Configure it for your database (e.g. MSSQL for testing):
   ```ini
   [Database]
   Persistence=FireDAC
   Type=MSSQL

   [MSSQL]
   Server=localhost
   Database=BoldDemo
   OSAuthent=True
   User=sa
   Password=
   ```
3. Drop a `TBoldDatabaseAdapterFireDAC` component
4. Set `BoldDatabaseAdapterFireDAC1.Connection` → `FDConnection1`
5. Set `BoldPersistenceHandleDB1.DatabaseAdapter` → `BoldDatabaseAdapterFireDAC1`

### Step 5: Create Your Model

1. Double-click `BoldModel1` to open the Model Editor
2. Create your classes:

**Person class:**
- Right-click → Add Class → Name: `Person`
- Add attributes:
  - `FirstName`: String
  - `LastName`: String
  - `Assets`: Currency

**Building class:**
- Add Class → Name: `Building`
- Add attributes:
  - `Address`: String
  - `ZipCode`: Integer

**Create an association (Ownership):**
- Select both classes
- Right-click → Add Association
- Name: `Ownership`
- Person side: `OwnedBuildings` (0..*)
- Building side: `Owners` (0..*)

3. **File → Save & Generate all files**

This creates `BusinessClasses.pas` with strongly-typed classes.

### Step 6: Add Business Classes to Project

1. Add the generated `BusinessClasses.pas` to your project
2. Add to DataModule uses clause:
   ```pascal
   uses
     BusinessClasses;
   ```

### Step 7: Design the Main Form

On your main form, add:

**List Handles** (from Bold Handles palette):
- `TBoldListHandle` named `blhAllPersons`
  - `RootHandle` → `DataModule1.BoldSystemHandle1`
  - `Expression` → `Person.allInstances`

- `TBoldListHandle` named `blhAllBuildings`
  - `RootHandle` → `DataModule1.BoldSystemHandle1`
  - `Expression` → `Building.allInstances`

**GUI Components** (from Bold Controls palette):
- `TBoldGrid` for persons → `BoldHandle` = `blhAllPersons`
- `TBoldGrid` for buildings → `BoldHandle` = `blhAllBuildings`
- `TBoldNavigator` → `BoldHandle` = `blhAllPersons`

**Actions** (from Bold Actions palette):
- `TBoldActivateSystemAction` → Opens the database
- `TBoldUpdateDBAction` → Saves changes to database
- Add buttons linked to these actions

### Step 8: Initialize the System

In your main form's OnCreate:

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activate opens/creates the database and loads the model
  BoldActivateSystemAction1.Execute;
end;
```

### Step 9: Run It! ▶️

1. Press F9 to run
2. Click "Open System" to activate
3. Use the navigator to add Person records
4. The grid automatically displays them
5. Click "Update DB" to save

**That's it!** You have a working Bold application. 🎊

---

## Working with Objects in Code 💻

Bold generates clean, strongly-typed classes. Here's how to use them:

### Creating Objects

```pascal
var
  Person: TPerson;
  Building: TBuilding;
begin
  // Create objects - Bold tracks them automatically
  Person := TPerson.Create(BoldSystemHandle1.System);
  Person.FirstName := 'John';
  Person.LastName := 'Doe';
  Person.Assets := 50000;

  Building := TBuilding.Create(BoldSystemHandle1.System);
  Building.Address := '123 Main Street';
  Building.ZipCode := 12345;

  // Create relationship
  Person.OwnedBuildings.Add(Building);
end;
```

### Querying with OCL

```pascal
var
  RichPeople: TBoldObjectList;
begin
  // Find all persons with assets > 100000
  RichPeople := BoldSystemHandle1.System.EvaluateExpressionAsNewElement(
    'Person.allInstances->select(assets > 100000)'
  ) as TBoldObjectList;

  // Use type-safe access
  for var i := 0 to RichPeople.Count - 1 do
    ShowMessage(TPerson(RichPeople[i]).FirstName);
end;
```

### Saving to Database

```pascal
// Save all changes
BoldSystemHandle1.UpdateDatabase;

// Or discard changes
BoldSystemHandle1.System.Discard;
```

---

## Key Concepts 📚

| Concept | Description |
|---------|-------------|
| **BoldSystem** | The runtime container for all your objects |
| **Handles** | Connect UI components to objects (`TBoldListHandle`, `TBoldExpressionHandle`) |
| **OCL Expressions** | Query language: `Person.allInstances->select(age > 30)` |
| **Subscription** | UI auto-updates when objects change |
| **M_ properties** | Direct access to Bold members: `Person.M_FirstName.AsString` |

---

## Common OCL Expressions 🔍

```
Person.allInstances                      -- All persons
Person.allInstances->size                -- Count of persons
Person.allInstances->select(assets > 0)  -- Filter
Person.allInstances->collect(lastName)   -- Extract attribute
Person.allInstances->sortedBy(lastName)  -- Sort
self.ownedBuildings->size                -- Count related objects
self.firstName + ' ' + self.lastName     -- Concatenation
```

---

## Next Steps 🚶

1. **Explore the Examples**: See `examples/Compound/Building/` for a complete sample
2. **Read the Tutorials**: Check `Doc/Starting Bfd - Part 1, 2, 3.pdf`
3. **Learn OCL**: See `Doc/ad970808_UML11_OCL.pdf` for the OCL specification
4. **Try Derived Attributes**: Computed values that auto-update
5. **Add Constraints**: Validate your model with OCL constraints

---

## Troubleshooting 🔧

**"Bold.inc not found"**
- Add `Source\Common\Include` to your project's search path

**Components not showing in palette**
- Verify the BPL is installed (Component → Install Packages)
- Check that all dependent packages are loaded in menu Component/Install packages...

**Database errors on first run**
- Use `TBoldCreateDatabaseAction` to create the schema first
- Or call `BoldPersistenceHandleDB1.CreateDataBaseSchema`

---

## Resources 🔗

- **Git Embarcadero original repo**: https://github.com/Embarcadero/BoldForDelphi
- **Git active development**: https://github.com/bero/BoldForDelphi
- **Wiki**: https://delphi.fandom.com/wiki/Bold_for_Delphi
- **Discord**: https://discord.gg/C6frzsn
- **Blog**: http://boldfordelphi.blogspot.com/

---

*Bold for Delphi - Model-Driven Development for Delphi since 2004*
