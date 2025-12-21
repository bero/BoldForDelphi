# CreateDatabase Demo

A utility application for creating and initializing Bold for Delphi databases.

## Purpose

This demo serves as a database setup tool for other Bold demos. It allows you to:

- Create a new database on your preferred database engine
- Generate the Bold schema (tables) for the shared demo model
- Test database connectivity by opening/closing the Bold system

## Shared Model

The application uses `DemoDataModule` and `DemoClasses` from the `examples/Delphi/Shared` folder. This model includes:

### Classes
- **Person** - FirstName, LastName, Assets, BirthDate, IsActive, FullName (derived)
- **Building** - Address, ZipCode, City, BuiltYear
- **ResidentialBuilding** (extends Building) - Apartments, MonthlyRent
- **CommercialBuilding** (extends Building) - OfficeSpace, ParkingSpaces
- **Ownership** - Share, AcquiredDate (association class for Person-Building)
- **Project** - Name, Description, StartDate, EndDate, Budget
- **Task** - Title, Priority, IsCompleted, DueDate

### Associations
- Person owns Buildings (via Ownership)
- Person resides in ResidentialBuilding
- Person manages Projects
- Project contains Tasks
- Tasks assigned to Person

## Configuration

Edit `DemoDatabase.ini` to configure your database connection:

```ini
[Database]
Type=MSSQL    ; Options: MSSQL, PostgreSQL, Firebird, SQLite

[MSSQL]
Server=localhost
Database=BoldDemo
OSAuthent=True    ; Use Windows Authentication

[PostgreSQL]
Server=localhost
Database=bolddemo
User=postgres
Password=

[Firebird]
Database=C:\Data\BoldDemo.fdb
User=SYSDBA
Password=masterkey

[SQLite]
Database=BoldDemo.db
```

## Usage

1. Configure `DemoDatabase.ini` for your database engine
2. Run the application
3. Click **Create Database** and enter a database name
4. Click **Open System** to verify the connection
5. Click **Close System** when done

## Files

| File | Description |
|------|-------------|
| `CreateDatabase.dpr` | Main project file |
| `CreateDatabase.dproj` | Delphi project settings |
| `DemoMainForm.pas/dfm` | Main form with buttons |
| `DemoDatabase.ini` | Database configuration |
| `..\..\Shared\DemoDataModule.pas` | Shared data module with Bold components |
| `..\..\Shared\DemoClasses.pas` | Generated model classes |

## Requirements

- Delphi with Bold for Delphi installed
- FireDAC database drivers for your chosen database engine
- Database server running (for MSSQL/PostgreSQL/Firebird)
