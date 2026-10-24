
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleSQLDirectReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleSQLDirect.res}

uses
  Classes,
  BoldDatabaseAdapterSQLDirect,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterSQLDirect]);
end;

end.
