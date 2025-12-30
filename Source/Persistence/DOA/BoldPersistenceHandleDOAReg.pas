
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleDOAReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleDOA.res}

uses
  Classes,
  BoldPersistenceHandleDOA,
  BoldDatabaseAdapterDOA,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleDOA]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterDOA]);
end;

end.
