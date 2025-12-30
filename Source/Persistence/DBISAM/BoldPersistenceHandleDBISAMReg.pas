
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleDBISAMReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleDBISAM.res}

uses
  Classes,
  BoldDatabaseAdapterDBIsam,
  BoldPersistenceHandleDBISAM,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleDBISAM]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterDBISAM]);
end;

end.
