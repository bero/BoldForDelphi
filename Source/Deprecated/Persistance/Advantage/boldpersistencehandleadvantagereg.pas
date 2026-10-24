
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleAdvantageReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleAdvantage.res}

uses
  Classes,
  BoldDatabaseAdapterAdvantage,
  BoldPersistenceHandleAdvantage,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleAdvantage]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterAdvantage]);
end;

end.
