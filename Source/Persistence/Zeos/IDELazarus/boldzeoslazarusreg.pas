
{ Global compiler directives }
{$include bold.inc}
unit BoldZeosLazarusReg;

{ Puts TBoldDatabaseAdapterZeos on the Lazarus component palette (#117), on
  the page Bold Persistence, next to TBoldPersistenceHandleDB, which uses it.
  It is in the Lazarus design-time package dclBoldZeos
  (packages/Lazarus/dclBoldZeos.lpk), apart from dclBold so that dclBold does
  not need Zeos.

  The palette icon is the bitmap of Bold's general database adapter
  (Images/Components/TBoldDatabaseAdapterDB.bmp) as PNG, in
  boldzeoslazarusreg.res; ../../../Common/IDELazarus/images/make_res.sh makes
  that file. }

interface

procedure Register;

implementation

{$R boldzeoslazarusreg.res}

uses
  Classes,
  BoldIDEConsts,
  BoldDatabaseAdapterZeos;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterZeos]);
end;

end.
