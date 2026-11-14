
{ Global compiler directives }
{$include bold.inc}
unit BoldZeosLazarusReg;

{ Puts TBoldDatabaseAdapterZeos on the Lazarus component palette (#117), on
  the page Bold Persistence, next to TBoldPersistenceHandleDB, which uses it.
  It is in the Lazarus design-time package dclBoldZeos
  (packages/Lazarus/dclBoldZeos.lpk), apart from dclBold so that dclBold does
  not need Zeos. A project that gets the component requires the runtime
  package BoldZeos, not this design-time package (BoldLazarusRequirements).

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
  ComponentEditors,
  BoldIDEConsts,
  BoldLazarusRequirements,
  BoldDatabaseAdapterZeos;

type
  TBoldDclBoldZeosNotRequired = class(TBoldRuntimeOnlyRequirements)
  public
    class function DesignPackageName: AnsiString; override;
  end;

class function TBoldDclBoldZeosNotRequired.DesignPackageName: AnsiString;
begin
  Result := 'dclBoldZeos';
end;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterZeos]);
  // A project that gets it requires the package BoldZeos, not dclBoldZeos
  RegisterComponentRequirements(TBoldDatabaseAdapterZeos, TBoldDclBoldZeosNotRequired);
end;

end.
