unit dmModel1;

interface

uses
  Classes,
  BoldHandles,
  BoldSubscription,
  BoldModel,
  BoldHandle,
  BoldAbstractModel;

type
  Tdm_Model1 = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dm_Model1: Tdm_Model1 = nil;

procedure Ensuredm_Model;
procedure Freedm_Model;

implementation

{ The same .pas and .dfm serve Delphi and Free Pascal: both compilers embed the
  text .dfm through the resource directive, and TDataModule is in Classes.
  Delphi on Windows gives the data module to the VCL Application. Free Pascal
  and Delphi on other platforms have no Application here, so this unit's
  finalization frees it. }

{$IF defined(MSWINDOWS) and not defined(FPC)}
  {$DEFINE DM_MODEL1_OWNED_BY_APPLICATION}
{$IFEND}

uses
  SysUtils{$IFDEF DM_MODEL1_OWNED_BY_APPLICATION},
  Forms{$ENDIF};

{$R *.dfm}

procedure Ensuredm_Model;
begin
  // Note: This may be a memory leak - who frees the dm_Model?
  // It was not quite OK to free it in finalization
  if not Assigned(dm_Model1) then
  {$IFDEF DM_MODEL1_OWNED_BY_APPLICATION}
    dm_Model1 := tdm_Model1.Create(Application);
  {$ELSE}
    // No Application object here; freed in this unit's finalization.
    dm_Model1 := Tdm_Model1.Create(nil);
  {$ENDIF}
end;

procedure Freedm_Model;
begin
  dm_model1.Free;
end;

{$IFNDEF DM_MODEL1_OWNED_BY_APPLICATION}
initialization

finalization
  FreeAndNil(dm_Model1);
{$ENDIF}

end.

