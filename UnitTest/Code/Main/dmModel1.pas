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
  text .dfm through the resource directive, and TDataModule is in Classes. }

uses
  SysUtils{$IFNDEF FPC},
  Forms{$ENDIF};

{$R *.dfm}

procedure Ensuredm_Model;
begin
  // Note: This may be a memory leak - who frees the dm_Model?
  // It was not quite OK to free it in finalization
  if not Assigned(dm_Model1) then
  {$IFDEF FPC}
    // No Application object under FPC; freed in this unit's finalization.
    dm_Model1 := Tdm_Model1.Create(nil);
  {$ELSE}
    dm_Model1 := tdm_Model1.Create(Application);
  {$ENDIF}
end;

procedure Freedm_Model;
begin
  dm_model1.Free;
end;

{$IFDEF FPC}
initialization

finalization
  FreeAndNil(dm_Model1);
{$ENDIF}

end.

