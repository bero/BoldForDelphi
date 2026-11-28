{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit fModelEditorClass;

{ The class dialog of the standalone model editor's LCL client (#128): the
  name of a class, whether it is persistent and abstract, and its
  superclass, which is shown, not changed. It edits the values the shared
  data module gives (TdmModelEditor.ClassValues, NewClassValues); the
  client gives them back to ApplyClassValues or AddClass. OK is enabled
  while the class has a name. }

interface

uses
  SysUtils,
  Classes,
  Forms,
  Controls,
  StdCtrls,
  BoldModelEditorData;

type
  TfrmModelEditorClass = class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    lblSuperclass: TLabel;
    lblSuperclassName: TLabel;
    chkPersistent: TCheckBox;
    chkAbstract: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure edtNameChange(Sender: TObject);
  public
    procedure LoadValues(const AValues: TModelEditorClassValues);
    { The values with the dialog's name, persistent and abstract; the
      superclass stays }
    procedure SaveValues(var AValues: TModelEditorClassValues);
  end;

{ Shows the dialog with AValues; True with the changed values when the user
  presses OK }
function EditClassValues(const ACaption: string; var AValues: TModelEditorClassValues): Boolean;

implementation

{$R *.lfm}

resourcestring
  sNoSuperclass = '(none)';

procedure TfrmModelEditorClass.LoadValues(const AValues: TModelEditorClassValues);
begin
  edtName.Text := AValues.Name;
  if AValues.Superclass <> '' then
    lblSuperclassName.Caption := AValues.Superclass
  else
    lblSuperclassName.Caption := sNoSuperclass;
  chkPersistent.Checked := AValues.Persistent;
  chkAbstract.Checked := AValues.Abstract;
  edtNameChange(edtName);
end;

procedure TfrmModelEditorClass.SaveValues(var AValues: TModelEditorClassValues);
begin
  AValues.Name := Trim(edtName.Text);
  AValues.Persistent := chkPersistent.Checked;
  AValues.Abstract := chkAbstract.Checked;
end;

procedure TfrmModelEditorClass.edtNameChange(Sender: TObject);
begin
  btnOK.Enabled := Trim(edtName.Text) <> '';
end;

function EditClassValues(const ACaption: string; var AValues: TModelEditorClassValues): Boolean;
var
  Dialog: TfrmModelEditorClass;
begin
  Dialog := TfrmModelEditorClass.Create(nil);
  try
    Dialog.Caption := ACaption;
    Dialog.LoadValues(AValues);
    Result := Dialog.ShowModal = mrOK;
    if Result then
      Dialog.SaveValues(AValues);
  finally
    Dialog.Free;
  end;
end;

end.
