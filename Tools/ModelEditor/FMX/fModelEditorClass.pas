unit fModelEditorClass;

{ The class dialog of the standalone model editor's FMX client (#128), the
  FireMonkey twin of ../LCL/fModelEditorClass.pas: the name of a class,
  whether it is persistent and abstract, and its superclass, which is shown,
  not changed. It edits the values the shared data module gives
  (TdmModelEditor.ClassValues, NewClassValues); the client gives them back
  to ApplyClassValues or AddClass. OK is enabled while the class has a name:
  edtNameChange follows the typing (OnChangeTracking), and a text set in
  code calls it itself. }

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation,
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

{$R *.fmx}

resourcestring
  sNoSuperclass = '(none)';

procedure TfrmModelEditorClass.LoadValues(const AValues: TModelEditorClassValues);
begin
  edtName.Text := AValues.Name;
  if AValues.Superclass <> '' then
    lblSuperclassName.Text := AValues.Superclass
  else
    lblSuperclassName.Text := sNoSuperclass;
  chkPersistent.IsChecked := AValues.Persistent;
  chkAbstract.IsChecked := AValues.Abstract;
  edtNameChange(edtName);
end;

procedure TfrmModelEditorClass.SaveValues(var AValues: TModelEditorClassValues);
begin
  AValues.Name := Trim(edtName.Text);
  AValues.Persistent := chkPersistent.IsChecked;
  AValues.Abstract := chkAbstract.IsChecked;
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
    Result := Dialog.ShowModal = mrOk;
    if Result then
      Dialog.SaveValues(AValues);
  finally
    Dialog.Free;
  end;
end;

end.
