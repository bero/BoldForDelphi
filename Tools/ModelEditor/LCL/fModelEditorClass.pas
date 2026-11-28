{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit fModelEditorClass;

{ The class dialog of the standalone model editor's LCL client (#128): the
  name of a class, its superclass, and whether it is persistent and
  abstract. The superclass is picked from the names the shared data module
  offers (TdmModelEditor.GetSuperclassNames), "(none)" for none; the root
  class of a boldified model has none to pick, and the box is disabled. It
  edits the values the data module gives (ClassValues, NewClassValues); the
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
    cbSuperclass: TComboBox;
    chkPersistent: TCheckBox;
    chkAbstract: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure edtNameChange(Sender: TObject);
  public
    { ASuperclassNames are the superclasses the box offers, an empty name
      for none }
    procedure LoadValues(const AValues: TModelEditorClassValues; ASuperclassNames: TStrings);
    procedure SaveValues(var AValues: TModelEditorClassValues);
  end;

{ Shows the dialog with AValues; True with the changed values when the user
  presses OK }
function EditClassValues(const ACaption: string; ASuperclassNames: TStrings;
  var AValues: TModelEditorClassValues): Boolean;

{ What the superclass box shows for a superclass name: "(none)" for none }
function SuperclassText(const AName: string): string;

implementation

{$R *.lfm}

resourcestring
  sNoSuperclass = '(none)';

function SuperclassText(const AName: string): string;
begin
  if AName = '' then
    Result := sNoSuperclass
  else
    Result := AName;
end;

procedure TfrmModelEditorClass.LoadValues(const AValues: TModelEditorClassValues; ASuperclassNames: TStrings);
var
  i: Integer;
begin
  edtName.Text := AValues.Name;
  cbSuperclass.Items.Clear;
  for i := 0 to ASuperclassNames.Count - 1 do
    cbSuperclass.Items.Add(SuperclassText(ASuperclassNames[i]));
  // The root class: its superclass (none) shown, nothing to pick
  cbSuperclass.Enabled := cbSuperclass.Items.Count > 0;
  if not cbSuperclass.Enabled then
    cbSuperclass.Items.Add(SuperclassText(AValues.Superclass));
  cbSuperclass.ItemIndex := cbSuperclass.Items.IndexOf(SuperclassText(AValues.Superclass));
  chkPersistent.Checked := AValues.Persistent;
  chkAbstract.Checked := AValues.Abstract;
  edtNameChange(edtName);
end;

procedure TfrmModelEditorClass.SaveValues(var AValues: TModelEditorClassValues);
begin
  AValues.Name := Trim(edtName.Text);
  if cbSuperclass.Enabled and (cbSuperclass.ItemIndex >= 0) then
  begin
    if cbSuperclass.Items[cbSuperclass.ItemIndex] = sNoSuperclass then
      AValues.Superclass := ''
    else
      AValues.Superclass := cbSuperclass.Items[cbSuperclass.ItemIndex];
  end;
  AValues.Persistent := chkPersistent.Checked;
  AValues.Abstract := chkAbstract.Checked;
end;

procedure TfrmModelEditorClass.edtNameChange(Sender: TObject);
begin
  btnOK.Enabled := Trim(edtName.Text) <> '';
end;

function EditClassValues(const ACaption: string; ASuperclassNames: TStrings;
  var AValues: TModelEditorClassValues): Boolean;
var
  Dialog: TfrmModelEditorClass;
begin
  Dialog := TfrmModelEditorClass.Create(nil);
  try
    Dialog.Caption := ACaption;
    Dialog.LoadValues(AValues, ASuperclassNames);
    Result := Dialog.ShowModal = mrOK;
    if Result then
      Dialog.SaveValues(AValues);
  finally
    Dialog.Free;
  end;
end;

end.
