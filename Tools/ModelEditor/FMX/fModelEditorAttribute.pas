unit fModelEditorAttribute;

{ The attribute dialog of the standalone model editor's FMX client (#128),
  the FireMonkey twin of ../LCL/fModelEditorAttribute.pas: the name and the
  type of an attribute, and whether it is persistent and derived. The type
  is picked from the model names of the document's type names
  (TdmModelEditor.GetTypeNames) or typed, in a TComboEdit. It edits the
  values the shared data module gives (AttributeValues, NewAttributeValues);
  the client gives them back to ApplyAttributeValues or AddAttribute. OK is
  enabled while the attribute has a name and a type: ValuesChange follows
  the typing (OnChangeTracking), and a text set in code calls it itself. }

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
  FMX.ComboEdit,
  FMX.Controls.Presentation,
  BoldModelEditorData;

type
  TfrmModelEditorAttribute = class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    lblType: TLabel;
    cbType: TComboEdit;
    chkPersistent: TCheckBox;
    chkDerived: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure ValuesChange(Sender: TObject);
  public
    { ATypeNames are the types the type box offers }
    procedure LoadValues(const AValues: TModelEditorAttributeValues; ATypeNames: TStrings);
    procedure SaveValues(var AValues: TModelEditorAttributeValues);
  end;

{ Shows the dialog with AValues and the types ATypeNames; True with the
  changed values when the user presses OK }
function EditAttributeValues(const ACaption: string; ATypeNames: TStrings;
  var AValues: TModelEditorAttributeValues): Boolean;

implementation

{$R *.fmx}

procedure TfrmModelEditorAttribute.LoadValues(const AValues: TModelEditorAttributeValues; ATypeNames: TStrings);
begin
  edtName.Text := AValues.Name;
  cbType.Items.Assign(ATypeNames);
  cbType.Text := AValues.TypeName;
  chkPersistent.IsChecked := AValues.Persistent;
  chkDerived.IsChecked := AValues.Derived;
  ValuesChange(edtName);
end;

procedure TfrmModelEditorAttribute.SaveValues(var AValues: TModelEditorAttributeValues);
begin
  AValues.Name := Trim(edtName.Text);
  AValues.TypeName := Trim(cbType.Text);
  AValues.Persistent := chkPersistent.IsChecked;
  AValues.Derived := chkDerived.IsChecked;
end;

procedure TfrmModelEditorAttribute.ValuesChange(Sender: TObject);
begin
  btnOK.Enabled := (Trim(edtName.Text) <> '') and (Trim(cbType.Text) <> '');
end;

function EditAttributeValues(const ACaption: string; ATypeNames: TStrings;
  var AValues: TModelEditorAttributeValues): Boolean;
var
  Dialog: TfrmModelEditorAttribute;
begin
  Dialog := TfrmModelEditorAttribute.Create(nil);
  try
    Dialog.Caption := ACaption;
    Dialog.LoadValues(AValues, ATypeNames);
    Result := Dialog.ShowModal = mrOk;
    if Result then
      Dialog.SaveValues(AValues);
  finally
    Dialog.Free;
  end;
end;

end.
