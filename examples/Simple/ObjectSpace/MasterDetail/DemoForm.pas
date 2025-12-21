{$INCLUDE bold.inc}
unit DemoForm;

interface

uses
  // VCL
  System.Classes,
  System.Actions,
  System.SysUtils,
  Vcl.ActnList,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.StdCtrls,

  // Bold
  BoldAbstractListHandle,
  BoldActions,
  BoldCursorHandle,
  BoldDBActions,
  BoldElements,
  BoldGrid,
  BoldHandle,
  BoldHandleAction,
  BoldHandles,
  BoldListBox,
  BoldListHandle,
  BoldNavigator,
  BoldNavigatorDefs,
  BoldRootedHandles,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle, BoldEdit, BoldLabel;

type
  TMainForm = class(TForm)
    grdTasks: TBoldGrid;
    Label1: TLabel;
    bnProjectTasks: TBoldNavigator;
    lhaTasks: TBoldListHandle;
    lhaProjects: TBoldListHandle;
    btnSave: TButton;
    Label3: TLabel;
    ActionList1: TActionList;
    Button1: TButton;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    pnlStatus: TPanel;
    lblDatabaseStatus: TLabel;
    lblBoldStatus: TLabel;
    lblConfigFile: TLabel;
    bnProjects: TBoldNavigator;
    btnClear: TButton;
    BoldLabel1: TBoldLabel;
    grdProjects: TBoldGrid;
    BoldGrid1: TBoldGrid;
    lhaProjectTasks: TBoldListHandle;
    bnTasks: TBoldNavigator;
    Label2: TLabel;
    lblInfo: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BoldActivateSystemAction1SystemClosed(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateStatusLabels;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.UITypes,
  DemoDataModule,
  DemoClasses;

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Connect the action to the system handle
  BoldActivateSystemAction1.BoldSystemHandle := dmDemo.BoldSystemHandle1;

  // Activate Bold system automatically
  dmDemo.BoldSystemHandle1.Active := True;
end;

procedure TMainForm.UpdateStatusLabels;
const
  clSuccess = $00008800;  // Dark green
  clError = $000000CC;    // Dark red
  clWarning = $000066AA;  // Dark orange
  clNeutral = $00606060;  // Gray
begin
  // Safety check
  if not Assigned(dmDemo) then
  begin
    lblConfigFile.Caption := 'Config: Data module not initialized';
    lblDatabaseStatus.Caption := 'Database: Not available';
    lblBoldStatus.Caption := 'Bold System: Not available';
    Exit;
  end;

  // Show config file path
  lblConfigFile.Caption := 'Config: ' + dmDemo.ConfigFile;

  // Check if config file exists
  if not FileExists(dmDemo.ConfigFile) then
  begin
    lblDatabaseStatus.Caption := 'Database: Config file not found!';
    lblDatabaseStatus.Font.Color := clError;
    lblBoldStatus.Caption := 'Bold System: Not available';
    lblBoldStatus.Font.Color := clError;
    Exit;
  end;

  // Check database connection
  try
    if dmDemo.FDConnection1.Connected then
    begin
      lblDatabaseStatus.Caption := 'Database: Connected (' + dmDemo.FDConnection1.Params.Database + ')';
      lblDatabaseStatus.Font.Color := clSuccess;
    end
    else
    begin
      lblDatabaseStatus.Caption := 'Database: Not connected';
      lblDatabaseStatus.Font.Color := clError;
    end;
  except
    on E: Exception do
    begin
      lblDatabaseStatus.Caption := 'Database: Error - ' + E.Message;
      lblDatabaseStatus.Font.Color := clError;
    end;
  end;

  // Check Bold system status
  var isBoldActive := dmDemo.BoldSystemHandle1.Active;
  if isBoldActive then
  begin
    lblBoldStatus.Caption := 'Bold System: Active';
    lblBoldStatus.Font.Color := clSuccess;
  end
  else
  begin
    lblBoldStatus.Caption := 'Bold System: Inactive';
    lblBoldStatus.Font.Color := clWarning;
  end;

  btnSave.Enabled := isBoldActive;
  btnClear.Enabled := isBoldActive;
end;

procedure TMainForm.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  // Initialize counters from existing data
  TProject.InitializeCounter(dmDemo.BoldSystemHandle1.System);
  TTask.InitializeCounter(dmDemo.BoldSystemHandle1.System);
  UpdateStatusLabels;
end;

procedure TMainForm.BoldActivateSystemAction1SystemClosed(Sender: TObject);
begin
  UpdateStatusLabels;
end;

procedure TMainForm.btnAddClick(Sender: TObject);
begin
  TProject.Create(dmDemo.BoldSystemHandle1.System);
end;

procedure TMainForm.btnDeleteClick(Sender: TObject);
var
  Project: TProject;
begin
  if Assigned(lhaProjects.CurrentBoldObject) then
  begin
    Project := lhaProjects.CurrentBoldObject as TProject;
    if MessageDlg('Delete project "' + Project.Name + '" and all its tasks?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      Project.Delete;
  end;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
var
  List: TBoldList;
  i: Integer;
begin
  if MessageDlg('Really delete all projects?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    List := dmDemo.BoldSystemHandle1.System.EvaluateExpressionAsNewElement('Project.allInstances') as TBoldList;
    try
      for i := List.Count - 1 downto 0 do
        (List[i] as TBoldObject).Delete;
    finally
      List.Free;
    end;
  end;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  try
    dmDemo.BoldSystemHandle1.UpdateDatabase;
  except
    BoldRaiseLastFailure(dmDemo.BoldSystemHandle1.System, 'SaveToDatabase', 'Update failed');
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
  if dmDemo.BoldSystemHandle1.Active then
    if dmDemo.BoldSystemHandle1.System.DirtyObjects.Count > 0 then
      case MessageDlg('There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: dmDemo.BoldSystemHandle1.System.UpdateDatabase;
        mrNo: dmDemo.BoldSystemHandle1.System.Discard;
        mrCancel: CanClose := False;
      end;
end;

end.
