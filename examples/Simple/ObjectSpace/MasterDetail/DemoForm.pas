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
    pnlTop: TPanel;
    Label1: TLabel;
    BoldLabel1: TBoldLabel;
    grdProjects: TBoldGrid;
    bnProjects: TBoldNavigator;
    btnClear: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    Splitter1: TSplitter;
    pnlBottom: TPanel;
    pnlBottomLeft: TPanel;
    Label3: TLabel;
    grdTasks: TBoldGrid;
    bnProjectTasks: TBoldNavigator;
    Splitter2: TSplitter;
    pnlBottomRight: TPanel;
    Label2: TLabel;
    BoldGrid1: TBoldGrid;
    bnTasks: TBoldNavigator;
    btnSave: TButton;
    Button1: TButton;
    pnlStatus: TPanel;
    lblConfigFile: TLabel;
    lblDatabaseStatus: TLabel;
    lblBoldStatus: TLabel;
    lhaTasks: TBoldListHandle;
    lhaProjects: TBoldListHandle;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    lhaProjectTasks: TBoldListHandle;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BoldActivateSystemAction1SystemClosed(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure pnlTopResize(Sender: TObject);
    procedure pnlBottomLeftResize(Sender: TObject);
    procedure pnlBottomRightResize(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateStatusLabels;
    procedure AutoSizeGridColumns(Grid: TBoldGrid);
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

procedure TMainForm.AutoSizeGridColumns(Grid: TBoldGrid);
var
  i: Integer;
  AvailableWidth: Integer;
  FlexibleColumns: Integer;
  FlexWidth: Integer;
begin
  if Grid.ColCount <= 1 then
    Exit;

  // Calculate available width (subtract scrollbar and indicator column)
  AvailableWidth := Grid.ClientWidth - Grid.ColWidths[0] - 4;

  // Distribute width evenly among data columns
  FlexibleColumns := Grid.ColCount - 1;  // Exclude indicator column
  if FlexibleColumns > 0 then
  begin
    FlexWidth := AvailableWidth div FlexibleColumns;
    for i := 1 to Grid.ColCount - 1 do
      Grid.ColWidths[i] := FlexWidth;
  end;
end;

procedure TMainForm.pnlTopResize(Sender: TObject);
begin
  AutoSizeGridColumns(grdProjects);
end;

procedure TMainForm.pnlBottomLeftResize(Sender: TObject);
begin
  AutoSizeGridColumns(grdTasks);
end;

procedure TMainForm.pnlBottomRightResize(Sender: TObject);
begin
  AutoSizeGridColumns(BoldGrid1);
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

  // Check database/persistence connection
  try
    if dmDemo.Connected then
    begin
      lblDatabaseStatus.Caption := 'Persistence: Connected (' + dmDemo.DatabaseName + ')';
      lblDatabaseStatus.Font.Color := clSuccess;
    end
    else
    begin
      lblDatabaseStatus.Caption := 'Persistence: Not connected';
      lblDatabaseStatus.Font.Color := clError;
    end;
  except
    on E: Exception do
    begin
      lblDatabaseStatus.Caption := 'Persistence: Error - ' + E.Message;
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
