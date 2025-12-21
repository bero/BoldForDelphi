{$INCLUDE bold.inc}
unit DemoMainForm;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TfrmDemoMain = class(TForm)
    btnCreateDatabase: TButton;
    btnOpenSystem: TButton;
    btnCloseSystem: TButton;
    lblStatus: TLabel;
    pnlButtons: TPanel;
    procedure btnCreateDatabaseClick(Sender: TObject);
    procedure btnOpenSystemClick(Sender: TObject);
    procedure btnCloseSystemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateStatus;
  end;

var
  frmDemoMain: TfrmDemoMain;

implementation

uses
  DemoDataModule;

{$R *.dfm}

procedure TfrmDemoMain.FormCreate(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmDemoMain.btnCreateDatabaseClick(Sender: TObject);
var
  DatabaseName: string;
begin
  DatabaseName := 'BoldDemo';
  if not InputQuery('Create Database', 'Enter database name:', DatabaseName) then
    Exit;
  if DatabaseName.Trim.IsEmpty then
  begin
    ShowMessage('Database name cannot be empty.');
    Exit;
  end;
  try
    Screen.Cursor := crHourGlass;
    try
      dmDemo.EnsureDatabaseExists(DatabaseName);
      dmDemo.CreateDatabaseSchema;
    finally
      Screen.Cursor := crDefault;
    end;
    ShowMessage('Database "' + DatabaseName + '" and schema created successfully.');
  except
    on E: Exception do
      ShowMessage('Error creating database: ' + E.Message);
  end;
end;

procedure TfrmDemoMain.btnOpenSystemClick(Sender: TObject);
begin
  try
    dmDemo.OpenSystem;
    UpdateStatus;
  except
    on E: Exception do
      ShowMessage('Error opening system: ' + E.Message);
  end;
end;

procedure TfrmDemoMain.btnCloseSystemClick(Sender: TObject);
begin
  try
    dmDemo.CloseSystem;
    UpdateStatus;
  except
    on E: Exception do
      ShowMessage('Error closing system: ' + E.Message);
  end;
end;

procedure TfrmDemoMain.UpdateStatus;
begin
  if dmDemo.BoldSystemHandle1.Active then
    lblStatus.Caption := 'System: OPEN'
  else
    lblStatus.Caption := 'System: CLOSED';
end;

end.
