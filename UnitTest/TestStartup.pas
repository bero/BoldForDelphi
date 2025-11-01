unit TestStartup;

interface

procedure Run;

implementation

uses
  DUnitX.Loggers.GUI.VCL,
  DUnitX.TestFramework,
  System.SysUtils,
  Vcl.Dialogs,
  Vcl.Forms;

procedure Run;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    try
      Application.Run;
    except
      on E: Exception do
        ShowMessage(E.ClassName + ': ' + E.Message);
    end;
  finally
    Application.ShowHint := false;
    Application.DestroyComponents;
  end;
end;

end.
