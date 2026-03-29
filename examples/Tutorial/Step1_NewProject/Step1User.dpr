program Step1User;

uses
  Vcl.Forms,
  Step1FormUser in 'Step1FormUser.pas' {Form21};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm21, Form21);
  Application.Run;
end.
