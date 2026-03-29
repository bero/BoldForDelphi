program Step2User;

uses
  Vcl.Forms,
  Step2FormUser in 'Step2FormUser.pas' {Form21};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm21, Form21);
  Application.Run;
end.
