{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit Test.BoldTemplateExpander;

{ BoldTemplateExpander, which the code generator expands its templates
  with. A line that ends in a backslash continues on the next one: the
  backslash and the line break go. The expanded text has the line breaks of
  the platform, LF under Free Pascal on Linux; the expander only removed a
  backslash before CRLF, so the code generated there kept the backslash
  lines (#128). }

interface

uses
  BoldTestFramework;

type
  TTestBoldTemplateExpander = class(TBoldTestCase)
  published
    procedure TestABackslashJoinsTheLines;
    procedure TestABackslashJoinsCRLFLines;
  end;

implementation

uses
  SysUtils,
  Classes,
  BoldTemplateExpander;

procedure TTestBoldTemplateExpander.TestABackslashJoinsTheLines;
var
  Holder: TBoldTemplateHolder;
begin
  Holder := TBoldTemplateHolder.Create(nil);
  try
    Holder.Template.Add('first \');
    Holder.Template.Add('second');
    Holder.Template.Add('third');
    Assert.AreEqual(2, Holder.ExpandedTemplate.Count, 'two lines: ' + Holder.ExpandedTemplate.CommaText);
    Assert.AreEqual('first second', Holder.ExpandedTemplate[0], 'the first line, joined with the second');
    Assert.AreEqual('third', Holder.ExpandedTemplate[1], 'the third line');
  finally
    Holder.Free;
  end;
end;

procedure TTestBoldTemplateExpander.TestABackslashJoinsCRLFLines;
var
  Variables: TBoldTemplateVariables;
begin
  Variables := TBoldTemplateVariables.Create;
  try
    Assert.AreEqual('first second'#13#10'third',
      BoldExpandTemplate('first \'#13#10'second'#13#10'third', Variables), 'CRLF line breaks');
  finally
    Variables.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldTemplateExpander);

end.
