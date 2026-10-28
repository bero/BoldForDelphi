/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldRegEx;

// Regular expression matching used by the OCL operations sqlLike,
// sqlLikeCaseInsensitive and regExpMatch, one implementation per compiler (#117).
// Delphi: TRegEx (PCRE). FPC: the RegExpr unit. Note that FPC 3.2.2's RegExpr works on
// byte strings, so case-insensitive matching of non-ASCII letters does not work there.

interface

// True when Text contains a match for Pattern.
function BoldRegExMatch(const Text, Pattern: string; IgnoreCase: Boolean): Boolean;
// S with every regular expression metacharacter escaped, so it matches literally.
function BoldRegExEscape(const S: string): string;

implementation

uses
{$IFDEF FPC}
  RegExpr;
{$ELSE}
  RegularExpressions,
  RegularExpressionsCore;
{$ENDIF}

{$IFDEF FPC}
function BoldRegExMatch(const Text, Pattern: string; IgnoreCase: Boolean): Boolean;
var
  RegExpr: TRegExpr;
begin
  RegExpr := TRegExpr.Create;
  try
    RegExpr.ModifierI := IgnoreCase;
    RegExpr.Expression := Pattern;
    if Text = '' then
    begin
      // TRegExpr (FPC 3.2.2) never matches an empty input; Delphi's TRegEx does
      // ('' LIKE '%'). An empty match after the last character is allowed, so
      // search from position 2 of a one-line-feed input with ^ and $ matching
      // at line boundaries: that is exactly an empty line.
      RegExpr.ModifierM := True;
      RegExpr.InputString := #10;
      Result := RegExpr.ExecPos(2);
    end
    else
      Result := RegExpr.Exec(Text);
  finally
    RegExpr.Free;
  end;
end;

function BoldRegExEscape(const S: string): string;
begin
  Result := QuoteRegExprMetaChars(S);
end;
{$ELSE}
function BoldRegExMatch(const Text, Pattern: string; IgnoreCase: Boolean): Boolean;
begin
  if IgnoreCase then
    Result := TRegEx.IsMatch(Text, Pattern, [roIgnoreCase])
  else
    Result := TRegEx.IsMatch(Text, Pattern, []);
end;

function BoldRegExEscape(const S: string): string;
begin
  Result := TPerlRegEx.EscapeRegExChars(S);
end;
{$ENDIF}

end.
