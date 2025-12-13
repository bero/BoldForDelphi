unit BoldGUIDUtils;

interface

function BoldCreateGUIDAsString(StripBrackets: Boolean = false): string;
function BoldCreateGUIDWithBracketsAsString: string;

implementation

uses
  SysUtils;

function BoldCreateGUIDWithBracketsAsString: string;
begin
  Result := TGUID.NewGuid.ToString;
end;

function BoldCreateGUIDAsString(StripBrackets: Boolean): string;
begin
  Result := TGUID.NewGuid.ToString;
  if StripBrackets then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

end.
