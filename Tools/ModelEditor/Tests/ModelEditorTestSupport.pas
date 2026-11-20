{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit ModelEditorTestSupport;

{ What the model editor's test units share: the repository's files, the
  text of a file, and a temporary folder per test. The same source for the
  Delphi and the Free Pascal test programs. }

interface

{ A file of the repository, by its path from the repository's root with /:
  the first folder above the test program that holds it }
function RepositoryFile(const ARelativePath: string): string;

{ A file's text with its line breaks as LF and without a UTF-8 byte order
  mark, so that a file checked out with CRLF and one with LF compare equal }
function TextOf(const AFileName: string): string;

{ A new empty folder in the system's temporary folder; DeleteFolder removes
  it with its files }
function NewTempFolder(const APrefix: string): string;
procedure DeleteFolder(const AFolder: string);

implementation

uses
  SysUtils,
  Classes
  {$IFNDEF FPC}, System.IOUtils{$ENDIF};

function RepositoryFile(const ARelativePath: string): string;
var
  Folder, Parent: string;
begin
  Folder := ExcludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ParamStr(0))));
  repeat
    Result := IncludeTrailingPathDelimiter(Folder) +
      StringReplace(ARelativePath, '/', PathDelim, [rfReplaceAll]);
    if FileExists(Result) then
      Exit;
    Parent := ExcludeTrailingPathDelimiter(ExtractFilePath(Folder));
    if (Parent = Folder) or (Parent = '') then
      Break;
    Folder := Parent;
  until False;
  raise Exception.CreateFmt('%s not found above %s', [ARelativePath, ParamStr(0)]);
end;

function TextOf(const AFileName: string): string;
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Bytes, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Bytes[0], Stream.Size);
  finally
    Stream.Free;
  end;
  Result := TEncoding.UTF8.GetString(Bytes);
  if (Result <> '') and (Result[1] = #$FEFF) then
    Delete(Result, 1, 1);
  Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
end;

function TempFolder: string;
begin
  {$IFDEF FPC}
  Result := GetTempDir;
  {$ELSE}
  Result := TPath.GetTempPath;
  {$ENDIF}
end;

function NewTempFolder(const APrefix: string): string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := IncludeTrailingPathDelimiter(TempFolder) + APrefix + '-' +
    Copy(GUIDToString(GUID), 2, 36);
  ForceDirectories(Result);
end;

procedure DeleteFolder(const AFolder: string);
var
  Search: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(AFolder) + '*', faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Attr and faDirectory) = 0 then
        SysUtils.DeleteFile(IncludeTrailingPathDelimiter(AFolder) + Search.Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(AFolder);
end;

end.
