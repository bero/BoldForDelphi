unit Test.BoldDirectoryTraverser;

{ TBoldDirectoryTraverser walks a directory tree and reports each directory
  and file. The files here all have an extension, because the default mask is
  '*.*', which on POSIX file systems matches only names with a dot. }

interface

uses
  SysUtils,
  Classes,
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldDirectoryTraverser = class(TBoldTestCase)
  private
    fRoot: string;
    fVisited: TStringList;
    procedure HandleFile(CurrentDirectory: string; CurrentFile: TSearchRec);
    procedure WriteFile(const FileName: string);
    procedure Traverse(PostOrder: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMissingPathRaises;
    procedure TestPreOrderVisitsFilesBeforeSubdirectories;
    procedure TestPostOrderVisitsSubdirectoriesFirst;
    procedure TestTraversalDoesNotLeak;
    {$IF defined(MSWINDOWS) and not defined(FPC)}
    procedure TestTraversalReleasesFindHandles;
    {$IFEND}
  end;

implementation

uses
  {$IF defined(MSWINDOWS) and not defined(FPC)}
  Windows,
  {$IFEND}
  BoldDefs,
  BoldDirectoryTraverser;

procedure TTestBoldDirectoryTraverser.SetUp;
begin
  inherited;
  fVisited := TStringList.Create;
  fRoot := IncludeTrailingPathDelimiter(BoldTestNewTempDir('BoldTraverser'));
  WriteFile(fRoot + 'top.txt');
  ForceDirectories(fRoot + 'sub');
  WriteFile(fRoot + 'sub' + PathDelim + 'inner.txt');
end;

procedure TTestBoldDirectoryTraverser.TearDown;
begin
  BoldTestDeleteTree(fRoot);
  fVisited.Free;
  inherited;
end;

procedure TTestBoldDirectoryTraverser.WriteFile(const FileName: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('content');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

procedure TTestBoldDirectoryTraverser.HandleFile(CurrentDirectory: string; CurrentFile: TSearchRec);
begin
  fVisited.Add(CurrentFile.Name);
end;

procedure TTestBoldDirectoryTraverser.Traverse(PostOrder: Boolean);
var
  Traverser: TBoldDirectoryTraverser;
begin
  Traverser := TBoldDirectoryTraverser.Create(nil);
  try
    Traverser.Path := fRoot;
    Traverser.OnFile := HandleFile;
    if PostOrder then
      Traverser.TraverseAlgorithm := taPostOrder
    else
      Traverser.TraverseAlgorithm := taPreOrder;
    Traverser.Execute;
  finally
    Traverser.Free;
  end;
end;

procedure TTestBoldDirectoryTraverser.TestMissingPathRaises;
var
  Traverser: TBoldDirectoryTraverser;
  Raised: Boolean;
begin
  Traverser := TBoldDirectoryTraverser.Create(nil);
  try
    Traverser.Path := fRoot + 'missing';
    Raised := False;
    try
      Traverser.Execute;
    except
      on EBold do
        Raised := True;
    end;
    Assert.IsTrue(Raised, 'a path that does not exist must raise EBold');
  finally
    Traverser.Free;
  end;
end;

procedure TTestBoldDirectoryTraverser.TestPreOrderVisitsFilesBeforeSubdirectories;
begin
  Traverse(False);
  Assert.AreEqual('top.txt,inner.txt', fVisited.CommaText);
end;

procedure TTestBoldDirectoryTraverser.TestPostOrderVisitsSubdirectoriesFirst;
begin
  Traverse(True);
  Assert.AreEqual('inner.txt,top.txt', fVisited.CommaText);
end;

// Each directory and file search must be closed. Under Free Pascal an open
// search holds heap memory; on Windows it holds a find handle, which a heap
// count cannot see.
procedure TTestBoldDirectoryTraverser.TestTraversalDoesNotLeak;
var
  i: Integer;
  Before, After: Int64;
begin
  Traverse(False); // warm up one-time allocations
  Before := {$IFDEF FPC}BoldTestAllocatedBytes{$ELSE}BoldTestAllocatedBlocks{$ENDIF};
  for i := 1 to 20 do
    Traverse(i mod 2 = 0);
  fVisited.Clear;
  After := {$IFDEF FPC}BoldTestAllocatedBytes{$ELSE}BoldTestAllocatedBlocks{$ENDIF};
  Assert.IsTrue(After <= Before, Format('20 traversals left %d %s',
    [After - Before, {$IFDEF FPC}'bytes'{$ELSE}'blocks'{$ENDIF}]));
end;

{$IF defined(MSWINDOWS) and not defined(FPC)}
// On Windows an open search holds a find handle, which the heap count above
// cannot see. One traversal of the test tree runs four searches, so 20
// traversals that never closed them would leave 80 handles; the margin leaves
// room for handles that other threads open meanwhile.
procedure TTestBoldDirectoryTraverser.TestTraversalReleasesFindHandles;
var
  i: Integer;
  Before, After: DWORD;
begin
  Traverse(False); // warm up one-time handles
  Assert.IsTrue(GetProcessHandleCount(GetCurrentProcess, Before), 'GetProcessHandleCount failed');
  for i := 1 to 20 do
  begin
    Traverse(i mod 2 = 0);
  end;
  Assert.IsTrue(GetProcessHandleCount(GetCurrentProcess, After), 'GetProcessHandleCount failed');
  Assert.IsTrue(Integer(After) - Integer(Before) < 20,
    Format('20 traversals left %d handles open', [Integer(After) - Integer(Before)]));
end;
{$IFEND}

initialization
  BoldRegisterTest(TTestBoldDirectoryTraverser);

end.
