unit Test.BoldTestFramework;

{ BoldTestAllocatedBytes and BoldTestAllocatedBlocks, which the leak tests
  read before and after the code under test. A block that is allocated must
  raise them, and freeing it must lower them again; a counter that does not
  move, or moves only one way, would make those leak tests pass or fail
  whatever the code does. On Delphi for Linux the counts come from the
  wrapper around libc's malloc that BoldTestFramework installs, so these tests
  also cover GetMem, AllocMem, ReallocMem and FreeMem through it. Free Pascal
  counts no blocks, so the block tests are Delphi only. }

interface

uses
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldTestFramework = class(TBoldTestCase)
  published
    procedure TestAllocatedBytesFollowGetMemAndFreeMem;
    procedure TestAllocatedBytesFollowReallocMem;
    {$IFNDEF FPC}
    procedure TestAllocatedBlocksFollowGetMemAllocMemAndFreeMem;
    procedure TestAllocatedBlocksUnchangedByReallocMem;
    {$ENDIF}
  end;

implementation

uses
  SysUtils;

const
  cBlockSize = 4000;

procedure TTestBoldTestFramework.TestAllocatedBytesFollowGetMemAndFreeMem;
var
  P: Pointer;
  Before, Held, After: Int64;
begin
  Before := BoldTestAllocatedBytes;
  GetMem(P, cBlockSize);
  try
    Held := BoldTestAllocatedBytes;
  finally
    FreeMem(P);
  end;
  After := BoldTestAllocatedBytes;
  Assert.IsTrue(Held - Before >= cBlockSize,
    string(Format('Holding a block of %d bytes added %d bytes', [cBlockSize, Held - Before])));
  Assert.IsTrue(Held - After >= cBlockSize,
    string(Format('Freeing a block of %d bytes removed %d bytes', [cBlockSize, Held - After])));
end;

procedure TTestBoldTestFramework.TestAllocatedBytesFollowReallocMem;
var
  P: Pointer;
  Before, Grown: Int64;
begin
  GetMem(P, 100);
  try
    Before := BoldTestAllocatedBytes;
    ReallocMem(P, cBlockSize);
    Grown := BoldTestAllocatedBytes;
  finally
    FreeMem(P);
  end;
  // At least half of the growth, whatever the size classes of the two blocks
  Assert.IsTrue(Grown - Before >= cBlockSize div 2,
    string(Format('Growing a block from 100 to %d bytes added %d bytes', [cBlockSize, Grown - Before])));
end;

{$IFNDEF FPC}
procedure TTestBoldTestFramework.TestAllocatedBlocksFollowGetMemAllocMemAndFreeMem;
var
  P, Q: Pointer;
  Before, Held, After: Int64;
begin
  Before := BoldTestAllocatedBlocks;
  GetMem(P, cBlockSize);
  Q := AllocMem(cBlockSize);
  try
    Held := BoldTestAllocatedBlocks;
  finally
    FreeMem(Q);
    FreeMem(P);
  end;
  After := BoldTestAllocatedBlocks;
  Assert.IsTrue(Held - Before >= 2,
    string(Format('GetMem and AllocMem added %d blocks', [Held - Before])));
  Assert.IsTrue(Held - After >= 2,
    string(Format('Freeing the two blocks removed %d blocks', [Held - After])));
end;

procedure TTestBoldTestFramework.TestAllocatedBlocksUnchangedByReallocMem;
var
  P: Pointer;
  Before, After: Int64;
begin
  GetMem(P, 100);
  try
    Before := BoldTestAllocatedBlocks;
    ReallocMem(P, cBlockSize);
    After := BoldTestAllocatedBlocks;
  finally
    FreeMem(P);
  end;
  Assert.AreEqual(Before, After, 'ReallocMem moves a block; it adds or removes none');
end;
{$ENDIF}

initialization
  BoldRegisterTest(TTestBoldTestFramework);

end.
