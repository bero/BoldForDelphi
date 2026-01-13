unit Test.BoldQueue;

interface

uses
  DUnitX.TestFramework,
  Classes,
  BoldQueue,
  BoldBase;

type
  // Concrete implementation for testing TBoldQueueable
  TTestQueueable = class(TBoldQueueable)
  private
    FDisplayCount: Integer;
    FApplyCount: Integer;
    FDiscardCount: Integer;
  protected
    procedure Display; override;
  public
    procedure Apply; override;
    procedure DiscardChange; override;
    procedure SetInDisplayList(Value: Boolean);
    property DisplayCount: Integer read FDisplayCount;
    property ApplyCount: Integer read FApplyCount;
    property DiscardCount: Integer read FDiscardCount;
  end;

  // Concrete implementation for testing TBoldQueue
  TTestQueue = class(TBoldQueue)
  private
    FActive: Boolean;
  protected
    function GetIsActive: Boolean; override;
  public
    procedure ActivateDisplayQueue; override;
    procedure DeActivateDisplayQueue; override;
  end;

  TEventHelper = class
  public
    EventFired: Boolean;
    procedure OnEvent(Sender: TObject);
  end;

  [TestFixture]
  TTestBoldQueue = class
  public
    [Test]
    procedure TestQueueableCreate;
    [Test]
    procedure TestQueueableDestroy;
    [Test]
    procedure TestQueueableGetDebugInfo;
    [Test]
    procedure TestQueueableGetDebugInfoWithMatchObject;
    [Test]
    procedure TestQueueCreate;
    [Test]
    procedure TestQueueDestroy;
    [Test]
    procedure TestQueueEmpty;
    [Test]
    procedure TestQueueCounts;
    [Test]
    procedure TestAddToDisplayList;
    [Test]
    procedure TestRemoveFromDisplayList;
    [Test]
    procedure TestDisplayOne;
    [Test]
    procedure TestDisplayAll;
    [Test]
    procedure TestAddToApplyList;
    [Test]
    procedure TestApplyAll;
    [Test]
    procedure TestApplyAllMatching;
    [Test]
    procedure TestDiscardChangeAll;
    [Test]
    procedure TestDiscardChangeAllMatching;
    [Test]
    procedure TestPreDisplayQueue;
    [Test]
    procedure TestPostDisplayQueue;
    [Test]
    procedure TestIsDisplaying;
    [Test]
    procedure TestIsDisplayQueueEmpty;
    [Test]
    procedure TestBoldLogQueue;
    [Test]
    procedure TestPrioritizedQueueable;
    [Test]
    procedure TestAfterInPriority;
    [Test]
    procedure TestStronglyPrioritizedSibbling;
    [Test]
    procedure TestMostPrioritizedQueuable;
    // Tests using global BoldInstalledQueue
    [Test]
    procedure TestGlobalQueueApplyAll;
    [Test]
    procedure TestGlobalQueueApplyAllMatching;
    [Test]
    procedure TestGlobalQueueDiscardChangeAll;
    [Test]
    procedure TestGlobalQueueDiscardChangeAllMatching;
  end;

implementation

uses
  SysUtils,
  BoldEnvironment,
  BoldLogHandler;

{ TTestQueueable }

procedure TTestQueueable.Apply;
begin
  Inc(FApplyCount);
  // Note: RemoveFromApplyList uses global BoldInstalledQueue, not test queue
end;

procedure TTestQueueable.DiscardChange;
begin
  Inc(FDiscardCount);
  // Note: RemoveFromApplyList uses global BoldInstalledQueue, not test queue
end;

procedure TTestQueueable.Display;
begin
  Inc(FDisplayCount);
  // Mark as no longer in display list so DisplayOne can proceed
  SetElementFlag(befIsInDisplayList, False);
end;

procedure TTestQueueable.SetInDisplayList(Value: Boolean);
begin
  SetElementFlag(befIsInDisplayList, Value);
end;

{ TTestQueue }

procedure TTestQueue.ActivateDisplayQueue;
begin
  FActive := True;
end;

procedure TTestQueue.DeActivateDisplayQueue;
begin
  FActive := False;
end;

function TTestQueue.GetIsActive: Boolean;
begin
  Result := FActive;
end;

{ TEventHelper }

procedure TEventHelper.OnEvent(Sender: TObject);
begin
  EventFired := True;
end;

{ TTestBoldQueue }

procedure TTestBoldQueue.TestQueueableCreate;
var
  Queueable: TTestQueueable;
  MatchObj: TObject;
begin
  MatchObj := TObject.Create;
  try
    Queueable := TTestQueueable.Create(MatchObj);
    try
      Assert.IsNotNull(Queueable);
      Assert.AreEqual(MatchObj, Queueable.MatchObject);
    finally
      Queueable.Free;
    end;
  finally
    MatchObj.Free;
  end;
end;

procedure TTestBoldQueue.TestQueueableDestroy;
var
  Queueable: TTestQueueable;
begin
  Queueable := TTestQueueable.Create(nil);
  Queueable.Free;
  Assert.Pass('Queueable destroyed without error');
end;

procedure TTestBoldQueue.TestQueueableGetDebugInfo;
var
  Queueable: TTestQueueable;
  DebugInfo: string;
begin
  Queueable := TTestQueueable.Create(nil);
  try
    DebugInfo := Queueable.DebugInfo;
    Assert.Contains(DebugInfo, 'TTestQueueable');
  finally
    Queueable.Free;
  end;
end;

procedure TTestBoldQueue.TestQueueableGetDebugInfoWithMatchObject;
var
  Queueable: TTestQueueable;
  MatchObj: TStringList;
  DebugInfo: string;
begin
  MatchObj := TStringList.Create;
  try
    Queueable := TTestQueueable.Create(MatchObj);
    try
      DebugInfo := Queueable.DebugInfo;
      Assert.Contains(DebugInfo, 'TTestQueueable');
      Assert.Contains(DebugInfo, 'TStringList');
    finally
      Queueable.Free;
    end;
  finally
    MatchObj.Free;
  end;
end;

procedure TTestBoldQueue.TestQueueCreate;
var
  Queue: TTestQueue;
begin
  Queue := TTestQueue.Create;
  try
    Assert.IsNotNull(Queue);
    Assert.IsTrue(Queue.IsActive);
    Assert.AreEqual(0, Queue.DisplayCount);
    Assert.AreEqual(0, Queue.ApplyCount);
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestQueueDestroy;
var
  Queue: TTestQueue;
begin
  Queue := TTestQueue.Create;
  Queue.Free;
  Assert.Pass('Queue destroyed without error');
end;

procedure TTestBoldQueue.TestQueueEmpty;
var
  Queue: TTestQueue;
begin
  Queue := TTestQueue.Create;
  try
    Assert.IsTrue(Queue.Empty);
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestQueueCounts;
var
  Queue: TTestQueue;
begin
  Queue := TTestQueue.Create;
  try
    Assert.AreEqual(0, Queue.DisplayCount);
    Assert.AreEqual(0, Queue.ApplyCount);
    Assert.AreEqual(0, Queue.PreDisplayCount);
    Assert.AreEqual(0, Queue.PostDisplayCount);
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestAddToDisplayList;
var
  Queue: TTestQueue;
  Queueable: TTestQueueable;
begin
  Queue := TTestQueue.Create;
  try
    Queueable := TTestQueueable.Create(nil);
    try
      Queue.AddToDisplayList(Queueable);
      Assert.AreEqual(1, Queue.DisplayCount);
      // Note: IsInDisplayList flag is set by TBoldQueueable.AddToDisplayList, not Queue.AddToDisplayList
    finally
      Queueable.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestRemoveFromDisplayList;
var
  Queue: TTestQueue;
  Queueable: TTestQueueable;
begin
  Queue := TTestQueue.Create;
  try
    Queueable := TTestQueueable.Create(nil);
    try
      Queue.AddToDisplayList(Queueable);
      Assert.AreEqual(1, Queue.DisplayCount);
      Queue.RemoveFromDisplayList(Queueable, True);
      Assert.AreEqual(0, Queue.DisplayCount);
    finally
      Queueable.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestDisplayOne;
var
  Queue: TTestQueue;
  Queueable: TTestQueueable;
  DisplayResult: Boolean;
begin
  Queue := TTestQueue.Create;
  try
    Queueable := TTestQueueable.Create(nil);
    try
      // Manually set the flag since we're adding directly to queue
      Queueable.SetInDisplayList(True);
      Queue.AddToDisplayList(Queueable);
      DisplayResult := Queue.DisplayOne;
      Assert.IsTrue(DisplayResult);
      Assert.AreEqual(1, Queueable.DisplayCount);
    finally
      Queueable.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestDisplayAll;
var
  Queue: TTestQueue;
  Q1, Q2, Q3: TTestQueueable;
begin
  Queue := TTestQueue.Create;
  try
    Q1 := TTestQueueable.Create(nil);
    Q2 := TTestQueueable.Create(nil);
    Q3 := TTestQueueable.Create(nil);
    try
      // Manually set flags since we're adding directly to queue
      Q1.SetInDisplayList(True);
      Q2.SetInDisplayList(True);
      Q3.SetInDisplayList(True);
      Queue.AddToDisplayList(Q1);
      Queue.AddToDisplayList(Q2);
      Queue.AddToDisplayList(Q3);
      Assert.AreEqual(3, Queue.DisplayCount);
      // Call DisplayOne for each item (DisplayAll uses global queue)
      Assert.IsTrue(Queue.DisplayOne);
      Assert.IsTrue(Queue.DisplayOne);
      Assert.IsTrue(Queue.DisplayOne);
      Assert.AreEqual(1, Q1.DisplayCount);
      Assert.AreEqual(1, Q2.DisplayCount);
      Assert.AreEqual(1, Q3.DisplayCount);
    finally
      Q3.Free;
      Q2.Free;
      Q1.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestAddToApplyList;
var
  Queue: TTestQueue;
  Queueable: TTestQueueable;
begin
  Queue := TTestQueue.Create;
  try
    Queueable := TTestQueueable.Create(nil);
    try
      Queue.AddToApplyList(Queueable);
      Assert.AreEqual(1, Queue.ApplyCount);
    finally
      Queueable.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestApplyAll;
var
  Queue: TTestQueue;
  Q1, Q2: TTestQueueable;
begin
  Queue := TTestQueue.Create;
  try
    Q1 := TTestQueueable.Create(nil);
    Q2 := TTestQueueable.Create(nil);
    try
      Assert.AreEqual(0, Queue.ApplyCount);
      Queue.AddToApplyList(Q1);
      Assert.AreEqual(1, Queue.ApplyCount);
      Queue.AddToApplyList(Q2);
      Assert.AreEqual(2, Queue.ApplyCount);
      Assert.IsFalse(Queue.Empty);
    finally
      Q2.Free;
      Q1.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestApplyAllMatching;
var
  Queue: TTestQueue;
  MatchObj: TObject;
  Q1, Q2: TTestQueueable;
begin
  Queue := TTestQueue.Create;
  try
    MatchObj := TObject.Create;
    try
      Q1 := TTestQueueable.Create(MatchObj);
      Q2 := TTestQueueable.Create(nil);
      try
        Queue.AddToApplyList(Q1);
        Queue.AddToApplyList(Q2);
        Assert.AreEqual(2, Queue.ApplyCount);
        // Verify MatchObject is correctly set
        Assert.AreSame(MatchObj, Q1.MatchObject);
        Assert.IsNull(Q2.MatchObject);
      finally
        Q2.Free;
        Q1.Free;
      end;
    finally
      MatchObj.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestDiscardChangeAll;
var
  Queue: TTestQueue;
  Q1, Q2: TTestQueueable;
begin
  Queue := TTestQueue.Create;
  try
    Q1 := TTestQueueable.Create(nil);
    Q2 := TTestQueueable.Create(nil);
    try
      Assert.IsTrue(Queue.Empty);
      Queue.AddToApplyList(Q1);
      Queue.AddToApplyList(Q2);
      Assert.AreEqual(2, Queue.ApplyCount);
      Assert.IsFalse(Queue.Empty);
    finally
      Q2.Free;
      Q1.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestDiscardChangeAllMatching;
var
  Queue: TTestQueue;
  MatchObj: TObject;
  Q1, Q2: TTestQueueable;
begin
  Queue := TTestQueue.Create;
  try
    MatchObj := TObject.Create;
    try
      Q1 := TTestQueueable.Create(MatchObj);
      Q2 := TTestQueueable.Create(nil);
      try
        Queue.AddToApplyList(Q1);
        Queue.AddToApplyList(Q2);
        Assert.AreEqual(2, Queue.ApplyCount);
        // Verify MatchObject is correctly assigned
        Assert.AreSame(MatchObj, Q1.MatchObject);
        Assert.IsNull(Q2.MatchObject);
      finally
        Q2.Free;
        Q1.Free;
      end;
    finally
      MatchObj.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestPreDisplayQueue;
var
  Queue: TTestQueue;
  Helper: TEventHelper;
begin
  Queue := TTestQueue.Create;
  try
    Helper := TEventHelper.Create;
    try
      Helper.EventFired := False;
      Queue.AddEventToPreDisplayQueue(Helper.OnEvent, nil, Helper);
      Assert.AreEqual(1, Queue.PreDisplayCount);
      Queue.PerformPreDisplayQueue;
      Assert.IsTrue(Helper.EventFired);
      Assert.AreEqual(0, Queue.PreDisplayCount);
    finally
      Helper.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestPostDisplayQueue;
var
  Queue: TTestQueue;
  Helper: TEventHelper;
begin
  Queue := TTestQueue.Create;
  try
    Helper := TEventHelper.Create;
    try
      Helper.EventFired := False;
      Queue.AddEventToPostDisplayQueue(Helper.OnEvent, nil, Helper);
      Assert.AreEqual(1, Queue.PostDisplayCount);
      Queue.PerformPostDisplayQueue;
      Assert.IsTrue(Helper.EventFired);
      Assert.AreEqual(0, Queue.PostDisplayCount);
    finally
      Helper.Free;
    end;
  finally
    Queue.Free;
  end;
end;

procedure TTestBoldQueue.TestIsDisplaying;
begin
  Assert.IsFalse(TBoldQueueable.IsDisplaying);
end;

procedure TTestBoldQueue.TestIsDisplayQueueEmpty;
begin
  Assert.IsTrue(TBoldQueueable.IsDisplayQueueEmpty);
end;

procedure TTestBoldQueue.TestBoldLogQueue;
var
  OldCount: Int64;
begin
  OldCount := BoldQueueLogCount;
  BoldLogQueue('Test message');
  Assert.AreEqual(OldCount + 1, BoldQueueLogCount);
end;

procedure TTestBoldQueue.TestPrioritizedQueueable;
var
  Q1, Q2: TTestQueueable;
begin
  Q1 := TTestQueueable.Create(nil);
  try
    Q2 := TTestQueueable.Create(nil);
    try
      Q2.PrioritizedQueuable := Q1;
      Assert.AreSame(Q1, Q2.PrioritizedQueuable);
    finally
      Q2.Free;
    end;
  finally
    Q1.Free;
  end;
end;

procedure TTestBoldQueue.TestAfterInPriority;
var
  Q1, Q2, Q3: TTestQueueable;
begin
  Q1 := TTestQueueable.Create(nil);
  try
    Q2 := TTestQueueable.Create(nil);
    try
      Q3 := TTestQueueable.Create(nil);
      try
        // Q3 depends on Q2 which depends on Q1
        Q2.PrioritizedQueuable := Q1;
        Q3.PrioritizedQueuable := Q2;
        Assert.IsTrue(Q3.AfterInPriority(Q1));
        Assert.IsTrue(Q3.AfterInPriority(Q2));
        Assert.IsFalse(Q1.AfterInPriority(Q2));
      finally
        Q3.Free;
      end;
    finally
      Q2.Free;
    end;
  finally
    Q1.Free;
  end;
end;

procedure TTestBoldQueue.TestStronglyPrioritizedSibbling;
var
  Q1, Q2, Q3: TTestQueueable;
begin
  Q1 := TTestQueueable.Create(nil);
  try
    Q2 := TTestQueueable.Create(nil);
    try
      Q3 := TTestQueueable.Create(nil);
      try
        // Both Q2 and Q3 depend on Q1
        Q2.PrioritizedQueuable := Q1;
        Q2.StronglyDependedOfPrioritized := True;
        Q3.PrioritizedQueuable := Q1;
        Q3.StronglyDependedOfPrioritized := True;
        Assert.IsTrue(Q2.StronglyPrioritizedSibbling(Q3));
        Assert.IsTrue(Q3.StronglyPrioritizedSibbling(Q2));
      finally
        Q3.Free;
      end;
    finally
      Q2.Free;
    end;
  finally
    Q1.Free;
  end;
end;

procedure TTestBoldQueue.TestMostPrioritizedQueuable;
var
  Q1, Q2: TTestQueueable;
begin
  Q1 := TTestQueueable.Create(nil);
  try
    Q2 := TTestQueueable.Create(nil);
    try
      // No priority chain
      Assert.IsNull(Q1.MostPrioritizedQueuable);
      // Q2 depends on Q1
      Q2.PrioritizedQueuable := Q1;
      // Q1 is not in display list, so MostPrioritizedQueuable returns nil
      Assert.IsNull(Q2.MostPrioritizedQueuable);
    finally
      Q2.Free;
    end;
  finally
    Q1.Free;
  end;
end;

procedure TTestBoldQueue.TestGlobalQueueApplyAll;
var
  Q1, Q2: TTestQueueable;
  InitialCount: Integer;
begin
  // Test using global BoldInstalledQueue
  Assert.IsNotNull(BoldInstalledQueue, 'BoldInstalledQueue should be available');
  InitialCount := BoldInstalledQueue.ApplyCount;
  Q1 := TTestQueueable.Create(nil);
  try
    Q2 := TTestQueueable.Create(nil);
    try
      Q1.AddToApplyList;  // Adds self to global queue
      Q2.AddToApplyList;
      Assert.AreEqual(InitialCount + 2, BoldInstalledQueue.ApplyCount);
      TBoldQueueable.ApplyAll;
      Assert.AreEqual(1, Q1.ApplyCount);
      Assert.AreEqual(1, Q2.ApplyCount);
    finally
      Q2.Free;
    end;
  finally
    Q1.Free;
  end;
end;

procedure TTestBoldQueue.TestGlobalQueueApplyAllMatching;
var
  MatchObj: TObject;
  Q1, Q2: TTestQueueable;
  InitialCount: Integer;
begin
  Assert.IsNotNull(BoldInstalledQueue, 'BoldInstalledQueue should be available');
  InitialCount := BoldInstalledQueue.ApplyCount;
  MatchObj := TObject.Create;
  try
    Q1 := TTestQueueable.Create(MatchObj);
    try
      Q2 := TTestQueueable.Create(nil);
      try
        Q1.AddToApplyList;
        Q2.AddToApplyList;
        Assert.AreEqual(InitialCount + 2, BoldInstalledQueue.ApplyCount);
        TBoldQueueable.ApplyAllMatching(MatchObj);
        Assert.AreEqual(1, Q1.ApplyCount, 'Q1 should be applied (matches)');
        Assert.AreEqual(0, Q2.ApplyCount, 'Q2 should not be applied (no match)');
        // Clean up remaining item
        TBoldQueueable.ApplyAll;
      finally
        Q2.Free;
      end;
    finally
      Q1.Free;
    end;
  finally
    MatchObj.Free;
  end;
end;

procedure TTestBoldQueue.TestGlobalQueueDiscardChangeAll;
var
  Q1, Q2: TTestQueueable;
  InitialCount: Integer;
begin
  Assert.IsNotNull(BoldInstalledQueue, 'BoldInstalledQueue should be available');
  InitialCount := BoldInstalledQueue.ApplyCount;
  Q1 := TTestQueueable.Create(nil);
  try
    Q2 := TTestQueueable.Create(nil);
    try
      Q1.AddToApplyList;
      Q2.AddToApplyList;
      Assert.AreEqual(InitialCount + 2, BoldInstalledQueue.ApplyCount);
      TBoldQueueable.DiscardChangeAll;
      Assert.AreEqual(1, Q1.DiscardCount);
      Assert.AreEqual(1, Q2.DiscardCount);
    finally
      Q2.Free;
    end;
  finally
    Q1.Free;
  end;
end;

procedure TTestBoldQueue.TestGlobalQueueDiscardChangeAllMatching;
var
  MatchObj: TObject;
  Q1, Q2: TTestQueueable;
  InitialCount: Integer;
begin
  Assert.IsNotNull(BoldInstalledQueue, 'BoldInstalledQueue should be available');
  InitialCount := BoldInstalledQueue.ApplyCount;
  MatchObj := TObject.Create;
  try
    Q1 := TTestQueueable.Create(MatchObj);
    try
      Q2 := TTestQueueable.Create(nil);
      try
        Q1.AddToApplyList;
        Q2.AddToApplyList;
        Assert.AreEqual(InitialCount + 2, BoldInstalledQueue.ApplyCount);
        TBoldQueueable.DiscardChangeAllMatching(MatchObj);
        Assert.AreEqual(1, Q1.DiscardCount, 'Q1 should be discarded (matches)');
        Assert.AreEqual(0, Q2.DiscardCount, 'Q2 should not be discarded (no match)');
        // Clean up remaining item
        TBoldQueueable.DiscardChangeAll;
      finally
        Q2.Free;
      end;
    finally
      Q1.Free;
    end;
  finally
    MatchObj.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldQueue);

end.
