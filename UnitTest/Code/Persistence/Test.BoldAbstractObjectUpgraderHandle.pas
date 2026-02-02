unit Test.BoldAbstractObjectUpgraderHandle;

interface

uses
  DUnitX.TestFramework,
  BoldId,
  BoldDbInterfaces,
  BoldAbstractObjectUpgrader,
  BoldAbstractObjectUpgraderHandle;

type
  // Concrete implementation of TBoldAbstractObjectUpgrader for testing
  TTestObjectUpgrader = class(TBoldAbstractObjectUpgrader)
  public
    procedure UpgradeObjectById(ObjectId: TBoldObjectId; Query: IBoldQuery); override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure FailTransaction; override;
  end;

  // Concrete implementation of TBoldAbstractObjectUpgraderHandle for testing
  TTestObjectUpgraderHandle = class(TBoldAbstractObjectUpgraderHandle)
  protected
    function CreateObjectUpgrader: TBoldAbstractObjectUpgrader; override;
  end;

  /// <summary>
  /// Test fixture for BoldAbstractObjectUpgraderHandle - Object upgrader handle component
  /// </summary>
  [TestFixture]
  [Category('Persistence')]
  TTestBoldAbstractObjectUpgraderHandle = class
  public
    [Test]
    [Category('Quick')]
    procedure TestHandleCreate;
    [Test]
    procedure TestHandleDestroy;
    [Test]
    procedure TestGetConfig_LazyInitialization;
    [Test]
    procedure TestGetConfig_ReturnsSameInstance;
    [Test]
    procedure TestConfigClass_ReturnsDefault;
    [Test]
    procedure TestGetObjectUpgrader_LazyInitialization;
    [Test]
    procedure TestGetObjectUpgrader_ReturnsSameInstance;
    [Test]
    procedure TestGetHandledObject_ReturnsObjectUpgrader;
    [Test]
    procedure TestSetConfig_CopiesItems;
    [Test]
    procedure TestSetConfig_ClearsExisting;
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

{ TTestObjectUpgrader }

procedure TTestObjectUpgrader.EndTransaction;
begin
  // No-op for testing
end;

procedure TTestObjectUpgrader.FailTransaction;
begin
  // No-op for testing
end;

procedure TTestObjectUpgrader.StartTransaction;
begin
  // No-op for testing
end;

procedure TTestObjectUpgrader.UpgradeObjectById(ObjectId: TBoldObjectId; Query: IBoldQuery);
begin
  // No-op for testing
end;

{ TTestObjectUpgraderHandle }

function TTestObjectUpgraderHandle.CreateObjectUpgrader: TBoldAbstractObjectUpgrader;
begin
  Result := TTestObjectUpgrader.Create(Config);
end;

{ TTestBoldAbstractObjectUpgraderHandle }

procedure TTestBoldAbstractObjectUpgraderHandle.TestHandleCreate;
var
  Handle: TTestObjectUpgraderHandle;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    Assert.IsNotNull(Handle);
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestHandleDestroy;
var
  Handle: TTestObjectUpgraderHandle;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  // Access Config and ObjectUpgrader to ensure they are created
  Handle.Config.Add;
  Assert.IsNotNull(Handle.ObjectUpgrader);
  Handle.Free;
  Assert.Pass('Handle destroyed without error');
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestGetConfig_LazyInitialization;
var
  Handle: TTestObjectUpgraderHandle;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    // Config should be created lazily when first accessed
    Assert.IsNotNull(Handle.Config);
    Assert.AreEqual(0, Handle.Config.Count);
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestGetConfig_ReturnsSameInstance;
var
  Handle: TTestObjectUpgraderHandle;
  Config1, Config2: TBoldObjectUpgraderConfiguration;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    Config1 := Handle.Config;
    Config2 := Handle.Config;
    Assert.AreSame(Config1, Config2, 'Config should return same instance');
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestConfigClass_ReturnsDefault;
var
  Handle: TTestObjectUpgraderHandle;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    // Config should be of type TBoldObjectUpgraderConfiguration
    Assert.IsTrue(Handle.Config is TBoldObjectUpgraderConfiguration);
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestGetObjectUpgrader_LazyInitialization;
var
  Handle: TTestObjectUpgraderHandle;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    // ObjectUpgrader should be created lazily when first accessed
    Assert.IsNotNull(Handle.ObjectUpgrader);
    Assert.IsTrue(Handle.ObjectUpgrader is TTestObjectUpgrader);
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestGetObjectUpgrader_ReturnsSameInstance;
var
  Handle: TTestObjectUpgraderHandle;
  Upgrader1, Upgrader2: TBoldAbstractObjectUpgrader;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    Upgrader1 := Handle.ObjectUpgrader;
    Upgrader2 := Handle.ObjectUpgrader;
    Assert.AreSame(Upgrader1, Upgrader2, 'ObjectUpgrader should return same instance');
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestGetHandledObject_ReturnsObjectUpgrader;
var
  Handle: TTestObjectUpgraderHandle;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    Assert.AreSame(Handle.ObjectUpgrader, Handle.HandledObject);
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestSetConfig_CopiesItems;
var
  Handle: TTestObjectUpgraderHandle;
  SourceConfig: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    // First access Config to ensure it's initialized (lazy init)
    Assert.IsNotNull(Handle.Config);

    SourceConfig := TBoldObjectUpgraderConfiguration.Create(nil);
    try
      Item := SourceConfig.Add as TBoldObjectUpgraderConfigurationItem;
      Item.ExpressionName := 'TestExpr';
      Item.UpgradeOlderThanVersion := 5;

      Handle.Config := SourceConfig;

      Assert.AreEqual(1, Handle.Config.Count);
      Assert.AreEqual('TestExpr', Handle.Config[0].ExpressionName);
      Assert.AreEqual(5, Handle.Config[0].UpgradeOlderThanVersion);
    finally
      SourceConfig.Free;
    end;
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgraderHandle.TestSetConfig_ClearsExisting;
var
  Handle: TTestObjectUpgraderHandle;
  SourceConfig: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Handle := TTestObjectUpgraderHandle.Create(nil);
  try
    // Add existing items
    Item := Handle.Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.ExpressionName := 'ExistingExpr';
    Item.UpgradeOlderThanVersion := 1;

    Assert.AreEqual(1, Handle.Config.Count);

    // Create new source config
    SourceConfig := TBoldObjectUpgraderConfiguration.Create(nil);
    try
      Item := SourceConfig.Add as TBoldObjectUpgraderConfigurationItem;
      Item.ExpressionName := 'NewExpr1';
      Item.UpgradeOlderThanVersion := 10;

      Item := SourceConfig.Add as TBoldObjectUpgraderConfigurationItem;
      Item.ExpressionName := 'NewExpr2';
      Item.UpgradeOlderThanVersion := 20;

      // SetConfig should clear existing and copy new
      Handle.Config := SourceConfig;

      Assert.AreEqual(2, Handle.Config.Count);
      Assert.AreEqual('NewExpr1', Handle.Config[0].ExpressionName);
      Assert.AreEqual('NewExpr2', Handle.Config[1].ExpressionName);
    finally
      SourceConfig.Free;
    end;
  finally
    Handle.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldAbstractObjectUpgraderHandle);

end.
