unit Test.BoldAbstractObjectUpgrader;

interface

uses
  DUnitX.TestFramework,
  BoldId,
  BoldDbInterfaces,
  BoldAbstractObjectUpgrader;

type
  // Concrete implementation for testing TBoldAbstractObjectUpgrader
  TTestObjectUpgrader = class(TBoldAbstractObjectUpgrader)
  public
    procedure UpgradeObjectById(ObjectId: TBoldObjectId; Query: IBoldQuery); override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure FailTransaction; override;
  end;

  /// <summary>
  /// Test fixture for BoldAbstractObjectUpgrader - Object upgrader configuration
  /// </summary>
  [TestFixture]
  [Category('PMapper')]
  TTestBoldAbstractObjectUpgrader = class
  public
    // TBoldObjectUpgraderConfigurationItem tests
    [Test]
    [Category('Quick')]
    procedure TestConfigItemCreate;
    [Test]
    procedure TestConfigItemExpressionName;
    [Test]
    procedure TestConfigItemUpgradeOlderThanVersion;
    [Test]
    procedure TestConfigItemAssign;
    [Test]
    procedure TestConfigItemGetDisplayName_Empty;
    [Test]
    procedure TestConfigItemGetDisplayName_WithName;
    [Test]
    procedure TestConfigItemGetConfig;

    // TBoldObjectUpgraderConfiguration tests
    [Test]
    procedure TestConfigCreate;
    [Test]
    procedure TestConfigAddItems;
    [Test]
    procedure TestConfigGetItems;
    [Test]
    procedure TestConfigItemByName;
    [Test]
    procedure TestConfigItemByName_NotFound;
    [Test]
    procedure TestConfigGetConfigOwner;

    // TBoldAbstractObjectUpgrader tests
    [Test]
    procedure TestUpgraderCreate;
    [Test]
    procedure TestNeedsManualUpdate_ItemNotFound;
    [Test]
    procedure TestNeedsManualUpdate_VersionOlder;
    [Test]
    procedure TestNeedsManualUpdate_VersionNewer;
    [Test]
    procedure TestNeedsManualUpdate_VersionEqual;
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

{ TTestBoldAbstractObjectUpgrader }

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemCreate;
var
  Config: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Assert.IsNotNull(Item);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemExpressionName;
var
  Config: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.ExpressionName := 'TestExpression';
    Assert.AreEqual('TestExpression', Item.ExpressionName);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemUpgradeOlderThanVersion;
var
  Config: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.UpgradeOlderThanVersion := 5;
    Assert.AreEqual(5, Item.UpgradeOlderThanVersion);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemAssign;
var
  Config1, Config2: TBoldObjectUpgraderConfiguration;
  Item1, Item2: TBoldObjectUpgraderConfigurationItem;
begin
  // Use two separate configs to avoid unique name conflict
  Config1 := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Config2 := TBoldObjectUpgraderConfiguration.Create(nil);
    try
      Item1 := Config1.Add as TBoldObjectUpgraderConfigurationItem;
      Item1.ExpressionName := 'SourceExpr';
      Item1.UpgradeOlderThanVersion := 10;

      Item2 := Config2.Add as TBoldObjectUpgraderConfigurationItem;
      Item2.Assign(Item1);

      Assert.AreEqual('SourceExpr', Item2.ExpressionName);
      Assert.AreEqual(10, Item2.UpgradeOlderThanVersion);
    finally
      Config2.Free;
    end;
  finally
    Config1.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemGetDisplayName_Empty;
var
  Config: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
  DisplayName: string;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.UpgradeOlderThanVersion := 3;
    DisplayName := Item.DisplayName;
    // Should contain 'Unassigned' since ExpressionName is empty
    Assert.Contains(DisplayName, '3');
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemGetDisplayName_WithName;
var
  Config: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
  DisplayName: string;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.ExpressionName := 'MyClass';
    Item.UpgradeOlderThanVersion := 7;
    DisplayName := Item.DisplayName;
    Assert.Contains(DisplayName, 'MyClass');
    Assert.Contains(DisplayName, '7');
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemGetConfig;
var
  Config: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Assert.AreSame(Config, Item.Config);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigCreate;
var
  Config: TBoldObjectUpgraderConfiguration;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Assert.IsNotNull(Config);
    Assert.AreEqual(0, Config.Count);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigAddItems;
var
  Config: TBoldObjectUpgraderConfiguration;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Config.Add;
    Config.Add;
    Config.Add;
    Assert.AreEqual(3, Config.Count);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigGetItems;
var
  Config: TBoldObjectUpgraderConfiguration;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.ExpressionName := 'TestItem';
    Assert.AreSame(Item, Config[0]);
    Assert.AreEqual('TestItem', Config[0].ExpressionName);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemByName;
var
  Config: TBoldObjectUpgraderConfiguration;
  Item, Found: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.ExpressionName := 'FindMe';
    Item.UpgradeOlderThanVersion := 5;

    Found := Config.ItemByName['FindMe'];
    Assert.IsNotNull(Found);
    Assert.AreSame(Item, Found);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigItemByName_NotFound;
var
  Config: TBoldObjectUpgraderConfiguration;
  Found: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Found := Config.ItemByName['NonExistent'];
    Assert.IsNull(Found);
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestConfigGetConfigOwner;
var
  Owner: TComponent;
  Config: TBoldObjectUpgraderConfiguration;
begin
  Owner := TComponent.Create(nil);
  try
    Config := TBoldObjectUpgraderConfiguration.Create(Owner);
    try
      Assert.AreSame(Owner, Config.ConfigOwner);
    finally
      Config.Free;
    end;
  finally
    Owner.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestUpgraderCreate;
var
  Config: TBoldObjectUpgraderConfiguration;
  Upgrader: TTestObjectUpgrader;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Upgrader := TTestObjectUpgrader.Create(Config);
    try
      Assert.IsNotNull(Upgrader);
      Assert.AreSame(Config, Upgrader.Config);
    finally
      Upgrader.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestNeedsManualUpdate_ItemNotFound;
var
  Config: TBoldObjectUpgraderConfiguration;
  Upgrader: TTestObjectUpgrader;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Upgrader := TTestObjectUpgrader.Create(Config);
    try
      // No item configured, should return False
      Assert.IsFalse(Upgrader.NeedsManualUpdate('NonExistent', 1));
    finally
      Upgrader.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestNeedsManualUpdate_VersionOlder;
var
  Config: TBoldObjectUpgraderConfiguration;
  Upgrader: TTestObjectUpgrader;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.ExpressionName := 'TestClass';
    Item.UpgradeOlderThanVersion := 5;

    Upgrader := TTestObjectUpgrader.Create(Config);
    try
      // Version 3 is older than 5, needs update
      Assert.IsTrue(Upgrader.NeedsManualUpdate('TestClass', 3));
    finally
      Upgrader.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestNeedsManualUpdate_VersionNewer;
var
  Config: TBoldObjectUpgraderConfiguration;
  Upgrader: TTestObjectUpgrader;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.ExpressionName := 'TestClass';
    Item.UpgradeOlderThanVersion := 5;

    Upgrader := TTestObjectUpgrader.Create(Config);
    try
      // Version 7 is newer than 5, no update needed
      Assert.IsFalse(Upgrader.NeedsManualUpdate('TestClass', 7));
    finally
      Upgrader.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestBoldAbstractObjectUpgrader.TestNeedsManualUpdate_VersionEqual;
var
  Config: TBoldObjectUpgraderConfiguration;
  Upgrader: TTestObjectUpgrader;
  Item: TBoldObjectUpgraderConfigurationItem;
begin
  Config := TBoldObjectUpgraderConfiguration.Create(nil);
  try
    Item := Config.Add as TBoldObjectUpgraderConfigurationItem;
    Item.ExpressionName := 'TestClass';
    Item.UpgradeOlderThanVersion := 5;

    Upgrader := TTestObjectUpgrader.Create(Config);
    try
      // Version 5 equals threshold, no update needed
      Assert.IsFalse(Upgrader.NeedsManualUpdate('TestClass', 5));
    finally
      Upgrader.Free;
    end;
  finally
    Config.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldAbstractObjectUpgrader);

end.
