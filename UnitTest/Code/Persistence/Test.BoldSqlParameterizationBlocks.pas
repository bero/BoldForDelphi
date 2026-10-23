unit Test.BoldSqlParameterizationBlocks;

{ Parameterized id-list SQL beyond a single fetch block and across the UNION
  branches of a child-mapped association.

  Model: Owner 1 -- 0..n Item, where Item is abstract and child-mapped
  (Bold.TableMapping=Children) with the concrete subclasses ItemA and ItemB.
  The embedded single link Item.owner therefore lives in two tables, so the
  multilink fetch of Owner.items UNIONs one branch per table and divides
  FetchBlockSize by that branch count (250 div 2 = 125 owners per block).

  The suite's default engine config caps parameterized id lists at 20, so
  every test raises MaxParamsInIdList to the SQL Server value first. The
  executed SQL is read back through the capture spy of
  Test.BoldSqlParameterization. }

interface

uses
  DUnitX.TestFramework,
  BoldSystem,
  BoldLogHandler,
  BoldTestCaseFireDAC,
  Test.BoldSqlParameterization;

type
  [TestFixture]
  [Category('PMapper')]
  TTestBoldSqlParameterizationBlocks = class(TBoldTestCaseFireDAC)
  private
    FCapture: TSQLCaptureLogHandler;
    FSavedHandler: TBoldLogHandler;
    procedure BeginCapture;
    procedure EndCapture;
    procedure RaiseMaxParamsInIdList;
    function CreateOwner(AIndex: integer): TBoldObject;
    function CreateItem(const AClassName: string; AOwner: TBoldObject; AValue: integer): TBoldObject;
    function ItemsOf(AOwner: TBoldObject): TBoldObjectList;
    procedure AssertMemberFetched(AObject: TBoldObject; const AMemberName, AMessage: string);
  protected
    procedure ConfigureModel; override;
  public
    [Test]
    procedure MultilinkFetch_ChildMapped_UnionsSubclassTablesWithParams;
    [Test]
    procedure MultilinkFetch_CrossesBlockBoundary;
    [Test]
    procedure ExactIdFetch_CrossesBlockBoundary;
    [Test]
    procedure Delete_CrossesBlockBoundary;
  end;

implementation

uses
  System.SysUtils,
  BoldDefs,
  BoldId,
  BoldDBInterfaces,
  BoldPersistenceControllerDefault;

{ TTestBoldSqlParameterizationBlocks }

procedure TTestBoldSqlParameterizationBlocks.ConfigureModel;
begin
  LoadModelFromStrings([
    'VERSION 19',
    '(Model',
    #9'"BlockFetchModel"',
    #9'"BusinessClassesRoot"',
    #9'""',
    #9'""',
    #9'"_Boldify.boldified=True,Bold.DelphiName=<Name>,Bold.UnitName=BlockFetchModel,Bold.RootClass=BusinessClassesRoot"',
    #9'(Classes',
    #9#9'(Class',
    #9#9#9'"BusinessClassesRoot"',
    #9#9#9'"<NONE>"',
    #9#9#9'TRUE',
    #9#9#9'FALSE',
    #9#9#9'""',
    #9#9#9'""',
    #9#9#9'"_Boldify.autoCreated=True,persistence=persistent,Bold.TableName=<Prefix>_OBJECT"',
    #9#9#9'(Attributes',
    #9#9#9')',
    #9#9#9'(Methods',
    #9#9#9')',
    #9#9')',
    #9#9'(Class',
    #9#9#9'"Owner"',
    #9#9#9'"BusinessClassesRoot"',
    #9#9#9'TRUE',
    #9#9#9'FALSE',
    #9#9#9'""',
    #9#9#9'""',
    #9#9#9'"persistence=persistent"',
    #9#9#9'(Attributes',
    #9#9#9#9'(Attribute',
    #9#9#9#9#9'"OwnerName"',
    #9#9#9#9#9'"String"',
    #9#9#9#9#9'FALSE',
    #9#9#9#9#9'""',
    #9#9#9#9#9'""',
    #9#9#9#9#9'2',
    #9#9#9#9#9'""',
    #9#9#9#9#9'"persistence=Persistent"',
    #9#9#9#9')',
    #9#9#9')',
    #9#9#9'(Methods',
    #9#9#9')',
    #9#9')',
    #9#9'(Class',
    #9#9#9'"Item"',
    #9#9#9'"BusinessClassesRoot"',
    #9#9#9'TRUE',
    #9#9#9'TRUE',
    #9#9#9'""',
    #9#9#9'""',
    #9#9#9'"persistence=persistent,Bold.TableMapping=Children"',
    #9#9#9'(Attributes',
    #9#9#9#9'(Attribute',
    #9#9#9#9#9'"ItemValue"',
    #9#9#9#9#9'"Integer"',
    #9#9#9#9#9'FALSE',
    #9#9#9#9#9'""',
    #9#9#9#9#9'""',
    #9#9#9#9#9'2',
    #9#9#9#9#9'""',
    #9#9#9#9#9'"persistence=Persistent"',
    #9#9#9#9')',
    #9#9#9')',
    #9#9#9'(Methods',
    #9#9#9')',
    #9#9')',
    #9#9'(Class',
    #9#9#9'"ItemA"',
    #9#9#9'"Item"',
    #9#9#9'TRUE',
    #9#9#9'FALSE',
    #9#9#9'""',
    #9#9#9'""',
    #9#9#9'"persistence=persistent"',
    #9#9#9'(Attributes',
    #9#9#9')',
    #9#9#9'(Methods',
    #9#9#9')',
    #9#9')',
    #9#9'(Class',
    #9#9#9'"ItemB"',
    #9#9#9'"Item"',
    #9#9#9'TRUE',
    #9#9#9'FALSE',
    #9#9#9'""',
    #9#9#9'""',
    #9#9#9'"persistence=persistent"',
    #9#9#9'(Attributes',
    #9#9#9')',
    #9#9#9'(Methods',
    #9#9#9')',
    #9#9')',
    #9')',
    #9'(Associations',
    #9#9'(Association',
    #9#9#9'"owneritems"',
    #9#9#9'"<NONE>"',
    #9#9#9'""',
    #9#9#9'""',
    #9#9#9'"persistence=Persistent,_Boldify.noName=True,Bold.DelphiName=<Name>"',
    #9#9#9'FALSE',
    #9#9#9'(Roles',
    #9#9#9#9'(Role',
    #9#9#9#9#9'"items"',
    #9#9#9#9#9'TRUE',
    #9#9#9#9#9'FALSE',
    #9#9#9#9#9'"Owner"',
    #9#9#9#9#9'""',
    #9#9#9#9#9'"0..n"',
    #9#9#9#9#9'""',
    #9#9#9#9#9'0',
    #9#9#9#9#9'2',
    #9#9#9#9#9'0',
    #9#9#9#9#9'"Bold.Embed=False"',
    #9#9#9#9#9'(Qualifiers',
    #9#9#9#9#9')',
    #9#9#9#9')',
    #9#9#9#9'(Role',
    #9#9#9#9#9'"owner"',
    #9#9#9#9#9'TRUE',
    #9#9#9#9#9'FALSE',
    #9#9#9#9#9'"Item"',
    #9#9#9#9#9'""',
    #9#9#9#9#9'"0..1"',
    #9#9#9#9#9'""',
    #9#9#9#9#9'0',
    #9#9#9#9#9'2',
    #9#9#9#9#9'0',
    #9#9#9#9#9'""',
    #9#9#9#9#9'(Qualifiers',
    #9#9#9#9#9')',
    #9#9#9#9')',
    #9#9#9')',
    #9#9')',
    #9')',
    ')']);
end;

procedure TTestBoldSqlParameterizationBlocks.BeginCapture;
begin
  FCapture := TSQLCaptureLogHandler.Create;
  FSavedHandler := BoldSQLLogHandler;
  BoldSQLLogHandler := FCapture;
end;

procedure TTestBoldSqlParameterizationBlocks.EndCapture;
begin
  BoldSQLLogHandler := FSavedHandler;
  FreeAndNil(FCapture);
end;

procedure TTestBoldSqlParameterizationBlocks.RaiseMaxParamsInIdList;
begin
  // Must be re-applied after every RefreshSystem (fresh persistence mapper).
  (System.PersistenceController as TBoldPersistenceControllerDefault).
    PersistenceMapper.SQLDataBaseConfig.MaxParamsInIdList := 500;
end;

function TTestBoldSqlParameterizationBlocks.CreateOwner(AIndex: integer): TBoldObject;
begin
  result := CreateObject('Owner');
  SetAttributeAsString(result, 'OwnerName', 'O' + IntToStr(AIndex));
end;

function TTestBoldSqlParameterizationBlocks.CreateItem(const AClassName: string;
  AOwner: TBoldObject; AValue: integer): TBoldObject;
begin
  result := CreateObject(AClassName);
  SetAttributeAsInteger(result, 'ItemValue', AValue);

  if Assigned(AOwner) then
  begin
    (result.BoldMemberByExpressionName['owner'] as TBoldObjectReference).BoldObject := AOwner;
  end;
end;

function TTestBoldSqlParameterizationBlocks.ItemsOf(AOwner: TBoldObject): TBoldObjectList;
begin
  result := AOwner.BoldMemberByExpressionName['items'] as TBoldObjectList;
end;

procedure TTestBoldSqlParameterizationBlocks.AssertMemberFetched(AObject: TBoldObject;
  const AMemberName, AMessage: string);
begin
  // Reading the member would lazily fetch it and hide a block that was never
  // fetched, so the persistence state is checked before any value is touched.
  Assert.IsTrue(AObject.BoldMemberByExpressionName[AMemberName].BoldPersistenceState = bvpsCurrent,
    AMessage);
end;

procedure TTestBoldSqlParameterizationBlocks.MultilinkFetch_ChildMapped_UnionsSubclassTablesWithParams;
const
  cOwnerCount = 10;
var
  vOwners: TBoldObjectList;
  vOwner: TBoldObject;
  vItems: TBoldObjectList;
  vClassA, vClassB: integer;
  i, j: integer;
begin
  for i := 1 to cOwnerCount do
  begin
    vOwner := CreateOwner(i);
    CreateItem('ItemA', vOwner, i);
    CreateItem('ItemB', vOwner, i + 1000);
  end;

  UpdateDatabase;
  RefreshSystem;
  RaiseMaxParamsInIdList;
  vOwners := System.ClassByExpressionName['Owner'];
  Assert.AreEqual(cOwnerCount, vOwners.Count, 'Owner extent');
  BeginCapture;
  try
    System.FetchLinksWithObjects(vOwners, 'items');
    Assert.IsTrue(FCapture.ContainsText('UNION'),
      'A child-mapped other end must be fetched with one UNION branch per subclass table: ' +
      FCapture.CapturedText);
    Assert.IsTrue(FCapture.ContainsText(':ID1'),
      'The UNION branches must bind the owner ids as parameters: ' + FCapture.CapturedText);
    Assert.AreEqual(1, FCapture.DistinctStatementCount('UNION'),
      'Ten owners fit one block, so exactly one parameterized UNION text is expected');
  finally
    EndCapture;
  end;

  for i := 0 to vOwners.Count - 1 do
  begin
    AssertMemberFetched(vOwners[i], 'items', 'items of every owner must come back from the bulk fetch');
    vItems := ItemsOf(vOwners[i]);
    Assert.AreEqual(2, vItems.Count, 'Each owner has one ItemA and one ItemB');
    vClassA := 0;
    vClassB := 0;

    for j := 0 to vItems.Count - 1 do
    begin
      if vItems[j].BoldClassTypeInfo.ExpressionName = 'ItemA' then
      begin
        Inc(vClassA);
      end
      else if vItems[j].BoldClassTypeInfo.ExpressionName = 'ItemB' then
      begin
        Inc(vClassB);
      end;
    end;

    Assert.AreEqual(1, vClassA, 'One item from the ItemA branch');
    Assert.AreEqual(1, vClassB, 'One item from the ItemB branch');
  end;
end;

procedure TTestBoldSqlParameterizationBlocks.MultilinkFetch_CrossesBlockBoundary;
const
  // 2 subclass tables -> FetchBlockSize 250 div 2 = 125 owners per block ->
  // blocks of 125, 125 and 1.
  cOwnerCount = 251;
var
  vOwners: TBoldObjectList;
  vItems: TBoldObjectList;
  i: integer;
begin
  for i := 1 to cOwnerCount do
  begin
    CreateItem('ItemA', CreateOwner(i), i);
  end;

  UpdateDatabase;
  RefreshSystem;
  RaiseMaxParamsInIdList;
  vOwners := System.ClassByExpressionName['Owner'];
  Assert.AreEqual(cOwnerCount, vOwners.Count, 'Owner extent');
  BeginCapture;
  try
    System.FetchLinksWithObjects(vOwners, 'items');
    Assert.IsTrue(FCapture.ContainsText(':ID1'), 'Blocks must be parameterized: ' + FCapture.CapturedText);
    // Both full blocks pad to the 250 bucket and share one text; the single
    // trailing owner takes the equality form.
    Assert.AreEqual(2, FCapture.DistinctStatementCount('UNION'),
      'Two full blocks must share one padded text, the one-id tail its own');
  finally
    EndCapture;
  end;

  for i := 0 to vOwners.Count - 1 do
  begin
    AssertMemberFetched(vOwners[i], 'items',
      'items of owner ' + GetAttributeAsString(vOwners[i], 'OwnerName') +
      ' must come back from its block, not from a lazy refetch');
    vItems := ItemsOf(vOwners[i]);
    Assert.AreEqual(1, vItems.Count, 'Each owner has one item');
    Assert.AreEqual('O' + IntToStr(GetAttributeAsInteger(vItems[0], 'ItemValue')),
      GetAttributeAsString(vOwners[i], 'OwnerName'),
      'Every block must bind its own owner ids, not those of the previous block');
  end;
end;

procedure TTestBoldSqlParameterizationBlocks.ExactIdFetch_CrossesBlockBoundary;
const
  // FetchBlockSize 250 -> blocks of 250 and 50.
  cItemCount = 300;
var
  vItems: TBoldObjectList;
  vSum: Int64;
  i: integer;
begin
  for i := 1 to cItemCount do
  begin
    CreateItem('ItemA', nil, i);
  end;

  UpdateDatabase;
  RefreshSystem;
  RaiseMaxParamsInIdList;
  vItems := System.ClassByExpressionName['ItemA'];
  Assert.AreEqual(cItemCount, vItems.Count, 'ItemA extent');
  BeginCapture;
  try
    System.FetchMembersWithObjects(vItems, TBoldMemberIdList(nil));
    Assert.IsTrue(FCapture.ContainsText(':ID250'),
      'The full block must be parameterized up to :ID250: ' + FCapture.CapturedText);
    Assert.AreEqual(2, FCapture.DistinctStatementCount(':ID1'),
      'One text for the full 250 block and one for the 50-id tail');
  finally
    EndCapture;
  end;

  vSum := 0;

  for i := 0 to vItems.Count - 1 do
  begin
    AssertMemberFetched(vItems[i], 'ItemValue', 'every item must be fetched by its block');
    Inc(vSum, GetAttributeAsInteger(vItems[i], 'ItemValue'));
  end;

  Assert.AreEqual(Int64(cItemCount) * (cItemCount + 1) div 2, vSum,
    'Both blocks must deliver their own ids'' values');
end;

procedure TTestBoldSqlParameterizationBlocks.Delete_CrossesBlockBoundary;
const
  // FetchBlockSize 250 -> DELETE blocks of 250 and 10.
  cItemCount = 260;
var
  vItems: TBoldObjectList;
  vDeletes: string;
  i: integer;
begin
  for i := 1 to cItemCount do
  begin
    CreateItem('ItemA', nil, i);
  end;

  UpdateDatabase;
  RefreshSystem;
  RaiseMaxParamsInIdList;
  vItems := System.ClassByExpressionName['ItemA'];
  Assert.AreEqual(cItemCount, vItems.Count, 'ItemA extent');
  System.FetchMembersWithObjects(vItems, TBoldMemberIdList(nil));

  for i := vItems.Count - 1 downto 0 do
  begin
    vItems[i].Delete;
  end;

  BeginCapture;
  try
    UpdateDatabase;
    vDeletes := FCapture.StatementsContaining('DELETE FROM');
    Assert.IsTrue(vDeletes <> '', 'Deleting saved objects must execute DELETE statements');
    Assert.IsTrue(Pos(':ID250', vDeletes) > 0,
      'The full block must be deleted with a parameterized 250-id list: ' + vDeletes);
    // Two tables (ItemA and the root table), each hit by the padded 250 block
    // and the 10-id tail.
    Assert.AreEqual(4, FCapture.DistinctStatementCount('DELETE FROM'),
      'Two tables x two block shapes: ' + vDeletes);
  finally
    EndCapture;
  end;

  RefreshSystem;
  Assert.AreEqual(0, System.ClassByExpressionName['ItemA'].Count,
    'Every block must be deleted, the tail block with its own ids');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSqlParameterizationBlocks);

end.
