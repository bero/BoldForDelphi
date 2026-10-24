
{ Global compiler directives }
{$include bold.inc}
unit BoldCondition;

interface

uses
  DB,
  BoldDefs,
  BoldBase,
  BoldStreams,
  BoldXMLStreaming,
  BoldId;

type
  {---Forward declaration of classes---}
  TBoldCondition = class;
  TBoldConditionWithClass = class;
  TBoldSQLCondition = class;
  TBoldTimestampCondition = class;
  TBoldChangePointCondition = class;

  {---TBoldCondition---}
  TBoldCondition = class(TBoldNonRefCountedObject, IBoldStreamable)
  private
    fMaxAnswers: integer;
    fOffset: integer;
    fAvailableAnswers: integer;
  public
    constructor create;
    function GetStreamName: string; virtual; abstract;
    property MaxAnswers: integer read fMaxAnswers write fMaxAnswers;
    property Offset: integer read fOffset write fOffset;
    property AvailableAnswers: integer read fAvailableAnswers write fAvailableAnswers;
  end;

  {---TBoldConditionWithClass---}
  TBoldConditionWithClass = class(TBoldCondition)
  private
    fTopSortedIndex: integer;
    fTime: TBoldTimestampType;
  public
    constructor create;
    function GetStreamName: string; override;
    property TopSortedIndex: integer read fTopSortedIndex write fTopSortedIndex;
    property Time: TBoldTimestampType read fTime write fTime;
  end;

  {---TBoldSQLCondition---}
  TBoldSQLCondition = class(TBoldConditionWithClass)
  private
    fWhereFragment: string;
    fOrderBy: string;
    fParams: TParams;
    fJoinInheritedTables: Boolean;
  public
    constructor Create;
    function GetStreamName: string; override;
    property WhereFragment: string read fWhereFragment write fWhereFragment;
    property OrderBy: string read fOrderBy write fOrderBy;
    property Params: TParams read fParams write fParams;
    property JoinInheritedTables: Boolean read fJoinInheritedTables write fJoinInheritedTables;
  end;

  {---TBoldRawSQLCondition---}
  TBoldRawSQLCondition = class(TBoldConditionWithClass)
  private
    fSQL: string;
    fParams: TParams;
  public
    function GetStreamName: string; override;
    property Params: TParams read fParams write fParams;
    property SQL: string read fSQL write fSQL;
  end;

  {---TBoldTimestampCondition---}
  TBoldTimestampCondition = class(TBoldConditionWithClass)
  private
    fTimestamp: TBoldTimeStampType;
  public
    function GetStreamName: string; override;
    property Timestamp: TBoldTimeStampType read fTimestamp write fTimestamp;
  end;

  {---TBoldChangePointCondition---}
  TBoldChangePointCondition = class(TBoldCondition)
  private
    fIdList: TBoldObjectIdList;
    fStart: TBoldTimestampType;
    fEnd: TBoldTimestampType;
    fMemberIds: TBoldMemberIdList;
  public
    function GetStreamName: string; override;
    property IdList: TBoldObjectIdList read fIdList write fIdList;
    property StartTime: TBoldTimestampType read fStart write fStart;
    property EndTime: TBoldTimestampType read fEnd write fEnd;
    property MemberIdList: TBoldMemberIdList read fMemberIds write fMemberIds;
  end;

  { TBoldXMLConditionWithClassStreamer }
  TBoldXMLConditionStreamer = class(TBoldXMLObjectStreamer)
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLConditionWithClassStreamer }
  TBoldXMLConditionWithClassStreamer = class(TBoldXMLConditionStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

implementation

uses
  classes,
  Contnrs,
  BoldDefaultStreamNames;

const
  BoldNodeName_MaxAnswers = 'MaxAnswers';
  BoldNodeName_Offset = 'Offset';
  BoldNodeName_AvailableAnswers = 'AvailableAnswers';
  BoldNodeName_TopSortedIndex = 'TopSortedIndex';
  BoldNodeName_Time = 'Time';
  BoldNodeName_WhereFragment = 'WhereFragment';
  BoldNodeName_OrderBy = 'OrderBy';
  BoldNodeName_JoinInheritedTables = 'JoinInheritedTables';
  BoldNodeName_Param = 'Param';
  BoldNodeName_Params = 'Params';
  BoldNodeName_Name = 'Name';
  BoldNodeName_DataType = 'DataType';
  BoldNodeName_ParamType = 'ParamType';
  BoldNodeName_Data = 'Data';
  BoldNodeName_Timestamp = 'Timestamp';
  BoldNodeName_IdList = 'IdList';
  BoldNodeName_StartTime = 'StartTime';
  BoldNodeName_EndTime = 'EndTime';
  BoldNodeName_MemberIds = 'MemberIds';

type
  {$IFNDEF BOLD_DELPHI17_OR_LATER}
  TValueBuffer = Pointer;
  {$ENDIF}

  { TBoldXMLSQLConditionStreamer}
  TBoldXMLSQLConditionStreamer = class(TBoldXMLConditionWithClassStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLTimestampConditionStreamer }
  TBoldXMLTimestampConditionStreamer = class(TBoldXMLConditionWithClassStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLChangePointConditionStreamer }
  TBoldXMLChangePointConditionStreamer = class(TBoldXMLConditionStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

const
  SQLConditionName = 'SQLCondition';
  RawSQLConditionName = 'RawSQLCondition';
  ClassConditionName = 'ClassCondition';
  TimestampConditionName = 'TimestampCondition';
  OclConditionName = 'OclCondition';
  ChangePointConditionName = 'ChangePointCondition';

{ TBoldConditionWithClass }

function TBoldConditionWithClass.GetStreamName: string;
begin
  result := ClassConditionName;
end;

constructor TBoldConditionWithClass.create;
begin
  inherited;
  fTime := BOLDMAXTIMESTAMP;
end;

{ TBoldSQLCondition }

constructor TBoldSQLCondition.Create;
begin
  inherited;
  fJoinInheritedTables := true;
end;

function TBoldSQLCondition.GetStreamName: string;
begin
  result := SQLConditionName;
end;

{ TBoldRawSQLCondition }

function TBoldRawSQLCondition.GetStreamName: string;
begin
  Result := RawSQLConditionName;
end;

{ TBoldTimestampCondition }

function TBoldTimestampCondition.GetStreamName: string;
begin
  result := TimestampConditionName;
end;

{ TBoldChangePointCondition }

function TBoldChangePointCondition.GetStreamName: string;
begin
  result := ChangePointConditionName;
end;

{ TBoldXMLConditionStreamer }

procedure TBoldXMLConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  Condition: TBoldCondition;
begin
  inherited;
  Condition := Obj as TBoldCondition;
  Condition.MaxAnswers := Node.ReadSubNodeInteger(BoldNodeName_MaxAnswers);
  Condition.Offset := Node.ReadSubNodeInteger(BoldNodeName_Offset);
  Condition.AvailableAnswers := Node.ReadSubNodeInteger(BoldNodeName_AvailableAnswers);
end;

procedure TBoldXMLConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  Condition: TBoldCondition;
begin
  inherited;
  Condition := Obj as TBoldCondition;
  Node.WriteSubNodeInteger(BoldNodeName_MaxAnswers, Condition.MaxAnswers);
  Node.WriteSubNodeInteger(BoldNodeName_Offset, Condition.Offset);
  Node.WriteSubNodeInteger(BoldNodeName_AvailableAnswers, Condition.AvailableAnswers);
end;

{ TBoldXMLConditionWithClassStreamer }

function TBoldXMLConditionWithClassStreamer.CreateObject: TObject;
begin
  result := TBoldConditionWithClass.create;
end;

function TBoldXMLConditionWithClassStreamer.GetStreamName: string;
begin
  result := ClassConditionName;
end;

procedure TBoldXMLConditionWithClassStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  Condition: TBoldConditionWithClass;
begin
  inherited;
  Condition := Obj as TBoldConditionWithClass;
  Condition.TopSortedIndex := Node.ReadSubNodeInteger(BoldNodeName_TopSortedIndex);
  Condition.Time := Node.ReadSubNodeInteger(BoldNodeName_Time);
end;

procedure TBoldXMLConditionWithClassStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  Condition: TBoldConditionWithClass;
begin
  inherited;
  Condition := Obj as TBoldConditionWithClass;
  Node.WriteSubNodeInteger(BoldNodeName_TopSortedIndex, Condition.TopSortedIndex);
  Node.WriteSubNodeInteger(BoldNodeName_Time, Condition.Time);
end;

{ SQL condition parameters in XML

  TParam.GetData and SetData are not inverse for strings: GetData writes a
  wide string as ANSI bytes while SetData reads the buffer as UTF-16, and
  GetData never writes the terminator of an ANSI string. String values are
  therefore streamed as text; all other types as the bytes GetData produces,
  which is the format older versions wrote for them too. }

const
  cStringParamTypes = [ftString, ftFixedChar, ftWideString, ftFixedWideChar, ftMemo, ftWideMemo, ftGuid];

type
  // TParam.GetData and SetData take a TValueBuffer: a byte array since Delphi
  // XE3, a plain pointer before that and in Free Pascal (see the declaration
  // above). The bytes always live in a byte array.
  TBoldParamBytes = {$IFDEF BOLD_DELPHI17_OR_LATER}TValueBuffer{$ELSE}array of Byte{$ENDIF};

function AsValueBuffer(const ABytes: TBoldParamBytes): TValueBuffer;
begin
  {$IFDEF BOLD_DELPHI17_OR_LATER}
  Result := ABytes;
  {$ELSE}
  Result := Pointer(ABytes);
  {$ENDIF}
end;

function ParamDataAsBytes(AParam: TParam): TBoldAnsiString;
var
  Buffer: TBoldParamBytes;
begin
  Result := '';
  SetLength(Buffer, AParam.GetDataSize);
  if Length(Buffer) = 0 then
    Exit;
  AParam.GetData(AsValueBuffer(Buffer));
  SetString(Result, PAnsiChar(@Buffer[0]), Length(Buffer));
end;

procedure SetParamDataFromBytes(AParam: TParam; const AData: TBoldAnsiString);
var
  Buffer: TBoldParamBytes;
begin
  if AData = '' then
    Exit;
  SetLength(Buffer, Length(AData));
  Move(PAnsiChar(AData)^, Buffer[0], Length(AData));
  AParam.SetData(AsValueBuffer(Buffer));
end;

// Older versions stored string values with GetData too: the ANSI bytes, then
// whatever the buffer held, base64-encoded when that included characters XML
// text cannot carry. The value is the ANSI text before the first #0.
function ReadParamString(DataNode: TBoldXMLNode): string;
var
  ZeroPos: Integer;
begin
  if not DataNode.HasBase64Data then
    Exit(DataNode.ReadString);
  Result := string(DataNode.ReadData);
  ZeroPos := Pos(#0, Result);
  if ZeroPos > 0 then
    SetLength(Result, ZeroPos - 1);
end;

{ TBoldXMLSQLConditionStreamer }

function TBoldXMLSQLConditionStreamer.CreateObject: TObject;
begin
  result := TBoldSQLCondition.create;
end;

function TBoldXMLSQLConditionStreamer.GetStreamName: string;
begin
  result := SQLConditionName;
end;

procedure TBoldXMLSQLConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  anSQLCond: TBoldSQLCondition;
  SubNodes: TObjectList;
  i: Integer;
  aSubNode: TBoldXMLNode;
  ParamsNode: TBoldXMLNode;
  DataNode: TBoldXMLNode;
  aParam: TParam;
begin
  inherited;
  anSQLCond := Obj as TBoldSQLCondition;
  anSQLCond.WhereFragment :=  Node.ReadSubNodeString(BoldNodeName_WhereFragment);
  anSQLCond.OrderBy := Node.ReadSubNodeString(BoldNodeName_OrderBy);
  anSQlCond.JoinInheritedTables := Node.ReadSubNodeBoolean(BoldNodeName_JoinInheritedTables);

  ParamsNode := Node.GetSubNode(BoldNodeName_Params);
  if not ParamsNode.IsNull then
  begin
    anSQLCond.Params := TParams.Create;
    SubNodes := Node.NewSubNodesByTagName(BoldNodeName_Param);
    try
      for i := 0 to SubNodes.Count - 1 do
      begin
        aSubNode := TBoldXMLNode(SubNodes[i]);
        aParam := anSQLCond.Params.Add as TParam;
        aParam.Name := aSubNode.ReadSubNodeString(BoldNodeName_Name);
        aParam.DataType := TFieldType(aSubNode.ReadSubNodeInteger(BoldNodeName_DataType));
        aParam.ParamType := TParamType(aSubNode.ReadSubNodeInteger(BoldNodeName_ParamType));
        DataNode := aSubNode.GetSubNode(BoldNodeName_Data);
        if assigned(DataNode) then
        begin
          // Value keeps the DataType just read.
          if aParam.DataType in cStringParamTypes then
            aParam.Value := ReadParamString(DataNode)
          else
            SetParamDataFromBytes(aParam, DataNode.ReadData);
        end;
        DataNode.Free;
      end;
    finally
      SubNodes.Free;
    end;
  end
  else
    anSQLCond.Params := nil;
  ParamsNode.Free;
end;

procedure TBoldXMLSQLConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  anSQLCond: TBoldSQLCondition;
  i: Integer;
  SubNode: TBoldXMLNode;
  ParamsNode: TBoldXMLNode;
begin
  inherited;
  anSQLCond := Obj as TBoldSQLCondition;
  Node.WriteSubNodeString(BoldNodeName_WhereFragment, anSQLCond.WhereFragment);
  Node.WriteSubNodeString(BoldNodeName_OrderBy, anSQLCond.OrderBy);
  Node.WriteSubNodeBoolean(BoldNodeName_JoinInheritedTables, anSqlCond.JoinInheritedTables);

  ParamsNode := Node.NewSubNode(BoldNodeName_Params);
  if assigned(anSQLCond.Params) then
  begin
    for i := 0 to anSQLCond.Params.Count - 1 do
    begin
      SubNode := ParamsNode.NewSubNode(BoldNodeName_Param);
      SubNode.WriteSubNodeString(BoldNodeName_Name, anSQLCond.Params[i].Name);
      SubNode.WriteSubNodeInteger(BoldNodeName_DataType, Integer(anSQLCond.Params[i].DataType));
      SubNode.WriteSubNodeInteger(BoldNodeName_ParamType, Integer(anSQLCond.Params[i].ParamType));
      if not anSQLCond.Params[i].IsNull then
      begin
        if anSQLCond.Params[i].DataType in cStringParamTypes then
          SubNode.WriteSubNodeString(BoldNodeName_Data, anSQLCond.Params[i].AsWideString)
        else
          SubNode.WriteSubNodeData(BoldNodeName_Data, ParamDataAsBytes(anSQLCond.Params[i]));
      end;
      SubNode.Free;
    end;
  end else
    ParamsNode.SetToNull;
  ParamsNode.Free;
end;

{ TBoldXMLTimestampConditionStreamer }

function TBoldXMLTimestampConditionStreamer.CreateObject: TObject;
begin
  result := TBoldTimestampCondition.create;
end;

function TBoldXMLTimestampConditionStreamer.GetStreamName: string;
begin
  result := TimestampConditionName;
end;

procedure TBoldXMLTimestampConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
begin
  inherited;
  (Obj as TBoldTimestampCondition).Timestamp := Node.ReadSubNodeInteger(BoldNodeName_Timestamp);
end;

procedure TBoldXMLTimestampConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteSubNodeInteger(BoldNodeName_Timestamp, (Obj as TBoldTimestampCondition).Timestamp);
end;

{ TBoldXMLChangePointConditionStreamer }

function TBoldXMLChangePointConditionStreamer.CreateObject: TObject;
begin
  result := TBoldChangePointCondition.Create;
end;

function TBoldXMLChangePointConditionStreamer.GetStreamName: string;
begin
  result := ChangePointConditionName;
end;

procedure TBoldXMLChangePointConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  aCond: TBoldChangePointCondition;
begin
  inherited;
  aCond := Obj as TBoldChangePointCondition;
  aCond.IdList := Node.ReadSubNodeObject(BoldNodeName_IdList, BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  aCond.StartTime := Node.ReadSubNodeInteger(BoldNodeName_StartTime);
  aCond.EndTime := Node.ReadSubNodeInteger(BoldNodeName_EndTime);
  aCond.MemberIdList := Node.ReadSubNodeObject(BoldNodeName_MemberIds, BOLDMEMBERIDLISTNAME) as TBoldMemberIdList;
end;

procedure TBoldXMLChangePointConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  aCond: TBoldChangePointCondition;
begin
  inherited;
  aCond := Obj as TBoldChangePointCondition;
  Node.WriteSubNodeObject(BoldNodeName_IdList, BOLDOBJECTIDLISTNAME, aCond.IdList);
  Node.WriteSubNodeInteger(BoldNodeName_StartTime, aCond.StartTime);
  Node.WriteSubNodeInteger(BoldNodeName_EndTime, aCond.EndTime);
  Node.WriteSubNodeObject(BoldNodeName_MemberIds, BOLDMEMBERIDLISTNAME, aCond.MemberIdList);
end;


{ TBoldCondition }

constructor TBoldCondition.create;
begin
  inherited;
  fMaxAnswers := -1;
  fOffset := -1;
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLConditionWithClassStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLSQLConditionStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLTimestampConditionStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLChangePointConditionStreamer.Create);

end.
