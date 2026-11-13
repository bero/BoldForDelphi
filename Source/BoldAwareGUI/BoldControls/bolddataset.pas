
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldDataSet;

interface

uses
  Classes,
  {$IFNDEF FPC}
  Generics.Collections,
  {$ENDIF}
  BoldDefs,
  DB,
  BoldSystem,
  BoldSystemRT,
  BoldElements,
  BoldSubscription,
  BoldAbstractListHandle,
  BoldListListControlPack,
  BoldControlPack,
  BoldControllerListControlPack,
  BoldStringControlPack,
  BoldListHandleFollower;

type
{$IFDEF FPC}
  { Free Pascal's TDataSet passes record buffers as TRecordBuffer (PAnsiChar)
    where Delphi's passes TRecBuf (NativeInt), and field and bookmark data as
    untyped pointers where Delphi's uses TValueBuffer and TBookmark (#117).
    The aliases let the shared code read the same for both compilers; the
    pointer overrides below are thin wrappers around it. }
  TRecBuf = TRecordBuffer;
  TValueBuffer = array of Byte;
{$ENDIF}
  { The list TDataSet.GetFieldList fills: a TList on Free Pascal }
  TBoldDataSetFieldList = {$IFDEF FPC}Classes.TList{$ELSE}TList<TField>{$ENDIF};

  TBDSBookmarkInfo = record
    Flag: TBookmarkFlag;
    Element: TBoldElement;
  end;

  PBDSBookmarkInfo = ^TBDSBookmarkInfo;

  TBoldAbstractDataSet = class;
  TBoldDataSet = class;
  TBoldDataSetFieldDescription = class;
  TBoldDataSetFieldDescriptions = class;

  { TBoldDataSetFieldDescription }
  TBoldDataSetFieldDescription = class(TCollectionItem)
  private
    fRTInfo: TBoldMemberRTInfo;
    fBoldProperties: TBoldStringFollowerController;
    fBufferSize: integer;
    fFieldName: string;
    fFieldOfs: integer;
    fFieldSize: integer;
    fFieldType: TFieldType;
    fRequired: boolean;
    fUseFieldSize: boolean;
    fInvalid: Boolean;
    function GetDataSet: TBoldAbstractDataSet;
    function GetExpression: string;
    function GetFieldDescriptions: TBoldDataSetFieldDescriptions;
    function GetListElementType: TBoldElementTypeInfo;
    procedure SetBoldProperties(AValue: TBoldStringFollowerController);
    procedure SetExpression(AValue: string);
    function GetFieldOfs: integer;
    function GetFieldSize: integer;
    function GetFieldType: TFieldType;
    function GetRequired: boolean;
    function GetRTInfo: TBoldMemberRTInfo;
    function GetUseFieldSize: boolean;
    procedure SetFieldName(const Value: String);
    function GetEffectiveFieldName: String;
    procedure Invalidate;
    procedure EnsureValid;
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure DefineFieldType(aClass: TClass);
  protected
    function GetDisplayName: TBoldRTLString; override;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property BufferSize: integer read FBuffersize;
    property EffectiveFieldName: String read GetEffectiveFieldName;
    property FieldSize: integer read GetFieldSize;
    property FieldType: TFieldType read GetFieldType;
    property FieldOfs: integer read GetFieldOfs;
    property ListelementType: TBoldElementTypeInfo read GetListelementType;
    property Required: boolean read GetRequired;
    property RTInfo: TBoldMemberRTInfo read GetRTInfo;
    property UseFieldSize: boolean read GetUseFieldSize;
    property Dataset: TBoldAbstractDataSet read GetDataset;
    property FieldDescriptions: TBoldDataSetFieldDescriptions read GetFieldDescriptions;
    property Expression: string read GetExpression write SetExpression;
  published
    property BoldProperties: TBoldStringFollowerController read FBoldProperties write SetBoldProperties;
    property FieldName: String read fFieldName write SetFieldName;
  end;

  { TBoldDataSetFieldDescriptions }
  TBoldDataSetFieldDescriptions = class(TCollection)
  private
    FDataset: TBoldAbstractDataSet;
    FFieldFollowers: TBoldControllerList;
    function GetItem(AIndex: Integer): TBoldDataSetFieldDescription;
  protected
    function GetOwner: TPersistent; override;
    property FieldFollowers: TBoldControllerList read FFieldFollowers;
  public
    constructor Create(AOwner: TBoldAbstractDataSet);
    destructor Destroy; override;
    function Add: TBoldDataSetFieldDescription;
    procedure InvalidateFields;
    property Items[index: Integer]: TBoldDataSetFieldDescription read GetItem; default;
    property Dataset: TBoldAbstractDataSet read FDataset;
  end;

  { TBoldAbstractDataSet }
  TBoldAbstractDataSet = class(TDataSet)
  private
    // The debounced refresh after Bold-side changes; see DebouncedResync
    fRefreshWindowOpen: Boolean;
    fRefreshPending: Boolean;
    // The row follower whose cell followers EnsureCellFollowers is creating,
    // while a row is read; nil otherwise
    fEnsuringRowFollower: TBoldFollower;
    FAutoOpen: boolean;
    FBookmarkOfs: integer;
    FContextSubscriber: TBoldPassthroughSubscriber;
    FFieldDescriptions: TBoldDataSetFieldDescriptions;
    FLayoutChanged: boolean;
    FListFollowerController: TBoldListAsFollowerListController;
    FListFollower: TBoldListHandleFollower;
    FRecordPos: integer;
    FBufferSize: integer;
    FRecordSize: integer;
    fLastKnownType: TBoldElementTypeInfo;
    fInternalCursorReady: Boolean;
    function GetIndexForElement(AElement: TBoldElement): integer;
    function GetIndexForBookmark(const Bookmark: TBookmark): integer;
    procedure InsertElement(ABuffer: TRecordBuffer); virtual;
    procedure _ReceiveContext(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function BeingDestroyed: Boolean;
    procedure BoldToBuffer(ABuffer: TRecordBuffer);
    procedure EnsureCellFollowers(ARow: Integer; AElement: TBoldElement);
    procedure _InsertRow(index: Integer; OwningFollower: TBoldFollower);
    procedure _DeleteRow(index: Integer; owningFollower: TBoldFollower);
    procedure BufferToBoldElement(AIndex: integer; ABuffer: TRecordBuffer);
    procedure BoldElementToBuffer(AIndex: integer; ABuffer: TRecordBuffer);
    function GetBookmarkInfo(Buffer: TRecordBuffer): PBDSBookmarkInfo;
    function GetActiveBoldElement: TBoldElement;
    function GetActiveBoldObject: TBoldObject;
    procedure SetAutoOpen(AValue: boolean);
    function GetBoldRTInfo(AFieldName: string): TBoldMemberRTInfo;
    function GetBoldHandle: TBoldAbstractListHandle;
    procedure SetBoldHandle(NewValue: TBoldAbstractListHandle);
    procedure SetFieldDescriptions(const Value: TBoldDataSetFieldDescriptions);
    procedure EnsureRecordPosInValidRange;
    procedure DebouncedResync;
    procedure ResyncLater;
    procedure DebouncedResyncDue;
  protected
    procedure Loaded; override;
    function CreateNewElement: TBoldElement; virtual;
    // How many field followers exist over all rows; see EnsureCellFollowers.
    // For tests: it shows that only the rows read are followed.
    function CellFollowerCount: Integer;
    procedure TypeMayHaveChanged;
    procedure TypeHasChanged; virtual;
    procedure LayoutChanged;
    procedure ClearCalcFields(Buffer: TRecBuf); overload; override;
    procedure DoOnNewRecord; override;
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
    function GetListElementType: TBoldElementTypeInfo; virtual;
    procedure DeleteAllFields;
    { Mandatory overrides }
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecBuf); overload; override;
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; override;
    {$IFNDEF FPC}
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; override;
    {$ENDIF}
    procedure SetRecNo(Value: Integer); override;
    function GetRecordSize: Word; override;
    {$IFDEF FPC}
    procedure SetFieldDataValue(Field: TField; Buffer: TValueBuffer);
    procedure GetBookmarkData(Buffer: TRecBuf; Data: Pointer); override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: Pointer); override;
    {$ELSE}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); overload; override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
    {$ENDIF}
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; overload; override;
    procedure InternalSetToRecord(Buffer: TRecBuf); overload; override;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag); overload; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    {$IFNDEF FPC}
    // Free Pascal's TDataSet never calls InternalAddRecord (its AppendRecord is
    // Append and Post), so only Delphi gets the override
    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); overload; override;
    {$ENDIF}
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    function LocateRecord(const KeyFields: string;
      const KeyValues: Variant; Options: TLocateOptions;
      SyncCursor: Boolean): Boolean;
    function ValuesInFields(AFields: TBoldDataSetFieldList; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    { Optional overrides }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure DoAfterScroll; override;
    function GetCanModify: boolean; override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    function GetList: TBoldList;
    property List: TBoldList read GetList;
    property ListElementType: TBoldElementTypeInfo read GetListElementType;
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
    property AutoOpen: boolean read FAutoOpen write SetAutoOpen;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure CreateDefaultFields;
    {$IFDEF FPC}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    // Public in Free Pascal's TDataSet, protected in Delphi's
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; override;
    function GetFieldDataValue(Field: TField; var Buffer: TValueBuffer): Boolean;
    {$ELSE}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; overload; override;
    {$ENDIF}
    // TBoldRTLString: Free Pascal's TDataSet declares these with AnsiString
    function Locate(const KeyFields: TBoldRTLString; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: TBoldRTLString; const KeyValues: Variant; const ResultFields: TBoldRTLString): Variant; override;
    procedure TryToOpen;
    property ActiveBoldElement: TBoldElement read GetActiveBoldElement ;
    property ActiveBoldObject: TBoldObject read GetActiveBoldObject;
    property BoldRTInfo[AFieldName: string]: TBoldMemberRTInfo read GetBoldRTInfo;
  published
    property FieldDescriptions: TBoldDataSetFieldDescriptions read fFieldDescriptions write SetFieldDescriptions;
  end;

  { TBoldDataSet }
  TBoldDataSet = class(TBoldAbstractDataSet)
  published
    {$IFNDEF T2H}
    property Active;
    property AutoOpen;
    property BoldHandle;
    property BeforePost;
    property BeforeClose;
    property BeforeInsert;
    property AfterInsert;
    property AfterOpen;
    property BeforeScroll;
    property AfterScroll;
    property AfterCancel;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  Math,
  BoldAttributes,
  {$IFDEF FPC}
  DBConst,
  {$ELSE}
  DBConsts,
  {$ENDIF}
  Variants,
  BoldClock,
  BoldEnvironment;

const
  // How long DebouncedResync collects further changes before it re-reads again
  cRefreshInterval = 100 / MSecsPerDay;

procedure NotEditingError(DataSet: TDataSet);
begin
  {$IFDEF FPC}
  // Free Pascal's message has the dataset name in it
  DatabaseErrorFmt(SNotEditing, [DataSet.Name]);
  {$ELSE}
  DatabaseError(SNotEditing);
  {$ENDIF}
end;


type
  TBoldConvInfo = record
    FBoldType: TClass;
    FFieldType: TFieldType;
    FBufferSize: integer;
    FUseSize: boolean;
    FUseDecimals: boolean;
  end;

const
  arConvInfo: array [0..14] of TBoldConvInfo =
    (
    // For ftWideString, FBufferSize is the field size in characters; see WideBufferSize
    (FBoldType: TBAString; FFieldType: ftWideString; FBufferSize: 255; FUseSize: True; FUseDecimals: False),
    (FBoldType: TBANumeric; FFieldType: ftInteger; FBufferSize: sizeof(integer); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAInteger; FFieldType: ftInteger; FBufferSize: sizeof(integer); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBASmallInt; FFieldType: ftSmallint; FBufferSize: sizeof(smallint); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAShortInt; FFieldType: ftSmallint; FBufferSize: sizeof(smallint); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAWord; FFieldType: ftWord; FBufferSize: sizeof(word); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAByte; FFieldType: ftSmallint; FBufferSize: sizeof(smallint); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAFloat; FFieldType: ftFloat; FBufferSize: sizeof(double); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBACurrency; FFieldType: ftCurrency; FBufferSize: sizeof(double); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBABlob; FFieldType: ftBlob; FBufferSize: 40; FUseSize: true; FUseDecimals: False),
    (FBoldType: TBADateTime; FFieldType: ftDateTime; FBufferSize: sizeof(TDateTime); FUseSize: False; FUseDecimals: False),
    (FBoldType: TBADate; FFieldType: ftDate; FBufferSize: SizeOf(Longint); FUseSize: False; FUseDecimals: False),
    (FBoldType: TBATime; FFieldType: ftTime; FBufferSize: SizeOf(Longint); FUseSize: False; FUseDecimals: False),
    (FBoldType: TBAValueSet; FFieldType: ftInteger; FBufferSize: sizeof(integer); FUseSize: false; FUseDecimals: False),
    (FBoldType: TBABoolean; FFieldType: ftBoolean; FBufferSize: sizeof(wordbool); FUseSize: false; FUseDecimals: False)
   );

{ The record buffer of a TWideStringField holds the characters and a #0. }
function WideBufferSize(Chars: integer): integer;
begin
  Result := (Chars + 1) * SizeOf(Char);
end;

{ At most MaxChars characters from a buffer that may lack its #0. }
function BufferToString(P: PChar; MaxChars: integer): string;
var
  Len: integer;
begin
  Len := 0;
  while (Len < MaxChars) and (P[Len] <> #0) do
    Inc(Len);
  SetString(Result, P, Len);
end;

{ The text and a #0 into a buffer of MaxChars characters plus the #0. }
procedure StringToBuffer(const S: string; P: PChar; MaxChars: integer);
var
  Len: integer;
begin
  Len := Min(Length(S), MaxChars);
  if Len > 0 then
    Move(PChar(S)^, P^, Len * SizeOf(Char));
  P[Len] := #0;
end;

type
  { What CreateBlobStream returns for writing: a copy of the blob that goes back
    to the attribute when the caller frees the stream, as TBlobField does. }
  TBoldDataSetBlobStream = class(TMemoryStream)
  private
    fBlob: TBABlob;
    fWriteBack: Boolean;
  public
    constructor Create(ABlob: TBABlob; AMode: TBlobStreamMode);
    destructor Destroy; override;
  end;

constructor TBoldDataSetBlobStream.Create(ABlob: TBABlob; AMode: TBlobStreamMode);
begin
  inherited Create;
  fBlob := ABlob;
  if (AMode <> bmWrite) and not ABlob.IsNull then
  begin
    ABlob.AsStream.SaveToStream(Self);
    Position := 0;
  end;
  // Only a complete copy may go back: the destructor also runs when this raises
  fWriteBack := True;
end;

destructor TBoldDataSetBlobStream.Destroy;
begin
  try
    if fWriteBack and not fBlob.AsStream.IsDataSame(Memory, Size) then
    begin
      // LoadFromStream reads Size bytes from the current position
      Position := 0;
      fBlob.AsStream.LoadFromStream(Self);
    end;
  finally
    inherited;
  end;
end;


{****************************************************************************
  TBoldDataSetFieldDescription
 ****************************************************************************}

constructor TBoldDataSetFieldDescription.Create(AOwner: TCollection);
begin
  Assert(AOwner is TBoldDataSetFieldDescriptions);
  inherited Create(AOwner);
  FBoldProperties := TBoldStringFollowerController.Create(nil);
  FBoldProperties.AfterMakeUptoDate := self.AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetListElementType;
  FieldDescriptions.FieldFollowers.Add(FBoldProperties);
  Invalidate;
end;

destructor TBoldDataSetFieldDescription.Destroy;
begin
  FieldDescriptions.FieldFollowers.Remove(FBoldProperties);
  FreeAndNil(FBoldProperties);
  inherited Destroy;
end;

procedure TBoldDataSetFieldDescription.DefineFieldType(aClass: TClass);
var
  i: integer;
begin
  FFieldType := ftWideString;
  FFieldSize := 255;
  FBufferSize := WideBufferSize(FFieldSize);
  FUseFieldSize := True;
  FRequired := False;

  for i := high(arConvInfo) downto Low(arConvInfo) do
  begin
    with arConvInfo[i] do
    begin
      if AClass.inheritsfrom(arConvInfo[i].FBoldType) then
      begin
        self.FFieldType := arConvInfo[i].FFieldType;
        self.FBufferSize := arConvInfo[i].FBufferSize;
        self.FUseFieldSize := arConvInfo[i].FUseSize;
        if FUseSize then
          self.FFieldSize := arConvInfo[i].FBufferSize
        else
          self.FFieldSize := 0;
        if self.FFieldType = ftWideString then
          self.FBufferSize := WideBufferSize(self.FFieldSize);
        self.FRequired := False;
        break;
      end;
    end;
  end;
end;

procedure TBoldDataSetFieldDescription.AfterMakeUptoDate(Follower: TBoldFollower);
var
  LField: TField;
  ActiveBuf: TRecordBuffer;

  function FindFieldNo(ANo: integer): TField;
  var
    FieldIx: integer;
  begin
    result := nil;
    for FieldIx := 0 to Dataset.FieldCount - 1 do
    begin
      if Dataset.Fields[FieldIx].FieldNo = ANo then
      begin
        result := Dataset.Fields[FieldIx];
        Exit;
      end;
    end;
  end;

begin
  with Dataset do
  begin
    if not Active then
      Exit;

    if Assigned(fEnsuringRowFollower) then
    begin
      // The follower was just created for the row that is being read into its
      // record buffer: the buffer gets this value, so there is nothing to
      // refresh. A follower of another row comes here when the display queue
      // is drained meanwhile (a log window that pumps messages does that):
      // its change needs a refresh, but not now, in the middle of GetRecord.
      // In a write mode only the active record matters, and that is handled
      // when the next change comes.
      if (Follower.OwningFollower <> fEnsuringRowFollower) and not (Dataset.State in dsWriteModes) then
        ResyncLater;
      Exit;
    end;

    if not (Dataset.State in dsWriteModes) then
    begin
      DebouncedResync;
      //UpdateCursorPos;
      //Resync([]);
    end
    else
    begin
      if GetActiveRecBuf(ActiveBuf) and (GetBookmarkInfo(ActiveBuf)^.Element = Follower.OwningFollower.Element) then
      begin
        BoldElementToBuffer(Follower.Index, ActiveBuf);
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        begin
          LField := FindFieldNo(Follower.Index + 1);
          if Assigned(LField) then
            DataEvent(deFieldChange, NativeInt(LField));
        end;
        SetModified(True);
      end
    end
  end;
end;

function TBoldDataSetFieldDescription.GetEffectiveFieldName: String;
var
  i: integer;
begin
  if FieldName <> '' then
    result := FieldName
  else
  begin
    result := BoldProperties.Expression;
    for i := 1 to length(Result) do
      if CharInSet(Result[i], ['.', '(', '-', '>', ')']) then
        result[i] := '_'
  end;
end;

procedure TBoldDataSetFieldDescription.EnsureValid;
var
  LFieldType: TBoldElementTypeInfo;
begin
  if fInvalid then
  begin
    if not Assigned(ListElementType) then
      raise EBold.CreateFmt('Cannot initialize fields - dataset %s has no context', [Dataset.Name]);

    //LFieldType := ListElementType.Evaluator.ExpressionType(Expression, ListElementType, false);
    LFieldType := ListElementType.Evaluator.ExpressionType(Expression,
      ListElementType, false, fBoldProperties.VariableList);
    if not Assigned(LFieldType) then
      raise Exception.CreateFmt('Invalid field expression %s', [Expression]);

    if ListElementType.Evaluator is TBoldRTEvaluator then
      fRTInfo := (ListElementType.Evaluator as TBoldRTEvaluator).RTInfo(Expression, ListElementType, false)
    else
      fRTInfo := nil;

    if LFieldType is TBoldAttributeTypeInfo then
    begin
      DefineFieldType(TBoldAttributeTypeInfo(LFieldType).AttributeClass);
      if (fRTInfo is TBoldAttributeRTInfo) and
         (TBoldAttributeRTInfo(FRTInfo).Length > 0) and
         FUseFieldSize then
      begin
        FFieldSize := TBoldAttributeRTInfo(FRTInfo).Length;
        if FFieldType = ftWideString then
          FBufferSize := WideBufferSize(FFieldSize)
        else
          FBufferSize := FFieldSize + 1;
      end;
    end
    else if LFieldType is TBoldClassTypeInfo then
    begin
      // The field holds the object's locator: a pointer, so 64 bits wide
      FFieldType := ftLargeint;
      FFieldSize := 0;
      FBufferSize := SizeOf(Int64);
      FRequired := False;
    end
    else
    begin
      FFieldType := ftWideString;
      FFieldSize := 40;
      FBufferSize := WideBufferSize(FFieldSize);
      FRequired := False;
    end;
    fInvalid := false;
  end;
end;

function TBoldDataSetFieldDescription.GetFieldDescriptions: TBoldDataSetFieldDescriptions;
begin
  result := TBoldDataSetFieldDescriptions(Collection);
end;

function TBoldDataSetFieldDescription.GetDataset: TBoldAbstractDataSet;
begin
  result := FieldDescriptions.Dataset;
end;

function TBoldDataSetFieldDescription.GetListElementType: TBoldElementTypeInfo;
begin
  result := Dataset.ListelementType;
end;

function TBoldDataSetFieldDescription.GetExpression: string;
begin
  result := FBoldProperties.Expression;
end;

procedure TBoldDataSetFieldDescription.SetExpression(AValue: string);
begin
  fBoldProperties.Expression := aValue;
  Invalidate;
end;

procedure TBoldDataSetFieldDescription.SetBoldProperties(AValue: TBoldStringFollowerController);
begin
  if (AValue <> FBoldProperties) and Assigned(AValue) then
    FBoldProperties.Assign(AValue);
  invalidate;
end;

procedure TBoldDataSetFieldDescription.Assign(Source: TPersistent);
begin
  if source is TBoldDataSetFieldDescription then
  begin
    self.BoldProperties.Assign(TBoldDataSetFieldDescription(Source).BoldProperties);
    self.FieldName := TBoldDataSetFieldDescription(Source).FieldName;
    Invalidate;
  end;
end;

procedure TBoldDataSetFieldDescription.Invalidate;
begin
  fInvalid := true;
end;

function TBoldDataSetFieldDescription.GetFieldOfs: integer;
begin
  EnsureValid;
  Result := FFieldOfs;
end;

function TBoldDataSetFieldDescription.GetFieldSize: integer;
begin
  EnsureValid;
  REsult := fFieldSize;
end;

function TBoldDataSetFieldDescription.GetFieldType: TFieldType;
begin
  EnsureValid;
  result := fFieldType;
end;

function TBoldDataSetFieldDescription.GetRequired: boolean;
begin
  Ensurevalid;
  result := fRequired;
end;

function TBoldDataSetFieldDescription.GetRTInfo: TBoldMemberRTInfo;
begin
  EnsureValid;
  result := fRTInfo;
end;

function TBoldDataSetFieldDescription.GetUseFieldSize: boolean;
begin
  EnsureValid;
  result := fUseFieldSize;
end;

procedure TBoldDataSetFieldDescription.SetFieldName(const Value: String);
begin
  fFieldName := Value;
  Invalidate;
end;

function TBoldDataSetFieldDescriptions.GetOwner: TPersistent;
begin
  result := DataSet;
end;

function TBoldDataSetFieldDescription.GetDisplayName: TBoldRTLString;
begin
  result := EffectiveFieldName + ' = ' + Expression;
end;

{****************************************************************************
  TBoldDataSetFieldDescriptions
****************************************************************************}

constructor TBoldDataSetFieldDescriptions.Create(AOwner: TBoldAbstractDataSet);
begin
  inherited Create(TBoldDataSetFieldDescription);
  FFieldFollowers := TBoldControllerList.Create(nil);
  FDataset := AOwner;
end;

destructor TBoldDataSetFieldDescriptions.Destroy;
begin
  Clear;
  FreeAndNil(FFieldFollowers);
  inherited Destroy;
end;

function TBoldDataSetFieldDescriptions.Add: TBoldDataSetFieldDescription;
begin
  result := TBoldDataSetFieldDescription(inherited Add);




end;

function TBoldDataSetFieldDescriptions.GetItem(AIndex: Integer): TBoldDataSetFieldDescription;
begin
  result := TBoldDataSetFieldDescription(inherited Items[AIndex]);
end;

procedure TBoldDataSetFieldDescriptions.InvalidateFields;
var
  i: integer;
begin
  for i := 0 to count - 1 do
    items[i].Invalidate;
end;

{****************************************************************************
  TBoldAbstractDataSet
 ****************************************************************************}

constructor TBoldAbstractDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLastKnownType := nil;
  fInternalCursorReady := false;
  FContextSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveContext);

  FFieldDescriptions := TBoldDataSetFieldDescriptions.Create(self);
  FLayoutChanged := True;

  FListFollowerController := TBoldListAsFollowerListController.Create(self, FFieldDescriptions.FieldFollowers);
  FListFollowerController.OnAfterInsertItem := _InsertRow;
  FListFollowerController.OnAfterDeleteItem := _DeleteRow;
  FListFollowerController.OnGetContextType := GetListElementType;

  FListFollower := TBoldListHandleFollower.Create(Owner, FListFollowerController);
  FRecordSize := 0;
  FRecordPos := -1;
end;

destructor TBoldAbstractDataSet.Destroy;
begin
  Close;
  // A refresh that is still due must not reach the freed dataset
  // BoldSharedClock is a function (nil after BoldClock's finalization), so no Assigned()
  if fRefreshWindowOpen and (BoldSharedClock <> nil) then
    BoldSharedClock.Cancel(DebouncedResyncDue);
  FRecordPos := -1;
  FreeAndNil(FContextSubscriber);
  FreeAndNil(FListFollower);
  FreeAndNil(FListFollowerController);
  FreeAndNil(FFieldDescriptions);
  inherited Destroy;
end;

function TBoldAbstractDataSet.GetListElementType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

procedure TBoldAbstractDataSet.SetAutoOpen(AValue: boolean);
begin
  if (FAutoOpen <> AValue) then
  begin
    FAutoOpen := AValue;
    if FAutoOpen then
      TryToOpen;
  end;    
end;

procedure TBoldAbstractDataSet.TryToOpen;
begin
  if not (csLoading in ComponentState) and
     not (csDestroying in ComponentState) and 
     not Active then
  try
    Open;
  except
  end;
end;

procedure TBoldAbstractDataSet.DoOnNewRecord;
begin
  InsertElement(TRecordBuffer(ActiveBuffer));
  inherited DoOnNewRecord;
end;

function TBoldAbstractDataSet.CreateNewElement: TBoldElement;
begin
  result := nil;
  if assigned(BoldHandle) and
     assigned(BoldHandle.List) and
     BoldHandle.List.CanCreateNew then
    result := BoldHandle.List.AddNew;
end;

procedure TBoldAbstractDataSet.InsertElement(ABuffer: TRecordBuffer);
var
  LElement: TBoldElement;
begin
  lElement := CreateNewElement;
  if not Assigned(LElement) then
    Abort;

  fRecordPos := List.IndexOf(LElement);

  with GetBookmarkInfo(ABuffer)^ do
  begin
    Flag := bfInserted;
    Element := LElement;
  end;
  BoldToBuffer(ABuffer);
end;

function TBoldAbstractDataSet.GetBoldRTInfo(AFieldName: string): TBoldMemberRTInfo;
var
 FieldDesc: TBoldDataSetFieldDescription;
 i: integer;
begin
  InternalInitFieldDefs;
  FieldDesc := nil;
  for i := 0 to FieldDescriptions.Count - 1 do
  begin
    if FieldDescriptions[i].EffectiveFieldName = AFieldName then
    begin
      FieldDesc := FieldDescriptions.Items[i];
      break;
    end;
  end;

  if Assigned(FieldDesc) then
    result := FieldDesc.RTInfo
  else
    raise Exception.CreateFmt('Invalid fieldname %s', [AFieldName]);
end;

procedure TBoldAbstractDataSet.EnsureRecordPosInValidRange;
begin
  if assigned(list) then
  begin
    if fRecordPos > List.Count-1 then
      fRecordPos := List.Count-1
  end
  else
    fRecordPos := -1;
end;

{ A form marks its components as being destroyed before it calls OnDestroy.
  A list handle in that state raises when it is asked for its list, and then
  tells again that its value changed; asking it from _ReceiveContext would
  repeat that until the stack overflows. }
function TBoldAbstractDataSet.BeingDestroyed: Boolean;
begin
  Result := (csDestroying in ComponentState) or
    (Assigned(BoldHandle) and (csDestroying in BoldHandle.ComponentState));
end;

procedure TBoldAbstractDataSet._ReceiveContext(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    beDestroying: Close;
    beValueIdentityChanged:
      if not BeingDestroyed then
      begin
        TypeMayHaveChanged;
        EnsureRecordPosInValidRange;
      end;
    beItemDeleted:
      if not BeingDestroyed then
        EnsureRecordPosInValidRange;
  end;
end;

function TBoldAbstractDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  BoldDataSetFieldDescription: TBoldDataSetFieldDescription;
  BlobValue: TBABlob;
  IndirectElement: TBoldIndirectElement;
  LElement: TBoldElement;
begin
  LElement := ActiveBoldElement;
  if not Assigned(LElement) then
    Result := TMemoryStream.Create
  else
  begin
    IndirectElement := TBoldIndirectElement.Create;
    try
      BoldDataSetFieldDescription := FieldDescriptions[Field.FieldNo - 1];
      LElement.EvaluateExpression(BoldDataSetFieldDescription.Expression,
        IndirectElement, false, BoldDataSetFieldDescription.fBoldProperties.VariableList);
      BlobValue := (IndirectElement.Value as TBABlob);
      if Mode = bmRead then
      begin
        // A copy: the caller frees what it gets, and the blob keeps its own stream
        Result := TMemoryStream.Create;
        if Assigned(BlobValue) and not BlobValue.IsNull then
        begin
          BlobValue.AsStream.SaveToStream(Result);
          Result.Position := 0;
        end;
      end
      else
      begin
        // A value that IndirectElement owns is gone after the finally below
        if not Assigned(BlobValue) or IndirectElement.OwnsValue then
          DatabaseErrorFmt({$IFDEF FPC}SReadOnlyField{$ELSE}SFieldReadOnly{$ENDIF}, [Field.DisplayName]);
        Result := TBoldDataSetBlobStream.Create(BlobValue, Mode);
      end;
    finally
      IndirectElement.Free;
    end;
  end;
end;

procedure TBoldAbstractDataSet.InternalEdit;
begin
  inherited InternalEdit;
end;

function TBoldAbstractDataSet.GetCanModify: boolean;
begin
  result := True;
end;

procedure TBoldAbstractDataSet.InternalCancel;
begin
  if Assigned(ActiveBoldObject) then
    ActiveBoldObject.Discard;
{CHECKME Is this needed???

  if State = dsInsert then
  begin
    if SQL then
      self.FListHandle.RemoveCurrentElement
  end;
}
  inherited;
end;

function TBoldAbstractDataSet.Lookup(const KeyFields: TBoldRTLString; const KeyValues: Variant;
  const ResultFields: TBoldRTLString): Variant;
begin
  Result := null;
  if LocateRecord(KeyFields, KeyValues, [loCaseInsensitive], false) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

procedure TBoldAbstractDataSet.DoAfterScroll;
begin
  if (FListFollower.Follower.CurrentIndex <> RecNo - 1) then
    fListFollower.SetFollowerIndex(RecNo - 1);  
  inherited ;
end;

function TBoldAbstractDataSet.ValuesInFields(AFields: TBoldDataSetFieldList; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  i: Integer;

  function CompareField(AField: TField; AValue: variant): boolean;
  var
    Key: string;
  begin
    if (loPartialKey in Options) then
    begin
      // The field starts with the key. Strings, not PChar: Free Pascal's
      // AnsiStrLComp takes PAnsiChar
      Key := VarToStr(AValue);
      if (loCaseInsensitive in Options) then
        Result := AnsiCompareText(Copy(AField.AsString, 1, Length(Key)), Key) = 0
      else
        Result := AnsiCompareStr(Copy(AField.AsString, 1, Length(Key)), Key) = 0
    end
    else
    begin
      if loCaseInsensitive  in Options then
        Result := AnsiCompareText(AField.AsString, VarToStr(AValue)) = 0
      else
        Result := AnsiCompareStr(AField.AsString, VarToStr(AValue)) = 0
    end;
  end;

begin
  if AFields.count = 1 then
    result := CompareField(TField(AFields[0]), KeyValues)
  else
  begin
    Result := True;
    for i := 0 to AFields.count - 1 do
    begin
      Result := Result and CompareField(TField(AFields[i]), KeyValues[i]);
      if not Result then
        Exit;
    end;
  end;
end;
function TBoldAbstractDataSet.LocateRecord(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions;
  SyncCursor: Boolean): Boolean;
var
  LFields: TBoldDataSetFieldList;
  Locator: TBoldObjectLocator;
  LocatorPtr: pointer;
  LElement: TBoldElement;
  i: Integer;
begin
  result := false;
  if BoldHandle.Count = 0 then
    Exit;

  CheckBrowseMode;
  CursorPosChanged;
  SetTempState(dsFilter);
  try
    if UpperCase(KeyFields) = 'SELF' then
    begin
      // The key is the locator's address, as the object-reference fields hold it
      LocatorPtr := Pointer(NativeInt(Int64(KeyValues)));
      if Assigned(LocatorPtr) then
      begin
        try
          Locator := TObject(LocatorPtr) as TBoldObjectLocator;
          LElement := Locator.BoldObject;
          with GetBookmarkInfo(TRecordBuffer(TempBuffer))^ do
          begin
            Element := LElement;
            Flag := bfCurrent;
          end;
          BoldToBuffer(TRecordBuffer(TempBuffer));
          FRecordPos := List.IndexOf(LElement);
          Result := FRecordPos <> -1;
        except
        end
      end
    end
    else
    begin
      LFields := TBoldDataSetFieldList.Create;
      try
        GetFieldList(LFields, KeyFields);
        for i := 0 to List.Count - 1 do
        begin
          with GetBookmarkInfo(TRecordBuffer(TempBuffer))^ do
          begin
            Element := List[i];
            Flag := bfCurrent;
          end;

          BoldToBuffer(TRecordBuffer(TempBuffer));
          if ValuesInFields(LFields, KeyValues, Options) then
          begin
            FRecordPos := i;
            Result := True;
            break;
          end;
        end;
      finally
        LFields.Free;
      end;
    end;
  finally
    RestoreState(dsBrowse);
  end;
end;

function TBoldAbstractDataSet.Locate(const KeyFields: TBoldRTLString; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TBoldAbstractDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := GetIndexForBookmark(Bookmark) <> -1;
end;

procedure TBoldAbstractDataSet.BoldToBuffer(ABuffer: TRecordBuffer);
var
  i: integer;
begin
  for i := 0 to FieldDescriptions.Count - 1 do
    BoldElementToBuffer(i, ABuffer);

  GetCalcFields(TRecBuf(ABuffer));
end;

{ Makes the dataset follow the fields of a row it reads. The control packs
  create the followers of a row's cells only when a control asks for them, as
  a grid does for the cells it draws. The dataset reads its values straight
  from the element, so without this no cell follower exists, nothing
  subscribes to the row's attributes, and a change of an attribute alone
  never reaches TBoldDataSetFieldDescription.AfterMakeUptoDate.

  Only the rows that are read get followers: with a grid attached those are
  the rows it shows and has shown, not the whole list. A row that was never
  read needs none, because reading it takes the current values.

  The row followers themselves appear when the list follower is displayed,
  which ends in _InsertRow and a refresh that reads the rows again; a row
  read before that is picked up then. }
procedure TBoldAbstractDataSet.EnsureCellFollowers(ARow: Integer; AElement: TBoldElement);
var
  ListFollower, RowFollower: TBoldFollower;
  i, CellCount: Integer;
begin
  if Assigned(fEnsuringRowFollower) then
    Exit;
  ListFollower := FListFollower.Follower;
  if not Assigned(ListFollower) or (ARow < 0) or (ARow >= ListFollower.SubFollowerCount) or
    not ListFollower.SubFollowerAssigned[ARow] then
    Exit;
  RowFollower := ListFollower.SubFollowers[ARow];
  // After a change of the list the row follower may still be another element's
  if not Assigned(RowFollower) or (RowFollower.Element <> AElement) or not RowFollower.Active then
    Exit;
  CellCount := RowFollower.SubFollowerCount;
  if CellCount > FieldDescriptions.Count then
    CellCount := FieldDescriptions.Count;
  fEnsuringRowFollower := RowFollower;
  try
    for i := 0 to CellCount - 1 do
      if not RowFollower.SubFollowerAssigned[i] then
        // Reading SubFollowers creates the follower, which evaluates its
        // expression and subscribes
        RowFollower.SubFollowers[i].Active := True;
  finally
    fEnsuringRowFollower := nil;
  end;
end;

function TBoldAbstractDataSet.CellFollowerCount: Integer;
var
  ListFollower, RowFollower: TBoldFollower;
  Row, i: Integer;
begin
  Result := 0;
  ListFollower := FListFollower.Follower;
  if not Assigned(ListFollower) then
    Exit;
  for Row := 0 to ListFollower.SubFollowerCount - 1 do
    if ListFollower.SubFollowerAssigned[Row] then
    begin
      RowFollower := ListFollower.SubFollowers[Row];
      if Assigned(RowFollower) then
        for i := 0 to RowFollower.SubFollowerCount - 1 do
          if RowFollower.SubFollowerAssigned[i] then
            Inc(Result);
    end;
end;

procedure TBoldAbstractDataSet._DeleteRow(index: Integer; owningFollower: TBoldFollower);
begin
  if not Active then
    Exit;

  if State = dsInsert then
    Exit;
  UpdateCursorPos;  
  if BoldHandle.CurrentElement <> ActiveBoldElement then
    if (State in dsWriteModes) then
      Cancel
    else
      Resync([])
  else
    if not (State in dsWriteModes) then
      Resync([]);

end;

procedure TBoldAbstractDataSet._InsertRow(index: Integer; OwningFollower: TBoldFollower);
begin
  if State in [dsInsert, dsInactive] then
    Exit;
  if (State in dsWriteModes) then
    UpdateRecord;
  DebouncedResync;
  //UpdateCursorPos;
  //Resync([]);
end;

function TBoldAbstractDataSet.GetList: TBoldList;
begin
  if Assigned(BoldHandle) then
    result := BoldHandle.List
  else
    Result := nil;
end;

procedure TBoldAbstractDataSet.BufferToBoldElement(AIndex: integer; ABuffer: TRecordBuffer);
var
  Locator: TBoldObjectLocator;
  LocatorPointer: pointer;
  LFieldAdr: pointer;
  LFieldDescr: TBoldDataSetFieldDescription;
  IndirectElement: TBoldIndirectElement;
  LValue: TBoldElement;
  LActiveElement: TBoldElement;
  LTimeStamp: TTimeStamp;
begin
  LFieldDescr := FieldDescriptions[AIndex];

  with LFieldDescr do
  begin
    LFieldAdr := ABuffer + FieldOfs;

    IndirectElement := TBoldIndirectElement.Create;
    try
      LValue := nil;
      try
        LActiveElement := GetBookmarkInfo(ABuffer)^.Element;

        //LActiveElement.EvaluateExpression(Expression, IndirectElement);
        LActiveElement.EvaluateExpression(Expression, IndirectElement, False,
          fBoldProperties.VariableList);
        LValue := IndirectElement.Value;
      except
      end;

      if Assigned(LValue) and LValue.Mutable then
      case FieldType of
        ftBoolean: begin
          // TBooleanField's native format is a WordBool
          if LValue is TBABoolean then
            TBABoolean(LValue).AsBoolean := PWordBool(LFieldAdr)^
          else if PWordBool(LFieldAdr)^ then
            LValue.AsString := 'Y'
          else
            LValue.AsString := 'N'
        end;

        ftWideString: begin
          LValue.AsString := BufferToString(PChar(LFieldAdr), FieldSize);
        end;

        ftLargeint: begin
          if LValue is TBoldObjectReference then
          begin
            LocatorPointer := Pointer(NativeInt(PInt64(LFieldAdr)^));
            if Assigned(LocatorPointer) then
            begin
              Locator := TObject(LocatorPointer) as TBoldObjectLocator;
              Locator.EnsureBoldObject;
              TBoldObjectReference(LValue).BoldObject := Locator.BoldObject;
            end
            else
              TBoldObjectReference(LValue).BoldObject := nil;
          end
          else
            LValue.AsString := IntToStr(PInt64(LFieldAdr)^);
        end;

        ftInteger: begin
          if lValue is TBAValueSet then
            (lValue as TBAValueSet).AsInteger := PInteger(LFieldAdr)^
          else
            LValue.AsString := IntToStr(PInteger(LFieldAdr)^);
        end;

        ftCurrency,
        ftFloat: begin
          LValue.AsString := FloatToStr(PDouble(LFieldAdr)^);
        end;

        // The native formats are TDateTimeRec's: Time and Date are 32-bit
        // integers, DateTime is a Double of milliseconds
        ftTime:
          if PInteger(LFieldAdr)^ <> 0 then
          begin
            LTimeStamp.Date := DateDelta;
            LTimeStamp.Time := PInteger(LFieldAdr)^;
            if LValue is TBATime then
              TBATime(LValue).AsTime := TimeStampToDateTime(LTimeStamp)
            else
              LValue.AsString := TimeToStr(TimeStampToDateTime(LTimeStamp));
          end
          else
            LValue.AsString := '';

        ftDate:
          if PInteger(LFieldAdr)^ <> 0 then
          begin
            LTimeStamp.Date := PInteger(LFieldAdr)^;
            LTimeStamp.Time := 0;
            if LValue is TBADate then
              TBADate(LValue).AsDate := TimeStampToDateTime(LTimeStamp)
            else
              LValue.AsString := DateToStr(TimeStampToDateTime(LTimeStamp));
          end
          else
            LValue.AsString := '';

        ftDateTime:
          if PDouble(LFieldAdr)^ <> 0 then
          begin
            if LValue is TBADateTime then
              TBADateTime(LValue).AsDateTime := TimeStampToDateTime(MSecsToTimeStamp(PDouble(LFieldAdr)^))
            else
              LValue.AsString := DateTimeToStr(TimeStampToDateTime(MSecsToTimeStamp(PDouble(LFieldAdr)^)));
          end
          else
            LValue.AsString := '';
      end;
    finally
      IndirectElement.Free;
    end;
  end;
end;

procedure TBoldAbstractDataSet.BoldElementToBuffer(AIndex: integer; ABuffer: TRecordBuffer);
var
  LDateTime: TDateTime;
  LTimeStamp: TTimeStamp;
  LInteger: integer;
  LInt64: Int64;
  LBoolean: WordBool;
  LFloat: double;
  LFieldAdr: pointer;
  IndirectElement: TBoldIndirectElement;
  LActiveElement: TBoldElement;
  LFieldDescr: TBoldDataSetFieldDescription;
  Locator: TBoldObjectLocator;
  LString: string;
begin
  LFieldDescr := FieldDescriptions[AIndex];

  with LFieldDescr do
  begin
    LFieldAdr := ABuffer + FieldOfs;

    IndirectElement := TBoldIndirectElement.Create;
    try
      FillChar(LFieldAdr^, BufferSize, 0);
      try
        LActiveElement := GetBookmarkInfo(ABuffer)^.Element;
        //LActiveElement.EvaluateExpression(Expression, IndirectElement);
        LActiveElement.EvaluateExpression(Expression, IndirectElement, False,
          fBoldProperties.VariableList);
        if Assigned(IndirectElement.Value) then
          case FieldType of
            ftBoolean: begin
              // TBooleanField's native format is a WordBool
              if IndirectElement.Value is TBABoolean then
                LBoolean := not TBABoolean(IndirectElement.Value).IsNull and
                  TBABoolean(IndirectElement.Value).AsBoolean
              else
                LBoolean := IndirectElement.Value.AsString = 'Y';
              Move(LBoolean, LFieldAdr^, SizeOf(LBoolean));
            end;

            ftWideString: begin
              StringToBuffer(IndirectElement.Value.AsString, PChar(LFieldAdr), FieldSize);
            end;

            ftLargeint: begin
              Locator := nil;
              if IndirectElement.Value is TBoldObjectReference then
                Locator := TBoldObjectReference(IndirectElement.Value).Locator
              else if IndirectElement.Value is TBoldObject then
                Locator := TBoldObject(IndirectElement.Value).BoldObjectLocator;
              LInt64 := NativeInt(Pointer(Locator));
              Move(LInt64, LFieldAdr^, SizeOf(LInt64));
            end;

            ftInteger: begin
              if IndirectElement.Value is TBAValueSet then
              begin
                if (IndirectElement.Value as TBAValueSet).IsNull then
                  LInteger := 0
                else
                  lInteger := (IndirectElement.Value as TBAValueSet).AsInteger;
              end
              else
              begin
                if IndirectElement.Value.AsString = '' then
                  LInteger := 0
                else
                  LInteger := StrToInt(IndirectElement.Value.AsString);
              end;
              Move(LInteger, LFieldAdr^, SizeOf(LInteger));
            end;

            ftCurrency,
            ftFloat: begin
              LFloat := StrToFloat(IndirectElement.Value.AsString);
              Move(LFloat, LFieldAdr^, SizeOf(LFloat));
            end;

            ftDate: begin
              LTimeStamp.Date := 0;
              if IndirectElement.Value is TBADate then
                LTimeStamp := DateTimeToTimeStamp((IndirectElement.value as TBADate).asDate)
              else
              begin
                LString := Trim(IndirectElement.Value.AsString);
                if LString <> '' then
                  LTimeStamp := DateTimeToTimeStamp(StrToDate(IndirectElement.Value.AsString));
              end;
              Move(LTimeStamp.Date, LFieldAdr^, SizeOf(LTimeStamp.Date));
            end;

            ftTime: begin
              LTimeStamp.Time := 0;
              if IndirectElement.value is TBATime then
                lTimeStamp := DateTimeToTimeStamp((IndirectElement.value as TBATime).asTime)
              else
              begin
                LString := Trim(IndirectElement.Value.AsString);
                if LString <> '' then
                  LTimestamp := DateTimeToTimeStamp(StrToTime(IndirectElement.Value.AsString));
              end;
              Move(LTimeStamp.Time, LFieldAdr^, SizeOf(LTimeStamp.Time));
            end;

            ftDateTime: begin
              lFloat := 0;
              if IndirectElement.Value is TBADateTime then
              begin
                lDateTime := (IndirectElement.value as TBADateTime).asDateTime;
                LFloat := TimeStampToMSecs(DateTimeToTimeStamp(LDateTime));
              end
              else
              begin
                LString := Trim(IndirectElement.Value.AsString);
                if LString <> '' then
                begin
                  LDateTime := StrToDateTime(IndirectElement.Value.AsString);
                  LFloat := TimeStampToMSecs(DateTimeToTimeStamp(LDateTime));
                end
              end;
              Move(LFloat, LFieldAdr^, SizeOf(LFloat));
            end;

            // ftBlob: TBlobField reads the content through CreateBlobStream, so
            // its buffer stays zero (the old text copy wrote two bytes per
            // character into a buffer sized in bytes)
          end;
      except
      end;
    finally
      IndirectElement.Free;
    end;
  end;
end;

function TBoldAbstractDataSet.GetActiveBoldElement: TBoldElement;
var
  ActiveBuf: TRecordBuffer;
begin
  if Active and GetActiveRecBuf(ActiveBuf) then
    result := GetBookmarkInfo(ActiveBuf)^.Element
  else
    result := nil;
end;

function TBoldAbstractDataSet.GetActiveBoldObject: TBoldObject;
begin
  result := (ActiveBoldElement as TBoldObject);
end;

function TBoldAbstractDataSet.GetBookmarkInfo(Buffer: TRecordBuffer): PBDSBookmarkInfo;
begin
  result := PBDSBookmarkInfo(Buffer + FBookmarkOfs);
end;

function TBoldAbstractDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FBufferSize);
end;

procedure TBoldAbstractDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;


procedure TBoldAbstractDataSet.InternalInitRecord(Buffer: TRecBuf);
begin
  FillChar(TRecordBuffer(Buffer)^, FBufferSize, 0);
end;















{$IFNDEF FPC}
function TBoldAbstractDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  // Still abstract in TDataSet, so implemented; the TRecBuf overload does the work
  Result := GetRecord(TRecBuf(Buffer), GetMode, DoCheck);
end;
{$ENDIF}

function TBoldAbstractDataSet.GetRecord(Buffer: TRecBuf; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  if not Assigned(List) then
  begin
    Result := grEOF;
    exit;
  end;

  // An empty list is an empty dataset: TDataSet then sets Bof and Eof
  if RecordCount = 0 then
  begin
    if GetMode = gmPrior then
      Result := grBOF
    else
      Result := grEOF;
    Exit;
  end;

  result := grOk;
  case GetMode of
    gmCurrent: begin
      if (FRecordPos < 0) or (FRecordPos >= RecordCount) then
        result := grError;
    end;

    gmNext: begin
      if FRecordPos >= RecordCount - 1 then
      begin
        result := grEof;
      end
      else
        Inc(FRecordPos, 1);
    end;

    gmPrior: if FRecordPos <= 0 then
      begin
        result := grBof;
        FRecordPos := -1;
      end
      else
        Dec(FRecordPos, 1);
  end;

  if result = grOk then
  begin
    GetCalcFields(Buffer);
    with GetBookmarkInfo(TRecordBuffer(Buffer))^ do
    begin
      Element := List[FRecordPos];
      Flag := bfCurrent;
    end;
    BoldToBuffer(TRecordBuffer(Buffer));
    EnsureCellFollowers(FRecordPos, List[FRecordPos]);
  end
  else
    if (result = grError) and (DoCheck) then
      DatabaseError('No records found!');
end;



function TBoldAbstractDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TBoldAbstractDataSet.GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
begin
  case State of
    dsBrowse: if IsEmpty then
      RecBuf := nil
    else
      RecBuf := TRecordBuffer(ActiveBuffer);

    dsEdit,
    dsInsert: RecBuf := TRecordBuffer(ActiveBuffer);

    dsSetKey: RecBuf := nil;

    dsCalcFields: RecBuf := TRecordBuffer(CalcBuffer);

    dsFilter: RecBuf := TRecordBuffer(TempBuffer);

    dsNewValue: RecBuf := nil;

    dsOldValue: RecBuf := nil;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

procedure TBoldAbstractDataSet.ClearCalcFields(Buffer: TRecBuf);
begin
  FillChar((TRecordBuffer(Buffer) + RecordSize)^, CalcFieldsSize, 0);
end;

{$IFDEF FPC}
function TBoldAbstractDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Value: TValueBuffer;
begin
  // A nil Buffer only asks whether the field has a value
  if Assigned(Buffer) then
    SetLength(Value, Field.DataSize);
  Result := GetFieldDataValue(Field, Value);
  if Result and Assigned(Buffer) and (Length(Value) > 0) then
    Move(Value[0], Buffer^, Length(Value));
end;

procedure TBoldAbstractDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  Value: TValueBuffer;
begin
  // A nil Buffer sets the field to null
  if Assigned(Buffer) and (Field.DataSize > 0) then
  begin
    SetLength(Value, Field.DataSize);
    Move(Buffer^, Value[0], Length(Value));
  end;
  SetFieldDataValue(Field, Value);
end;
{$ENDIF}

function TBoldAbstractDataSet.{$IFDEF FPC}GetFieldDataValue{$ELSE}GetFieldData{$ENDIF}(Field: TField; var Buffer: TValueBuffer): Boolean;
var
  LActiveElement: TBoldElement;
  IndirectElement: TBoldIndirectElement;
  LRecBuf: TRecordBuffer;
  Attr: TBoldAttribute;
begin
  Result := False;
  if not GetActiveRecBuf(LRecBuf) then
    Exit;

  with Field do
  begin
    if FieldNo > 0 then
    begin
      with FieldDescriptions[Field.FieldNo - 1] do
      begin
        IndirectElement := TBoldIndirectElement.Create;
        try
          LActiveElement := GetBookmarkInfo(LRecBuf)^.Element;
          //LActiveElement.EvaluateExpression(Expression, IndirectElement);
          LActiveElement.EvaluateExpression(Expression, IndirectElement, False,
            fBoldProperties.VariableList);
          if Assigned(IndirectElement.Value) and (IndirectElement.Value is TBoldAttribute) then
          begin
            Attr := TBoldAttribute(IndirectElement.Value);
            result := not Attr.IsNull;
            if (Length(Buffer) >= SizeOf(Variant)) and (Field.DataType=ftVariant) then
              PVariant(@Buffer[0])^ := IndirectElement.Value.GetAsVariant;
            Field.readOnly := not Attr.CanModify;
          end
          else
            result := True;
        finally
          IndirectElement.Free;
        end;
        if Length(Buffer) > 0 then
          Move((LRecBuf + FieldOfs)^, Buffer[0], Min(Min(Length(Buffer), Field.DataSize), BufferSize));
      end;
    end
    else
    begin
      if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
      begin
        // A calculated field: a null flag byte, then the value
        Inc(LRecBuf, FRecordSize + Offset);
        Result := Byte(LRecBuf^) <> 0;
        if Result and (Length(Buffer) > 0) then
          Move((LRecBuf + 1)^, Buffer[0], Min(Length(Buffer), DataSize));
      end;
    end;
    if (DataType = ftWideString) and (fRecordSize > 30) then
      DisplayWidth := 30;
  end;
end;

procedure TBoldAbstractDataSet.{$IFDEF FPC}SetFieldDataValue{$ELSE}SetFieldData{$ENDIF}(Field: TField; Buffer: TValueBuffer);
var
  LRecBuf: TRecordBuffer;
begin
  with Field do
  begin
    if not (State in dsWriteModes) then
      NotEditingError(Self);

    if not GetActiveRecBuf(LRecBuf) then
      Exit;

    if FieldNo > 0 then
    begin
      if State = dsCalcFields then
        NotEditingError(Self);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        DatabaseErrorFmt({$IFDEF FPC}SReadOnlyField{$ELSE}SFieldReadOnly{$ENDIF}, [DisplayName]);
      Validate({$IFDEF FPC}Pointer(Buffer){$ELSE}Buffer{$ENDIF});
      if FieldKind <> fkInternalCalc then
      begin
        with FieldDescriptions[Field.FieldNo - 1] do
        begin
          FillChar((LRecBuf + FieldOfs)^, BufferSize, 0);
          if Length(Buffer) > 0 then
            Move(Buffer[0], (LRecBuf + FieldOfs)^, Min(Length(Buffer), BufferSize));
        end;
        BufferToBoldElement(Field.FieldNo - 1, LRecBuf);
      end;
    end
    else {fkCalculated, fkLookup}
    begin
      // A null flag byte, then the value
      Inc(LRecBuf, FRecordSize + Offset);
      PByte(LRecBuf)^ := Ord(Length(Buffer) > 0);
      if Length(Buffer) > 0 then
        Move(Buffer[0], (LRecBuf + 1)^, Min(Length(Buffer), DataSize));
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Field));
  end;
end;


{$IFDEF FPC}
procedure TBoldAbstractDataSet.GetBookmarkData(Buffer: TRecBuf; Data: Pointer);
begin
  // A bookmark holds the record's element
  PPointer(Data)^ := Pointer(GetBookmarkInfo(TRecordBuffer(Buffer))^.Element);
end;

procedure TBoldAbstractDataSet.SetBookmarkData(Buffer: TRecBuf; Data: Pointer);
begin
  if Assigned(Data) then
    GetBookmarkInfo(TRecordBuffer(Buffer))^.Element := TBoldElement(PPointer(Data)^)
  else
    GetBookmarkInfo(TRecordBuffer(Buffer))^.Element := nil;
end;

procedure TBoldAbstractDataSet.InternalGotoBookmark(ABookmark: Pointer);
var
  LIndex: integer;
begin
  if Assigned(ABookmark) then
    LIndex := GetIndexForElement(TBoldElement(PPointer(ABookmark)^))
  else
    LIndex := -1;
  if LIndex >= 0 then
    FRecordPos := LIndex;
end;
{$ELSE}
procedure TBoldAbstractDataSet.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  // A bookmark holds the record's element
  if Length(Data) >= SizeOf(Pointer) then
    PPointer(@Data[0])^ := Pointer(GetBookmarkInfo(TRecordBuffer(Buffer))^.Element);
end;
{$ENDIF}








function TBoldAbstractDataSet.GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag;
begin
  Result := GetBookmarkInfo(TRecordBuffer(Buffer))^.Flag;
end;
procedure TBoldAbstractDataSet.SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);
begin
  GetBookmarkInfo(TRecordBuffer(Buffer))^.Flag := Value;
end;

function TBoldAbstractDataSet.GetIndexForElement(AElement: TBoldElement): integer;
begin
  if Assigned(AElement) and Assigned(List) then
    try
      result := List.IndexOf(AElement)
    except
      result := -1;
    end
  else
    result := -1;
end;

function TBoldAbstractDataSet.GetIndexForBookmark(const Bookmark: TBookmark): integer;
begin
  if Length(Bookmark) >= SizeOf(Pointer) then
    result := GetIndexForElement(TBoldElement(PPointer(@Bookmark[0])^))
  else
    result := -1;
end;

{$IFNDEF FPC}
procedure TBoldAbstractDataSet.InternalGotoBookmark(Bookmark: TBookmark);
var
  LIndex: integer;
begin
  LIndex := GetIndexForBookmark(Bookmark);
  if LIndex >= 0 then
    FRecordPos := LIndex;
end;

{$ENDIF}

procedure TBoldAbstractDataSet.InternalSetToRecord(Buffer: TRecBuf);
var
  LIndex: integer;
begin
  LIndex := GetIndexForElement(GetBookmarkInfo(TRecordBuffer(Buffer))^.Element);
  if LIndex >= 0 then
    FRecordPos := LIndex;
end;

{$IFNDEF FPC}
procedure TBoldAbstractDataSet.SetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  if Length(Data) >= SizeOf(Pointer) then
    GetBookmarkInfo(TRecordBuffer(Buffer))^.Element := TBoldElement(PPointer(@Data[0])^)
  else
    GetBookmarkInfo(TRecordBuffer(Buffer))^.Element := nil;
end;
{$ENDIF}
procedure TBoldAbstractDataSet.InternalFirst;
begin
  FRecordPos := -1;
end;
procedure TBoldAbstractDataSet.InternalLast;
begin
  FRecordPos := List.Count;
end;



procedure TBoldAbstractDataSet.InternalClose;
begin
  BindFields(False);
  {$IFDEF FPC}
  if DefaultFields then
    DestroyFields;
  {$ELSE}
  // DestroyFields frees only the fields TDataSet created itself
  DestroyFields;
  {$ENDIF}
  fInternalCursorReady := false;
end;

procedure TBoldAbstractDataSet.InternalHandleException;
begin
  BoldEffectiveEnvironment.HandleDesigntimeException(Self);
end;
procedure TBoldAbstractDataSet.InternalDelete;
begin
  ActiveBoldObject.Delete;
  if List.Count = 0 then
    FRecordPos := -1;  
end;



{$IFNDEF FPC}
procedure TBoldAbstractDataSet.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
begin
  // TDataSet.AddRecord (AppendRecord, InsertRecord) calls DoOnNewRecord first,
  // which created the element already; a second AddNew would add a second one
  FRecordPos := List.IndexOf(GetBookmarkInfo(TRecordBuffer(Buffer))^.Element);
end;
{$ENDIF}





procedure TBoldAbstractDataSet.InternalOpen;
begin
  fInternalCursorReady := Assigned(BoldHandle) and BoldHandle.Enabled;
  if not fInternalCursorReady then Exit;
  FRecordPos := -1;
  // The type the fields are built for. Without it the first beValueIdentityChanged
  // (the handle's current element moving, as DoAfterScroll does) looks like a new
  // type and LayoutChanged closes and reopens the dataset on its first record.
  fLastKnownType := ListElementType;

  if Assigned(BoldHandle.ListElementType) then
    InternalInitFieldDefs;
  {$IFDEF FPC}
  if DefaultFields then
    CreateFields;
  {$ELSE}
  // CreateFields keeps persistent fields, as FieldOptions.AutoCreateMode says
  CreateFields;
  {$ENDIF}

  BindFields(True);

  BookmarkSize := SizeOf(TBDSBookmarkInfo);
  FBookmarkOfs := FRecordSize + CalcFieldsSize;
  FBufferSize := FBookmarkOfs + BookmarkSize;
end;

procedure TBoldAbstractDataSet.InternalPost;
begin
  FRecordPos := List.IndexOf(ActiveBoldElement);
  if (FRecordPos <> FListFollower.Follower.CurrentIndex) then
  begin
    FListFollower.SetFollowerIndex(FRecordPos);
    CursorPosChanged;
  end;
end;



function TBoldAbstractDataSet.IsCursorOpen: Boolean;
begin
  Result := fInternalCursorReady ;
end;








function TBoldAbstractDataSet.GetRecordCount: Integer;
begin
  if not Assigned(List) then
    result := 0
  else
    Result := List.Count;
end;

function TBoldAbstractDataSet.GetRecNo: Integer;
var
  ActiveBuf: TRecordBuffer;
  LIndex: integer;
begin
  Result := 0;
  if GetActiveRecBuf(ActiveBuf) then
  begin
    LIndex := List.IndexOf(GetBookmarkInfo(ActiveBuf)^.Element);
    if (LIndex >= 0) and (LIndex < RecordCount) then
      Result :=  LIndex + 1;
  end;
end;

procedure TBoldAbstractDataSet.InternalInitFieldDefs;
var
  i: integer;
begin
  FRecordSize := 0;
  FieldDefs.Clear;

  for i := 0 to FieldDescriptions.Count - 1 do
  begin
    with FieldDescriptions[i] do
    begin
      TFieldDef.Create(FieldDefs, EffectiveFieldName, FieldType, FieldSize, Required, i + 1);
      fFieldOfs := fRecordSize;
      fRecordSize := fRecordSize + BufferSize;
    end;
  end;
end;

procedure TBoldAbstractDataSet.LayoutChanged;
var
  WasActive: boolean;
begin
  WasActive := Active;
  FLayoutChanged := True;
  if WasActive then
    Close;

  FieldDescriptions.InvalidateFields;

  if (WasActive or AutoOpen) and not (csDestroying in componentState) then
    TryToOpen;
end;

function TBoldAbstractDataSet.GetBoldHandle: TBoldAbstractListHandle;
begin
  result := FListFollower.BoldHandle;
end;

procedure TBoldAbstractDataSet.SetBoldHandle(NewValue: TBoldAbstractListHandle);
begin
  if fListFollower.BoldHandle <> NewValue then
  begin
    fListFollower.BoldHandle := NewValue;
    fContextSubscriber.CancelAllSubscriptions;
    if assigned(NewValue) then
    begin
      NewValue.addsubscription(fContextSubscriber, beDestroying, beDestroying);
      NewValue.addsubscription(fContextSubscriber, beValueIdentityChanged, beValueIdentityChanged);
      if assigned(NewValue.List) then
        NewValue.List.addsubscription(fContextSubscriber, beItemDeleted, beItemDeleted);
    end;
  end;
end;

procedure TBoldAbstractDataSet.SetFieldDescriptions(const Value: TBoldDataSetFieldDescriptions);
begin
  FieldDescriptions.Assign(Value);
end;

procedure TBoldAbstractDataSet.TypeMayHaveChanged;
var
  NewType: TBoldElementTypeInfo;
begin
  NewType := ListElementType;
  if (NewType <> fLastKnownType) then
  begin
    fLastKnownType := NewType;
    TypeHasChanged;
  end;
end;

procedure TBoldAbstractDataSet.TypeHasChanged;
begin
  LayOutChanged;
end;

procedure TBoldAbstractDataSet.Loaded;
begin
  if not (csdesigning in ComponentState) and (FieldDescriptions.Count = 0) then
    CreateDefaultFields; 
  inherited;
  if AutoOpen and not Active then
    TryToOpen;
end;

procedure TBoldAbstractDataSet.CreateDefaultFields;
var
  i: integer;
  NewField: TBoldDataSetFieldDescription;
begin
  try
    DeleteAllFields;
    if Assigned(ListElementType) then
    begin
      if (ListElementType is TBoldClassTypeInfo) then
      begin
        with ListElementType as TBoldClassTypeInfo do
        begin
          for i := 0 to AllMembers.Count - 1 do
            if AllMembers[i].IsAttribute then
            begin
              NewField := FieldDescriptions.Add;
              NewField.Boldproperties.Expression := AllMembers[i].ExpressionName;
            end;
        end;
      end
      else if (ListElementType is TBoldAttributeTypeInfo) then
      begin
        FieldDescriptions.Add;
      end
      else if (ListElementType is TBoldListTypeInfo) then
      begin
        FieldDescriptions.Add;
      end;
    end;
  finally
    LayoutChanged;
  end;
end;

procedure TBoldAbstractDataSet.DeleteAllFields;
begin
  while FieldDescriptions.Count > 0 do
    FieldDescriptions.Delete(0);
end;

procedure TBoldAbstractDataSet.SetRecNo(Value: Integer);
begin
end;

{ Re-reads the records after a Bold-side change: the first change at once,
  and the ones that follow within cRefreshInterval together, once the interval
  is over. BoldSharedClock delivers that on the main thread through
  CheckSynchronize, which VCL and FMX call from their idle loops and a console
  application calls itself. It used to be a VCL TTimer, which needs a message
  loop. }
procedure TBoldAbstractDataSet.DebouncedResync;
begin
  if not fRefreshWindowOpen then
  begin
    fRefreshPending := False;
    UpdateCursorPos;
    Resync([]);
  end
  else
    fRefreshPending := True;
  if BoldSharedClock <> nil then
  begin
    fRefreshWindowOpen := True;
    BoldSharedClock.Schedule(Now + cRefreshInterval, DebouncedResyncDue);
  end;
end;

{ As DebouncedResync, for a caller that is in the middle of reading a record:
  never at once, always when the interval is over. }
procedure TBoldAbstractDataSet.ResyncLater;
begin
  if BoldSharedClock = nil then
    Exit;
  fRefreshPending := True;
  if not fRefreshWindowOpen then
  begin
    fRefreshWindowOpen := True;
    BoldSharedClock.Schedule(Now + cRefreshInterval, DebouncedResyncDue);
  end;
end;

procedure TBoldAbstractDataSet.DebouncedResyncDue;
begin
  fRefreshWindowOpen := False;
  if not fRefreshPending then
    Exit;
  fRefreshPending := False;
  // The dataset may have been closed while the refresh was due
  if not Active then
    Exit;
  UpdateCursorPos;
  Resync([]);
end;


end.
