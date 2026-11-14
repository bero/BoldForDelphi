{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit BoldRegistrationChecks;
{ What BoldRegistrationCheck and BoldZeosRegistrationCheck share (#117):
  catching the RegisterComponents calls of a design-time package's Register
  procedure without the IDE, writing a component as the text of a form file
  and reading it back, looking for a component's palette icon, and asking which packages a
  project gets when a component is dropped on a form. }

interface

uses
  Classes;

type
  TRegistered = record
    Page: string;
    ComponentClass: TComponentClass;
  end;

var
  Failures: Integer = 0;
  Registered: array of TRegistered;

procedure Check(const AWhat: string; APassed: Boolean; const AActual: string);
{ Classes.RegisterComponents calls this; in the IDE the palette's procedure is here }
procedure CatchRegisterComponents(const Page: AnsiString; ComponentClasses: array of TComponentClass);
function CountOnPage(const APage: string): Integer;
function IsRegistered(AClass: TComponentClass): Boolean;
{ The component as the text of a form file }
function ComponentToText(AComponent: TComponent): AnsiString;
{ Reads the text into AInstance (the root, already created) or, with nil,
  creates the component the text names }
function TextToComponent(const AText: AnsiString; AInstance: TComponent): TComponent;
{ The form text without its first line, which has the name of the root: with
  the LCL linked in, a data module read while one of its name exists is
  renamed (CheckModule_1) }
function AfterFirstLine(const AText: AnsiString): AnsiString;
procedure CheckEveryComponentRoundTrips;
{ The palette icon of a component is a PNG in a resource named after the
  class, which the registration unit links in. Returns its size as
  'width x height', or why there is none. }
function PaletteIconSize(AClass: TComponentClass): string;
{ When a component is dropped on a form, the IDE adds to the project the
  package of the component's unit and the design-time package that
  registered it, and then lets the component's requirements
  (RegisterComponentRequirements) change that list. A project must get the
  runtime package only: with the design-time package it would link the
  registration code and the IDE's interface. }
procedure CheckNoComponentRequiresTheDesignPackage(const ARuntimePackage, ADesignPackage: string);

implementation

uses
  SysUtils,
  ComponentEditors;

function AfterFirstLine(const AText: AnsiString): AnsiString;
begin
  Result := Copy(AText, Pos(AnsiString(#10), AText) + 1, MaxInt);
end;

procedure CheckNoComponentRequiresTheDesignPackage(const ARuntimePackage, ADesignPackage: string);
var
  i, Wrong: Integer;
  Packages: TStringList;
  Requirements: TComponentRequirements;
  FirstWrong: string;
begin
  Wrong := 0;
  FirstWrong := '';
  Packages := TStringList.Create;
  try
    // As the IDE's list is
    Packages.Sorted := True;
    Packages.Duplicates := dupIgnore;
    for i := 0 to High(Registered) do
    begin
      Packages.Clear;
      Packages.Add(AnsiString(ARuntimePackage));
      Packages.Add(AnsiString(ADesignPackage));
      Requirements := GetComponentRequirements(Registered[i].ComponentClass);
      if Assigned(Requirements) then
        Requirements.RequiredPkgs(Packages);
      if string(Packages.CommaText) <> ARuntimePackage then
      begin
        Inc(Wrong);
        if FirstWrong = '' then
          FirstWrong := string(Registered[i].ComponentClass.ClassName) + ' requires ' + string(Packages.CommaText);
      end;
    end;
  finally
    Packages.Free;
  end;
  if Wrong = 0 then
    Check('a project that gets a component requires ' + ARuntimePackage + ', not ' + ADesignPackage, True,
      IntToStr(Length(Registered)) + ' components')
  else
    Check('a project that gets a component requires ' + ARuntimePackage + ', not ' + ADesignPackage, False,
      IntToStr(Wrong) + ' of ' + IntToStr(Length(Registered)) + ' components, e.g. ' + FirstWrong);
end;

procedure Check(const AWhat: string; APassed: Boolean; const AActual: string);
begin
  if APassed then
    Writeln('  OK    ', AWhat, ' = ', AActual)
  else
  begin
    Writeln('  FAIL  ', AWhat, ' = ', AActual);
    Inc(Failures);
  end;
end;

{ Classes.RegisterComponents calls this; in the IDE the palette's procedure is here }
procedure CatchRegisterComponents(const Page: AnsiString; ComponentClasses: array of TComponentClass);
var
  i: Integer;
begin
  for i := Low(ComponentClasses) to High(ComponentClasses) do
  begin
    SetLength(Registered, Length(Registered) + 1);
    Registered[High(Registered)].Page := string(Page);
    Registered[High(Registered)].ComponentClass := ComponentClasses[i];
    // What the IDE does too, so the classes are found when a form is read
    RegisterClass(ComponentClasses[i]);
  end;
end;

function CountOnPage(const APage: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Registered) do
    if Registered[i].Page = APage then
      Inc(Result);
end;

function IsRegistered(AClass: TComponentClass): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Registered) do
    if Registered[i].ComponentClass = AClass then
      Result := True;
end;

{ The component as the text of a form file }
function ComponentToText(AComponent: TComponent): AnsiString;
var
  Binary, Text: TMemoryStream;
begin
  // Bytes, not a string stream: in this mode a string is UTF-16
  Binary := TMemoryStream.Create;
  Text := TMemoryStream.Create;
  try
    Binary.WriteComponent(AComponent);
    Binary.Position := 0;
    ObjectBinaryToText(Binary, Text);
    SetString(Result, PAnsiChar(Text.Memory), Text.Size);
  finally
    Text.Free;
    Binary.Free;
  end;
end;

{ Reads the text into AInstance (the root, already created) or, with nil,
  creates the component the text names }
function TextToComponent(const AText: AnsiString; AInstance: TComponent): TComponent;
var
  Binary, Text: TMemoryStream;
begin
  Binary := TMemoryStream.Create;
  Text := TMemoryStream.Create;
  try
    Text.WriteBuffer(PAnsiChar(AText)^, Length(AText));
    Text.Position := 0;
    ObjectTextToBinary(Text, Binary);
    Binary.Position := 0;
    Result := Binary.ReadComponent(AInstance);
  finally
    Text.Free;
    Binary.Free;
  end;
end;

procedure CheckEveryComponentRoundTrips;
var
  i: Integer;
  Original, Reloaded: TComponent;
  Written, WrittenAgain: AnsiString;
  Name: string;
begin
  for i := 0 to High(Registered) do
  begin
    Name := string(Registered[i].ComponentClass.ClassName);
    Reloaded := nil;
    Original := Registered[i].ComponentClass.Create(nil);
    try
      try
        Original.Name := 'Component1';
        Written := ComponentToText(Original);
        Reloaded := TextToComponent(Written, nil);
        WrittenAgain := ComponentToText(Reloaded);
        Check(Name + ' reads back as written', (Reloaded.ClassType = Original.ClassType) and (Written = WrittenAgain),
          IntToStr(Length(Written)) + ' characters on ' + Registered[i].Page);
      except
        on E: Exception do
          Check(Name + ' reads back as written', False, string(E.ClassName) + ': ' + E.Message);
      end;
    finally
      Reloaded.Free;
      Original.Free;
    end;
  end;
end;

{ The palette icon of a component is a PNG in a resource named after the
  class, which the unit BoldLazarusReg links in. Returns its size as
  'width x height', or why there is none. }
function PaletteIconSize(AClass: TComponentClass): string;
const
  PNGSignature: array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);
var
  Stream: TResourceStream;
  Header: array[0..23] of Byte;
begin
  try
    Stream := TResourceStream.Create(HInstance, string(AClass.ClassName), RT_RCDATA);
    try
      if (Stream.Read(Header, SizeOf(Header)) <> SizeOf(Header)) or
        not CompareMem(@Header[0], @PNGSignature[0], SizeOf(PNGSignature)) then
        Result := 'not a PNG'
      else
        // The IHDR chunk follows the signature: width and height, big-endian
        Result := IntToStr(Header[18] shl 8 + Header[19]) + ' x ' + IntToStr(Header[22] shl 8 + Header[23]);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
      Result := 'no resource (' + string(E.ClassName) + ')';
  end;
end;

end.
