{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program BoldRegistrationCheck;
{ Checks what the Lazarus design-time package dclBold registers, without the
  IDE (#117): it catches the calls of BoldLazarusReg.Register, and for every
  registered component class creates an instance, writes it as the text of a
  form file and reads it back, as the form designer does when it saves and
  opens a form, and checks that it has its palette icon. It then does the same with a small data module: a TBoldModel
  holding the Invoicing example's model, a system handle, chained list
  handles, a persistence handle and a TBoldDataSet with fields, which refer
  to each other.
  One line per check; exit code = number of failed checks. }

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils,
  Classes,
  BoldEnvironment,
  BoldHandles,
  BoldSystemHandle,
  BoldListHandle,
  BoldFilteredHandle,
  BoldModel,
  BoldPersistenceHandleFileXML,
  BoldDataSet,
  BoldLazarusReg,
  BoldRegistrationChecks;

procedure CheckEveryComponentHasItsPaletteIcon;
var
  i: Integer;
  Size: string;
begin
  for i := 0 to High(Registered) do
  begin
    Size := PaletteIconSize(Registered[i].ComponentClass);
    // As in Delphi, where TBoldFilteredHandle has no bitmap and shows the default icon
    if Registered[i].ComponentClass = TBoldFilteredHandle then
      Check('TBoldFilteredHandle has no palette icon of its own', Pos('no resource', Size) = 1, Size)
    else
      Check(string(Registered[i].ComponentClass.ClassName) + ' has its palette icon', Size = '24 x 24', Size);
  end;
end;

type
  { A data module as a form designer would hold it: the components are
    owned by it and found by name }
  TCheckModule = class(TDataModule)
  end;

function InvoicingModelFileName: string;
begin
  // The program is in packages/Lazarus/tests/bin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' + PathDelim + '..' +
    PathDelim + '..' + PathDelim + 'examples' + PathDelim + 'Compound' + PathDelim + 'Invoicing' +
    PathDelim + 'Model' + PathDelim + 'Invoicing.bld');
end;

procedure BuildModule(AModule: TComponent);
var
  ModelText: TStringList;
  Model: TBoldModel;
  TypeInfoHandle: TBoldSystemTypeInfoHandle;
  Persistence: TBoldPersistenceHandleFileXML;
  SystemHandle: TBoldSystemHandle;
  Masters, Details: TBoldListHandle;
  DataSet: TBoldDataSet;
  Field: TBoldDataSetFieldDescription;
begin
  Model := TBoldModel.Create(AModule);
  Model.Name := 'BoldModel1';
  ModelText := TStringList.Create;
  try
    ModelText.LoadFromFile(InvoicingModelFileName);
    Model.SetFromModelAsString(ModelText);
  finally
    ModelText.Free;
  end;
  TypeInfoHandle := TBoldSystemTypeInfoHandle.Create(AModule);
  TypeInfoHandle.Name := 'BoldSystemTypeInfoHandle1';
  TypeInfoHandle.BoldModel := Model;
  Persistence := TBoldPersistenceHandleFileXML.Create(AModule);
  Persistence.Name := 'BoldPersistenceHandleFileXML1';
  Persistence.BoldModel := Model;
  Persistence.FileName := 'data.xml';
  SystemHandle := TBoldSystemHandle.Create(AModule);
  SystemHandle.Name := 'BoldSystemHandle1';
  SystemHandle.SystemTypeInfoHandle := TypeInfoHandle;
  SystemHandle.PersistenceHandle := Persistence;
  Masters := TBoldListHandle.Create(AModule);
  Masters.Name := 'blhMasters';
  Masters.StaticSystemHandle := SystemHandle;
  Masters.RootHandle := SystemHandle;
  Masters.Expression := 'Customer.allInstances';
  Details := TBoldListHandle.Create(AModule);
  Details.Name := 'blhDetails';
  Details.StaticSystemHandle := SystemHandle;
  Details.RootHandle := Masters;
  Details.Expression := 'invoices';
  DataSet := TBoldDataSet.Create(AModule);
  DataSet.Name := 'BoldDataSet1';
  DataSet.BoldHandle := Masters;
  Field := DataSet.FieldDescriptions.Add;
  Field.BoldProperties.Expression := 'name';
  Field.FieldName := 'Name';
  Field := DataSet.FieldDescriptions.Add;
  Field.BoldProperties.Expression := 'openBalance';
  Field.FieldName := 'Open balance';
end;

procedure CheckModuleRoundTrips;
var
  Original, Reloaded, ReloadedAgain: TCheckModule;
  Written, WrittenAgain, WrittenThird: AnsiString;
  OriginalModel, ReloadedModel: TBoldModel;
  SystemHandle: TBoldSystemHandle;
  Masters, Details: TBoldListHandle;
  DataSet: TBoldDataSet;
begin
  Reloaded := nil;
  ReloadedAgain := nil;
  Original := TCheckModule.CreateNew(nil);
  try
    try
      Original.Name := 'CheckModule';
      BuildModule(Original);
      Written := ComponentToText(Original);
      Reloaded := TCheckModule.CreateNew(nil);
      TextToComponent(Written, Reloaded);
      WrittenAgain := ComponentToText(Reloaded);
      ReloadedAgain := TCheckModule.CreateNew(nil);
      TextToComponent(WrittenAgain, ReloadedAgain);
      WrittenThird := ComponentToText(ReloadedAgain);
      // Not Written = WrittenAgain: a TBoldModel that was read from a form
      // writes its classes in another order than one filled from a .bld file,
      // and adds the tagged value Bold.CRC (the same classes; checked below).
      // From then on the text is stable, so a form saved twice in the
      // designer does not change.
      Check('the data module, once read, is written the same again', WrittenAgain = WrittenThird,
        IntToStr(Length(WrittenAgain)) + ' characters, ' + IntToStr(Reloaded.ComponentCount) + ' components');

      OriginalModel := Original.FindComponent('BoldModel1') as TBoldModel;
      ReloadedModel := Reloaded.FindComponent('BoldModel1') as TBoldModel;
      Check('the model comes back with its classes',
        (OriginalModel.MoldModel.Classes.Count > 5) and
        (ReloadedModel.MoldModel.Classes.Count = OriginalModel.MoldModel.Classes.Count),
        IntToStr(ReloadedModel.MoldModel.Classes.Count) + ' classes');
      SystemHandle := Reloaded.FindComponent('BoldSystemHandle1') as TBoldSystemHandle;
      Masters := Reloaded.FindComponent('blhMasters') as TBoldListHandle;
      Details := Reloaded.FindComponent('blhDetails') as TBoldListHandle;
      DataSet := Reloaded.FindComponent('BoldDataSet1') as TBoldDataSet;
      Check('the system handle finds its type info handle and its model',
        Assigned(SystemHandle.SystemTypeInfoHandle) and (SystemHandle.SystemTypeInfoHandle.Owner = Reloaded) and
        (SystemHandle.SystemTypeInfoHandle.BoldModel = Reloaded.FindComponent('BoldModel1')),
        string(SystemHandle.SystemTypeInfoHandle.Name));
      Check('the system handle finds its persistence handle',
        SystemHandle.PersistenceHandle = Reloaded.FindComponent('BoldPersistenceHandleFileXML1'),
        (SystemHandle.PersistenceHandle as TBoldPersistenceHandleFileXML).FileName);
      Check('the master list handle is rooted on the system handle',
        (Masters.RootHandle = SystemHandle) and (Masters.StaticSystemHandle = SystemHandle), Masters.Expression);
      Check('the detail list handle is rooted on the master', Details.RootHandle = Masters, Details.Expression);
      Check('the dataset finds its list handle', DataSet.BoldHandle = Masters, string(DataSet.BoldHandle.Name));
      Check('the dataset keeps its fields',
        (DataSet.FieldDescriptions.Count = 2) and
        (DataSet.FieldDescriptions[0].FieldName = 'Name') and
        (DataSet.FieldDescriptions[0].BoldProperties.Expression = 'name') and
        (DataSet.FieldDescriptions[1].FieldName = 'Open balance'),
        IntToStr(DataSet.FieldDescriptions.Count) + ' fields');
    except
      on E: Exception do
        Check('the data module is written and read', False, string(E.ClassName) + ': ' + E.Message);
    end;
  finally
    ReloadedAgain.Free;
    Reloaded.Free;
    Original.Free;
  end;
end;

begin
  try
    RegisterComponentsProc := CatchRegisterComponents;
    BoldLazarusReg.Register;

    Writeln('Registration');
    Check('components registered', Length(Registered) = 19, IntToStr(Length(Registered)));
    Check('on the page Bold Handles', CountOnPage('Bold Handles') = 12, IntToStr(CountOnPage('Bold Handles')));
    Check('on the page Bold Persistence', CountOnPage('Bold Persistence') = 2, IntToStr(CountOnPage('Bold Persistence')));
    Check('on the page Bold Misc', CountOnPage('Bold Misc') = 5, IntToStr(CountOnPage('Bold Misc')));
    Check('TBoldDataSet is registered', IsRegistered(TBoldDataSet), 'TBoldDataSet');
    Check('Bold knows that it runs in the IDE', BoldInternalRunningInIDE, BoolToStr(BoldInternalRunningInIDE, True));

    Writeln('Every component, written as form text and read back');
    CheckEveryComponentRoundTrips;

    Writeln('Every component has its palette icon');
    CheckEveryComponentHasItsPaletteIcon;

    Writeln('A data module with handles, a persistence handle and a dataset');
    CheckModuleRoundTrips;

    if Failures = 0 then
      Writeln('All checks passed')
    else
      Writeln(Failures, ' check(s) FAILED');
    ExitCode := Failures;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 255;
    end;
  end;
end.
