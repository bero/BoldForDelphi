{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program BoldZeosRegistrationCheck;
{ Checks what the Lazarus design-time package dclBoldZeos registers, without
  the IDE (#117): it catches the calls of BoldZeosLazarusReg.Register, checks
  that TBoldDatabaseAdapterZeos is on the page Bold Persistence with its
  palette icon, and writes a data module with a TZConnection, the adapter and
  a TBoldPersistenceHandleDB that refer to each other as the text of a form
  file and reads it back.
  One line per check; exit code = number of failed checks. }

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils,
  Classes,
  ZConnection,
  BoldSQLDatabaseConfig,
  BoldPersistenceHandleDB,
  BoldDatabaseAdapterZeos,
  BoldZeosLazarusReg,
  BoldRegistrationChecks;

type
  { A data module as a form designer would hold it: the components are
    owned by it and found by name }
  TCheckModule = class(TDataModule)
  end;

procedure BuildModule(AModule: TComponent);
var
  Connection: TZConnection;
  Adapter: TBoldDatabaseAdapterZeos;
  Persistence: TBoldPersistenceHandleDB;
begin
  Connection := TZConnection.Create(AModule);
  Connection.Name := 'ZConnection1';
  Connection.Protocol := 'sqlite';
  Connection.Database := 'invoicing.db';
  Adapter := TBoldDatabaseAdapterZeos.Create(AModule);
  Adapter.Name := 'BoldDatabaseAdapterZeos1';
  Adapter.Connection := Connection;
  Adapter.DatabaseEngine := dbeGenericANSISQL92;
  Persistence := TBoldPersistenceHandleDB.Create(AModule);
  Persistence.Name := 'BoldPersistenceHandleDB1';
  Persistence.DatabaseAdapter := Adapter;
end;

procedure CheckModuleRoundTrips;
var
  Original, Reloaded, ReloadedAgain: TCheckModule;
  Written, WrittenAgain, WrittenThird: AnsiString;
  Connection: TZConnection;
  Adapter: TBoldDatabaseAdapterZeos;
  Persistence: TBoldPersistenceHandleDB;
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
      // Not Written = WrittenAgain: a TZConnection that was read from a form
      // writes four more of its format settings (SecondFractionSeperator)
      // than a new one. From then on the text is stable, so a form saved
      // twice in the designer does not change.
      Check('the data module, once read, is written the same again', WrittenAgain = WrittenThird,
        IntToStr(Length(WrittenAgain)) + ' characters, ' + IntToStr(Reloaded.ComponentCount) + ' components');
      Connection := Reloaded.FindComponent('ZConnection1') as TZConnection;
      Adapter := Reloaded.FindComponent('BoldDatabaseAdapterZeos1') as TBoldDatabaseAdapterZeos;
      Persistence := Reloaded.FindComponent('BoldPersistenceHandleDB1') as TBoldPersistenceHandleDB;
      Check('the adapter finds its connection',
        Assigned(Connection) and (Adapter.Connection = Connection), string(Adapter.Connection.Protocol));
      Check('the adapter keeps its database engine', Adapter.DatabaseEngine = dbeGenericANSISQL92,
        IntToStr(Ord(Adapter.DatabaseEngine)));
      Check('the persistence handle finds the adapter', Persistence.DatabaseAdapter = Adapter,
        string(Persistence.DatabaseAdapter.Name));
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
    BoldZeosLazarusReg.Register;
    // In the IDE Zeos's and Bold's own design-time packages register these
    RegisterClasses([TZConnection, TBoldPersistenceHandleDB]);

    Writeln('Registration');
    Check('components registered', Length(Registered) = 1, IntToStr(Length(Registered)));
    Check('on the page Bold Persistence', CountOnPage('Bold Persistence') = 1, IntToStr(CountOnPage('Bold Persistence')));
    Check('TBoldDatabaseAdapterZeos is registered', IsRegistered(TBoldDatabaseAdapterZeos), 'TBoldDatabaseAdapterZeos');
    Check('TBoldDatabaseAdapterZeos has its palette icon', PaletteIconSize(TBoldDatabaseAdapterZeos) = '24 x 24',
      PaletteIconSize(TBoldDatabaseAdapterZeos));

    Writeln('Every component, written as form text and read back');
    CheckEveryComponentRoundTrips;

    Writeln('A data module with a connection, the adapter and a persistence handle');
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
