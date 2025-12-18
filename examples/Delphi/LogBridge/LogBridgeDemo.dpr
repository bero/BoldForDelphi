program LogBridgeDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  BoldLogInterfaces,
  BoldLogSinks,
  BoldAppLogSink;

procedure SimulateBoldFrameworkCalls;
begin
  // These represent calls that would happen inside Bold framework
  BoldLog('ObjectSpace initialized');
  BoldLog('Loading model from database...', llDebug);
  BoldLog('Loaded %d classes', [42]);
  BoldLog('Connection pool low on connections', llWarning);
  BoldLog('Failed to acquire lock on object', llError);
end;

procedure SimulateApplicationCalls;
begin
  // These represent your application's own logging
  WriteLn('APP: Starting business operation...');
  WriteLn('APP: Processing user request...');
  WriteLn('APP: Operation complete.');
end;

var
  AppSink: TBoldAppLogSink;
begin
  try
    WriteLn('=== Bold Log Bridge Demo ===');
    WriteLn;

    // Register our custom sink that bridges to the app's logging
    AppSink := TBoldAppLogSink.Create('[Bold]');
    AppSink.Levels := AllBoldLogLevels; // Receive all levels for demo
    BoldLogManager.RegisterSink(AppSink);

    WriteLn('Registered AppSink - Bold logs will now appear in app log');
    WriteLn;

    // Simulate mixed application and Bold logging
    WriteLn('--- Simulating application flow ---');
    WriteLn;

    SimulateApplicationCalls;
    WriteLn;

    WriteLn('--- Bold framework logs (via bridge) ---');
    WriteLn;

    SimulateBoldFrameworkCalls;
    WriteLn;

    // Demonstrate filtering
    WriteLn('--- Now filtering to errors only ---');
    WriteLn;

    AppSink.Levels := [llError];
    BoldLog('This info message will NOT appear');
    BoldLog('This warning will NOT appear', llWarning);
    BoldLog('This error WILL appear', llError);
    WriteLn;

    // Demonstrate multiple sinks
    WriteLn('--- Adding file sink alongside app sink ---');
    WriteLn;

    var FileSink := TBoldFileLogSink.Create('bold_demo.log');
    FileSink.Levels := AllBoldLogLevels;
    BoldLogManager.RegisterSink(FileSink);

    BoldLog('This goes to both app log (errors only) and file (all levels)');
    BoldLog('File will have this, app log will not', llInfo);
    BoldLog('Both will have this error', llError);

    WriteLn('Check bold_demo.log for file output');
    WriteLn;

    WriteLn('=== Demo Complete ===');
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
end.
