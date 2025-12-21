program LogBridgeDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  BoldLogInterfaces,
  BoldLogSinks;

const
  LogFileName = 'LogBridgeDemo.log';

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
  BoldLog('APP: Starting business operation...');
  BoldLog('APP: Processing user request...');
  BoldLog('APP: Operation complete.');
end;

var
  FileSink: TBoldFileLogSink;
begin
  try
    WriteLn('=== Bold Log Bridge Demo ===');
    WriteLn('All logging output goes to: ' + LogFileName);
    WriteLn;

    // Register file sink to capture all Bold logs
    FileSink := TBoldFileLogSink.Create(LogFileName);
    FileSink.Levels := AllBoldLogLevels;
    BoldLogManager.RegisterSink(FileSink);

    BoldLog('=== Demo Started ===');
    BoldLog('');

    // Simulate mixed application and Bold logging
    BoldLog('--- Simulating application flow ---');
    SimulateApplicationCalls;
    BoldLog('');

    BoldLog('--- Bold framework logs ---');
    SimulateBoldFrameworkCalls;
    BoldLog('');

    // Demonstrate filtering
    BoldLog('--- Now filtering to errors only ---');
    FileSink.Levels := [llError];
    BoldLog('This info message will NOT appear');
    BoldLog('This warning will NOT appear', llWarning);
    BoldLog('This error WILL appear', llError);
    BoldLog('');

    // Restore all levels
    FileSink.Levels := AllBoldLogLevels;
    BoldLog('--- Restored all log levels ---');
    BoldLog('This info message will appear again');
    BoldLog('');

    BoldLog('=== Demo Complete ===');

    WriteLn('Demo finished. Check ' + LogFileName + ' for output.');
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
end.
