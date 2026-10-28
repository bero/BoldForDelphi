
{ Global compiler directives }
{$include bold.inc}
unit BoldEnvironmentIDE;

interface

implementation

uses
  BoldEnvironment;

initialization
  BoldInternalRunningInIDE := true;
end.
