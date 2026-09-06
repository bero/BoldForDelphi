unit SQLiteUniProvider;

{ Empty twin of UniDAC's SQLiteUniProvider for UniDAC installations without the
  SQLite provider (the vendored 10.4 copy in Attracs-Common). This folder is the
  LAST entry on the DebugUniDAC search path, so a real provider unit found under
  $(UniDAC)\Source\UniProviders\SQLite always takes precedence; only when none
  exists does the compiler fall back to this unit. Same idea as UniDACStubs and
  Source\Common\Stubs: no $IFDEF anywhere. With this stub linked, Engine=SQLite
  fails at run time with an unknown-provider error, which is the truth. }

interface

implementation

end.
