
{ Global compiler directives }
{$include bold.inc}
unit BoldDefsDT;

interface
uses
  BoldDefs;

const
{$IFDEF BOLD_DELPHI6}
  HelpFile = 'boldd6.hlp';
{$ENDIF}
{$IFDEF BOLD_DELPHI7}
  HelpFile = 'boldd7.hlp';
{$ENDIF}

  ATTRIBUTEWIZARDHELPFILE = 'boldattributewizard.hlp';
  MODELEDITORHELPFILE = 'boldmodeleditor.hlp';

  URLBoldForDelphi = 'https://github.com/Embarcadero/BoldForDelphi/tree/develop';
  URLSupport = 'http://www.forum.boldfordelphi.com';

  GETTINGSTARTEDPATH = '\doc\frames\';
  GETTINGSTARTEDDOCNAME = 'getting_started_f.html';

  regPath = 'Path';
  regPathRoot = 'RootDir';

  {Delphi Registry keys}
  regLibrary = 'Library';
  regSearchPath = 'Search Path';

  { Component editor verbs }
  BOLD_OPENUMLEDITOR = 'Open Bold UML Editor';

  { Delphi syntax symbols }
  BOLDSYM_ASSIGNMENT = ':=';
  BOLDSYM_BEGIN = 'begin';
  BOLDSYM_END = 'end;';
  BOLDSYM_THEN = 'then';
  BOLDSYM_ASSIGNED = 'Assigned';
  BOLDSYM_AND = 'and';
  BOLDSYM_TYPEINTEGER = '';
  BOLDSYM_RETURNRESULT = '';
  BOLDSYM_POINTERDEREFERENCE = '.';
  BOLDSYM_QUOTECHAR = '''';

implementation

end.
