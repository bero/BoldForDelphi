unit fMain;

interface

uses
  Types,
  UITypes,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Dialogs,
  Forms,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  BoldSubscription,
  BoldHandle,
  BoldAbstractModel,
  BoldModel,
  BoldHandles,
  BoldSystemHandle,
  ConwayClasses,
  BoldReferenceHandle,
  BoldPropertiesController,
  BoldTrackBar,
  BoldLabel,
  BoldMemo,
  BoldCheckBox;

type
  TfrmMain = class(TForm)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    btnTick: TButton;
    btnClear: TButton;
    btnStart: TButton;
    Timer1: TTimer;
    refGame: TBoldReferenceHandle;
    BoldLabel3: TBoldLabel;
    Label4: TLabel;
    BoldMemo1: TBoldMemo;
    Label1: TLabel;
    Label2: TLabel;
    BoldLabel1: TBoldLabel;
    btbFontSize: TBoldTrackBar;
    bpcFontSize: TBoldPropertiesController;
    Label3: TLabel;
    BoldLabel2: TBoldLabel;
    BoldTrackBar1: TBoldTrackBar;
    bpcTimerInterval: TBoldPropertiesController;
    BoldLabel4: TBoldLabel;
    bcbCollecting: TBoldCheckBox;
    btnLoad: TButton;
    btnSave: TButton;
    btnHelp: TButton;
    dlgPattern: TOpenDialog;
    dlgSavePattern: TSaveDialog;
    procedure btnTickClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure BoldMemo1Click(Sender: TObject);
    procedure BoldMemo1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    function GetGame: TGame;
  private
    { Private declarations }
    property Game: TGame read GetGame;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation      

{$R *.DFM}

procedure TfrmMain.btnTickClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Game.Tick;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  Game.ClearCells;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
  if Timer1.Enabled then
    (Sender as TButton).Caption := 'Stop ticking'
  else
    (Sender as TButton).Caption := 'Start ticking'
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  refGame.Value := TGame.Create(nil);
end;

{ The board is plain text: a live cell is an asterisk and everything else is
  blank. ConwayClasses declares the same two characters, but privately, so they
  are repeated here rather than reached into. }
const
  ACTIVECELL = '*';
  EMPTYCELL = ' ';

resourcestring
  sHelp =
    'Conway''s Game of Life, with every living cell a Bold object and the ' +
    'board itself a derived attribute of the game.' + sLineBreak + sLineBreak +

    'DRAWING' + sLineBreak +
    'Click a square to turn it on or off. Clicking just past the end of a ' +
    'row extends that row, which is how you draw on an empty board.' + sLineBreak +
    'You can also type straight into the board: an asterisk is a living ' +
    'cell and a space is an empty one.' + sLineBreak + sLineBreak +

    'PATTERNS' + sLineBreak +
    'Load pattern reads a starting board from a text file. Save pattern ' +
    'writes the current board back out, so you can keep anything you draw.' + sLineBreak +
    'Initial.txt and zip2.lif.txt come with the demo. Life 1.05 files, ' +
    'which mark an empty cell with a full stop and begin their comment ' +
    'lines with #, are translated as they are loaded.' + sLineBreak + sLineBreak +

    'RUNNING' + sLineBreak +
    'One generation advances the board a single step. Start ticking runs it ' +
    'on a timer. Set the interval on the track bar first: it starts at zero, ' +
    'and a timer with no interval never fires.' + sLineBreak + sLineBreak +

    'THE RULES' + sLineBreak +
    'Every cell has eight neighbours, counting diagonals. Moving from one ' +
    'generation to the next, all cells change at the same instant:' + sLineBreak +
    '   - a living cell with two or three living neighbours stays alive,' + sLineBreak +
    '   - an empty cell with exactly three living neighbours comes to life,' + sLineBreak +
    '   - every other cell ends up empty.' + sLineBreak + sLineBreak +
    'Too few neighbours and a cell dies of isolation, too many and it dies ' +
    'of overcrowding. Everything the board does follows from those rules.';

{ Toggles the cell under the mouse.

  Writing the board attribute is what does the work. It is reverse derived, so
  assigning to it runs TGame._board_ReverseDerive, which rebuilds the cell
  objects from the text. Editing one character here is therefore the same
  operation as typing into the memo, only aimed. }
procedure TfrmMain.BoldMemo1Click(Sender: TObject);
var
  Caret: TPoint;
  Rows: TStringList;
  Row: string;
  Col: Integer;
begin
  if not Assigned(Game) then
    Exit;
  Caret := BoldMemo1.CaretPos;
  Rows := TStringList.Create;
  try
    Rows.Text := BoldMemo1.Lines.Text;
    // A caret clamps to the text it is in, so a click below the last line or
    // past the end of a line lands on the nearest real position. Padding from
    // there lets a click just off the right edge still extend the row, which is
    // what makes an empty board usable.
    while Rows.Count <= Caret.Y do
      Rows.Add('');
    Row := Rows[Caret.Y];
    Col := Caret.X + 1;
    if Length(Row) < Col then
      Row := Row + StringOfChar(EMPTYCELL, Col - Length(Row));
    if Row[Col] = ACTIVECELL then
      Row[Col] := EMPTYCELL
    else
      Row[Col] := ACTIVECELL;
    Rows[Caret.Y] := Row;
    Game.Board := Rows.Text;
  finally
    Rows.Free;
  end;
end;

{ Keeps the board to the only two characters that mean anything.

  Anything else is not an error as such: _board_ReverseDerive treats every
  character that is not an asterisk as an empty cell, and the next derivation
  rewrites the text from the objects, so a stray letter would vanish by itself.
  Refusing it at the keyboard just makes that obvious straight away rather than
  one generation later.

  Characters below a space are let through so that Backspace, Enter and the
  arrow keys keep working. Pasting is not filtered here; text arriving that way
  is normalised by the derivation as described above. }
procedure TfrmMain.BoldMemo1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Key >= ' ') and (Key <> ACTIVECELL) and (Key <> EMPTYCELL) then
    Key := #0;
end;

{ Loads a starting pattern from a text file.

  Two conventions are accepted. This demo's own files use an asterisk and a
  space, and Life 1.05 files use an asterisk and a full stop, with '#' starting
  a header or comment line. Translating the second into the first is the whole
  of abcXYZ123!?/#* *the import. }
procedure TfrmMain.btnLoadClick(Sender: TObject);
var
  Pattern: TStringList;
  Row: string;
  i: Integer;
begin
  if not Assigned(Game) then
    Exit;
  if dlgPattern.InitialDir = '' then
    dlgPattern.InitialDir := ExtractFilePath(Application.ExeName);
  if not dlgPattern.Execute then
    Exit;

  Pattern := TStringList.Create;
  try
    Pattern.LoadFromFile(dlgPattern.FileName);
    for i := 0 to Pattern.Count - 1 do
    begin
      Row := Pattern[i];
      if (Row <> '') and (Row[1] = '#') then
        Row := ''
      else
        Row := StringReplace(Row, '.', EMPTYCELL, [rfReplaceAll]);
      Pattern[i] := Row;
    end;
    Game.Board := Pattern.Text;
  finally
    Pattern.Free;
  end;
end;

{ Writes the board out in this demo's own format, asterisk and space. }
procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  Pattern: TStringList;
begin
  if not Assigned(Game) then
    Exit;
  if dlgSavePattern.InitialDir = '' then
    dlgSavePattern.InitialDir := ExtractFilePath(Application.ExeName);
  if not dlgSavePattern.Execute then
    Exit;

  Pattern := TStringList.Create;
  try
    Pattern.Text := Game.Board;
    Pattern.SaveToFile(dlgSavePattern.FileName);
  finally
    Pattern.Free;
  end;
end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  MessageDlg(sHelp, mtInformation, [mbOK], 0);
end;

function TfrmMain.GetGame: TGame;
begin
  Result := refGame.Value as TGame;
end;

end.

