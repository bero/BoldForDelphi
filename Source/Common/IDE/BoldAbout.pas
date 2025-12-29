unit BoldAbout;

interface

uses
  Windows,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  {$IFNDEF NO_OTA}
  ToolsApi,
  {$ENDIF}
  ShellAPI,
  Buttons,
  ExtCtrls,
  Menus,
  ImgList,
  Graphics, System.ImageList;

type
  TfrmAboutBold = class(TForm)
    PageControl: TPageControl;
    BtnOK: TButton;
    TabAbout: TTabSheet;
    ImageLogoDelphi: TImage;
    Label14: TLabel;
    LabelProductName: TLabel;
    LabelVersion: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    Label12: TLabel;
    LabelURLLatestGit: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblDiscordSupport: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure URLOriginalGitClick(Sender: TObject);
    procedure URLLatestGitClick(Sender: TObject);
    procedure lblDiscordSupportClick(Sender: TObject);
  private
    procedure GetVersionInfo;
    procedure OpenURL(const URL: string);
  public
    {public declarations}
  end;

implementation

{$R *.dfm}

uses
  ActiveX,
  Registry,
  ShlObj,
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldRegistry,
  BoldDefs,
  BoldWinINet,
  BoldCursorGuard,
  BoldDefsDT;

const
  SubItemRelease = 0;
  SubItemStatus = 1;
  SubItemInfo = 2;
  SubItemProduct = 3;
  SubItemParams = 4;

function SelectDirectoryWithInitial(const Caption: string; const Root: WideString; var Directory: string): Boolean;
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;

  function BrowserCallback(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
  begin
    if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    begin
      SendMessage(Wnd, BFFM_SETSELECTION, Integer(LongBool(True)), lpData);
    end;
    Result := 0;
  end;

begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        lpfn := @BrowserCallback;
        if Directory<>'' then
          lParam := integer(pchar(Directory));
      end;
      WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

{*****************************************************************************
 * Form init and finalization
 *****************************************************************************}

procedure TfrmAboutBold.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage        := TabAbout;
  GetVersionInfo;
end;

{*****************************************************************************
 * Internal methods
 *****************************************************************************}

procedure TfrmAboutBold.GetVersionInfo;
type
  PLangCharSetInfo = ^TLangCharSetInfo;
  TLangCharSetInfo = record
    Lang: Word;
    CharSet: Word;
  end;
var
  FileName: array [0..260] of Char;
  SubBlock: array [0..255] of Char;
  VerHandle: Cardinal;
  Size: Word;
  Buffer: Pointer;
  Data: Pointer;
  DataLen: LongWord;
  LangCharSetInfo: PLangCharSetInfo;
  LangCharSetString: string;
begin
  {Get size and allocate buffer for VerInfo}
  if GetModuleFileName(hInstance, FileName, SizeOf(FileName)) > 0 then
  begin
    Size := GetFileVersionInfoSize(FileName, VerHandle);
    if Size > 0 then
    begin
      GetMem(Buffer, Size);
      try
        if GetFileVersionInfo(FileName, VerHandle, Size, Buffer) then
        begin
          {Query first language and that language blocks version info}
          if VerQueryValue(Buffer, '\VarFileInfo\Translation', Pointer(LangCharSetInfo), DataLen) then // do not localize
          begin
            LangCharSetString := IntToHex(LangCharSetInfo^.Lang, 4) +
                                 IntToHex(LangCharSetInfo^.CharSet, 4);
            if VerQueryValue(Buffer, StrPCopy(SubBlock, '\StringFileInfo\' + LangCharSetString + '\ProductName'), Data, DataLen) then // do not localize
            begin
              LabelProductName.Caption := StrPas(PChar(Data)); // marco
            Caption := LabelProductName.Caption;
          end;
            if VerQueryValue(Buffer, StrPCopy(SubBlock, '\StringFileInfo\' + LangCharSetString + '\FileVersion'), Data, DataLen) then // do not localize
              LabelVersion.Caption := StrPas(PChar(Data));
          end;
        end;
      finally
        FreeMem(Buffer, Size);
      end;
    end
  end;
end;


{*****************************************************************************
 * User events
 *****************************************************************************}

procedure TfrmAboutBold.OpenURL(const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), '', '', SW_SHOWNORMAL); // do not localize
end;

procedure TfrmAboutBold.URLLatestGitClick(Sender: TObject);
begin
  OpenURL(sURL_LatestGitForBold);
end;

procedure TfrmAboutBold.URLOriginalGitClick(Sender: TObject);
begin
  OpenURL(sURL_OriginalGitForBold);
end;

procedure TfrmAboutBold.lblDiscordSupportClick(Sender: TObject);
begin
  OpenURL(sURL_DiscordSupport);
end;

{Register}

function ReadURL(const URL: string): string;
var
  hInternetSession: HINTERNET;
  hURLFile: HINTERNET;
  Buffer: array [0..1024] of char;
  NumberOfBytesRead: DWORD;
begin
  Result := '';
  hInternetSession := BoldInternetOpen('BoldSoft', BOLD_INTERNET_OPEN_TYPE_PRECONFIG, '', '', 0); // do not localize
  if Assigned(hInternetSession) then
  try
    hURLFile := BoldInternetOpenUrl(hInternetSession, URL, '', BOLD_INTERNET_FLAG_RELOAD, 0);
    if Assigned(hURLFile) then
    try

      while BoldInternetReadFile(hURLFile, @Buffer, SizeOf(Buffer) - 1, NumberOfBytesRead) and
            (NumberOfBytesRead > 0) do
      begin
        Buffer[NumberOfBytesRead] := #0;
        Result := Result + Buffer;
      end
    finally
      BoldInternetCloseHandle(hURLFile);
    end;
  finally
    BoldInternetCloseHandle(hInternetSession);
  end;

  if SameText(Copy(Result, 1, 7), 'http://') then // do not localize
    Result := ReadURL(Result);
end;

end.
