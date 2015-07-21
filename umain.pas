{ TODO : OpenGL: illumination from model, spotlights, better 3D model }
{ TODO : Wavefront: add curves }
{ TODO : Welcome dialog: revamp completely }
{ TODO : acExit always possible }
{ TODO : pnPieces: variable docking }

{ Main program unit

  v3.1.3; 2015-Mar-01
  Copyleft (c) GPLv3: 1996-2015 Heiko Tietze heiko_tietze@web.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit umain;

{$mode objfpc}
{$H+}

interface

{$I conditions.inc}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ComCtrls,
  Menus, StdCtrls, ExtCtrls, Buttons, Inifiles, FileUtil, DateUtils, GraphUtil,
  LazUTF8, LCLType, LCLIntf, LMessages, LResources, LConvEncoding, TAGraph,
  TASeries, OpenGLContext, GraphType, Strutils, Process, types,
  {$ifdef UseOpenGl}uglscrabblegrid,uglcontext,{$endif}
  udgscrabblegrid, uscrabblegrid, ustatistics, ubruteforce, uwordsearch, unewgame,
  unetwork, uabout, upoll, uremotegames, udefaults, utcpclient, utcpserver,
  utcptypes, uspecialhint, uwelcome, umessages, ugamecourse, ulanguage, uletter,
  uscrabble, uboard, ugameoptions, utypes, upieces, urandom, uupdater, udictionary,
  uscrabblecube
  {$IFDEF Windows}, windirs, mmsystem, themes{$ENDIF}
  ;

const
  LM_SYNCMESSAGE = LM_USER + 1;
  LM_RESIZESTATUSBAR = LM_USER + 2;

type
  TTaggedStringList=class(TStringList)
    private
      FTag : Longword;
    public
      property Tag:Longword read FTag write FTag;
    end;

  { TfmMain }
  TfmMain = class(TForm)
    acNewGame: TAction;
    acSettings: TAction;
    acNextPlayer: TAction;
    acExit: TAction;
    acSave: TAction;
    acLoad: TAction;
    acNetwork: TAction;
    acBestMove: TAction;
    acScreenshot: TAction;
    acGameAnalysis: TAction;
    acRestore: TAction;
    acPause: TAction;
    acHelp: TAction;
    acHighscore: TAction;
    acKibitz: TAction;
    acDemo: TAction;
    acCambioSecco: TAction;
    acChallenge: TAction;
    acShufflePieces: TAction;
    acWordSearch: TAction;
    alMain: TActionList;
    bvStatus: TBevel;
    cbChatReceiver: TComboBox;
    chScore: TChart;
    bsScore: TBarSeries;
    edChat: TEdit;
    lbMessages: TLabel;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miRackHeader: TMenuItem;
    miLocalization: TMenuItem;
    mi3DToolbar: TMenuItem;
    miFormize: TMenuItem;
    miShuffle: TMenuItem;
    pbStatus: TPaintBox;
    pbPlayer: TPaintBox;
    pnStatus: TPanel;
    pbInfo: TPaintBox;
    pbPoints: TPaintBox;
    pnLeft: TPanel;
    pnPieces: TPanel;
    pnMessages: TPanel;
    pn3D: TPanel;
    pnPiecesHeader: TPanel;
    pn2DHeader: TPanel;
    pnMessagesHeader: TPanel;
    pn3DHeader: TPanel;
    pnRight: TPanel;
    pn2D: TPanel;
    pmRack: TPopupMenu;
    pm3D: TPopupMenu;
    pbProgress: TProgressBar;
    seBestMove: TEdit;
    ilMain: TImageList;
    ilSmallImages: TImageList;
    lbTimer: TLabel;
    miBest: TMenuItem;
    midx: TMenuItem;
    midz: TMenuItem;
    midy: TMenuItem;
    miFacebook: TMenuItem;
    miChallenge: TMenuItem;
    miSeparator7: TMenuItem;
    miState: TMenuItem;
    miHighscore: TMenuItem;
    miDemo: TMenuItem;
    miAssistant: TMenuItem;
    miSeparator6: TMenuItem;
    miNews: TMenuItem;
    miForum: TMenuItem;
    miUpdate: TMenuItem;
    miSeparator4: TMenuItem;
    miHelp: TMenuItem;
    miAbout: TMenuItem;
    miInformation: TMenuItem;
    miRefresh: TMenuItem;
    miKibitz: TMenuItem;
    miNetwork: TMenuItem;
    miMessage: TMenuItem;
    miInvite: TMenuItem;
    miLeave: TMenuItem;
    miTimeLeft: TMenuItem;
    miScore: TMenuItem;
    miLettersLeft: TMenuItem;
    miGameAnalysis: TMenuItem;
    miScreenshot: TMenuItem;
    miBestMove: TMenuItem;
    miSeparator5: TMenuItem;
    miWordSearch: TMenuItem;
    miSeparator: TMenuItem;
    miSeparator3: TMenuItem;
    miSave: TMenuItem;
    miLoad: TMenuItem;
    miSeparator2: TMenuItem;
    miSeparator1: TMenuItem;
    miNextPlayer: TMenuItem;
    miExit: TMenuItem;
    miNewGame: TMenuItem;
    miGame: TMenuItem;
    miConfiguration: TMenuItem;
    miSettings: TMenuItem;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    OpenDialog: TOpenDialog;
    pnPosition: TPanel;
    pnChat: TPanel;
    pcMessages: TPageControl;
    pmStatusInfo: TPopupMenu;
    pmGameServer: TPopupMenu;
    pmDimension3D: TPopupMenu;
    SaveDialog: TSaveDialog;
    spChat: TSplitter;
    spRight: TSplitter;
    spMain: TSplitter;
    tbHelp: TToolButton;
    tbHighscore: TToolButton;
    tbSeparator6: TToolButton;
    tbKibitz: TToolButton;
    tbCambioSecco: TToolButton;
    tbChallenge: TToolButton;
    tb3D: TToolBar;
    tbDimension: TToolButton;
    tbPosition: TToolButton;
    tbZoom: TToolButton;
    tbBrightness: TToolButton;
    tbBackward: TToolButton;
    tbForward: TToolButton;
    tbSeparator7: TToolButton;
    tbCubeReset: TToolButton;
    tbWordSearch: TToolButton;
    tiMain: TTimer;
    trPosition: TTrackBar;
    tvGameServer: TTreeView;
    tsGameServer: TTabSheet;
    tsMessages: TTabSheet;
    tsGameCourse: TTabSheet;
    tsScore: TTabSheet;
    tbMain: TToolBar;
    tbExit: TToolButton;
    tbSeparator1: TToolButton;
    tbNewGame: TToolButton;
    tbNextPlayer: TToolButton;
    tbSeparator2: TToolButton;
    tbSeparator5: TToolButton;
    tbSeparator3: TToolButton;
    tbNetwork: TToolButton;
    tbBestMove: TToolButton;
    tbRestore: TToolButton;
    tbSeparator4: TToolButton;
    tbPause: TToolButton;
    udBestMove: TUpDown;
    procedure acCambioSeccoExecute(Sender: TObject);
    procedure acChallengeExecute(Sender: TObject);
    procedure acDemoExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure acHighscoreExecute(Sender: TObject);
    procedure acKibitzExecute(Sender: TObject);
    procedure acLoadExecute(Sender: TObject);
    procedure acNetworkExecute(Sender: TObject);
    procedure acNewGameExecute(Sender: TObject);
    procedure acNextPlayerExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acRestoreExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acScreenshotExecute(Sender: TObject);
    procedure acSettingsExecute(Sender: TObject);
    procedure acShufflePiecesExecute(Sender: TObject);
    procedure acWordSearchExecute(Sender: TObject);
    procedure cbChatReceiverDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure cbChatReceiverMeasureItem(Contro1l: TWinControl; Index: Integer; var AHeight: Integer);
    procedure edChatKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mi3DToolbarClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miAssistantClick(Sender: TObject);
    procedure miBestClick(Sender: TObject);
    procedure miFacebookClick(Sender: TObject);
    procedure miFormizeClick(Sender: TObject);
    procedure miForumClick(Sender: TObject);
    procedure miInviteClick(Sender: TObject);
    procedure miKibitzClick(Sender: TObject);
    procedure miLeaveClick(Sender: TObject);
    procedure miLocalizationClick(Sender: TObject);
    procedure miMessageClick(Sender: TObject);
    procedure alMainUpdate(aAction: TBasicAction; var Handled: Boolean);
    procedure miNewsClick(Sender: TObject);
    procedure miRackHeaderClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure miStateClick(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure pbInfoMouseEnter(Sender: TObject);
    procedure pbInfoPaint(Sender: TObject);
    procedure pbPlayerPaint(Sender: TObject);
    procedure pbPointsPaint(Sender: TObject);
    procedure pbStatusMouseEnter(Sender: TObject);
    procedure pbStatusPaint(Sender: TObject);
    procedure pcMessagesPageChanged(Sender: TObject);
    procedure pmDimension3DPopup(Sender: TObject);
    procedure pmGameServerPopup(Sender: TObject);
    procedure pmStatusInfoClose(Sender: TObject);
    procedure pn2DResize(Sender: TObject);
    procedure pnPiecesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormIdle(Sender:Tobject;var Done:boolean);
    procedure seBestMoveChange(Sender: TObject);
    procedure seBestMoveKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure seBestMoveKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tbCubeResetClick(Sender: TObject);
    procedure tbDimensionClick(Sender: TObject);
    procedure tbCubeSettingClick(Sender: TObject);
    procedure tiMainTimer(Sender: TObject);
    procedure trPositionChange(Sender: TObject);
    procedure tvGameServerAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure tvGameServerGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure tvGameServerShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure acGameAnalysisExecute(Sender: TObject);
    procedure acBestMoveExecute(Sender: TObject);
    procedure udBestMoveChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
  private
    PieceList      : TPieceList;
    ScrabbleGrid   : TScrabbleGrid;
    ScrabbleCube   : TScrabbleCube;
    fm3D           : TForm;
    TCPClientIsWorking: boolean;
    IsGameServer, Pinged, MsgConnectionLostDone   : boolean;
    LastMessage    : longword;
    sbPoints: word;
    sbWho: byte;
    LetterSize,NumberSize: byte;
    SquareSize: integer;
    ChatHistory    : TTaggedStringList;
    Initialized    : boolean;
    LastAction     : TDateTime;
    LastClientState : TClientState;
    procedure AddTime(const aSeconds:word);
    procedure Initialize;
    procedure ScrabblePlaySound(aMsg:TScrabbleMessageType;async:boolean=true);
    procedure DoUserInput(Sender: TObject; Msg: Cardinal);
    procedure DoLanguageChanged(Sender: TObject);
    procedure DoConfigChange(Sender: TObject);
    procedure DoDictionaryChange(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    //messages
    procedure DoMessage(aMsgType:TScrabbleMessageType; aMsg: string; aSender:string='');
    procedure DoGetMessageFont(aMessageType: TScrabbleMessageType; aSender: string; out aBackgroundColor: TColor; out aFont: TFont);
    function DoGetPopupSetting(aMessageType: TScrabbleMessageType): boolean;
    procedure DoSetPopupSetting(aMessageType: TScrabbleMessageType; aValue: boolean);
    //view
    function DoGetBonusText(const x, y, z: byte): string;
    function DoGetHint(const x, y, z: byte): string;
    procedure DoRepaint(aSender: TScrabbleEvent);
    function DoSetCurrentMove(aIndex: word):word;
    procedure DoUpdatePoints(Who: byte; Value: word);
    procedure DoResizeStatusbar(var Msg: TLMessage); message LM_RESIZESTATUSBAR;
    //game course
    procedure DoProgress(Sender:TObject; const Value:byte);
    procedure UpdatePieceList;
    procedure UpdateGameServer;
    procedure UpdateScore;
    function DoGetLetterValue(const aChar: widechar): byte;
    procedure DoMoveNumberChange(Sender: TObject);
    //scrabble
    function LoadFrom(aFileName:string):boolean;
    procedure SaveTo(aFileName:string);
    procedure DoNewGame(out aPlayerNames: string; out aRackSize: byte;
                        out aBoardSize: byte; out aDimension: TScrabbleDimension;
                        out aFieldTypeArray: TFieldTypeArray; out aRandSeed: LongWord;
                        out aLetters: TLetterList; out aCanJokerExchange : boolean;
                        out aGameEndBonus: word; out aNumberOfPasses : byte;
                        out aJokerPenalty : byte; out aChangeIsPass:boolean;
                        out aTimeControl:TTimeControlSetting; out aTimeControlValue : Longword;
                        out aLimitedExchange:byte;
                        out aCambioSecco : boolean;out aWhatif : boolean;
                        out aAdd:boolean; out aSubstract: boolean;
                        out aTimePenaltyValue : byte; out aTimePenaltyCount : byte; out aTimeGameLost : boolean;
                        out aWordCheckMode : TWordCheckMode; out aWordCheckPeriod:byte;
                        out aWordCheckPenalty : byte; out aWordCheckBonus  : byte;
                        out aScrabbleBonus:byte; out aCLABBERS : boolean);
    procedure DoAfterNewGame(Sender: TObject);
    procedure DoAfterNextPlayer(Sender: TObject);
    procedure DoGameEnd(Sender: TObject);
    procedure DoTimeOut;
    procedure DoChallenge(aIsValid:boolean; aInvalid:string; aWho: byte);
    procedure UpdateMove;
    function DoCheckWords(const aValue: string; var aNotFound: string;DoAsk:boolean=false): boolean;
    function DoAskForJoker:WideChar;
    procedure DoJokerize(aLetter: TLetter);
    //network
    procedure SyncNewGame;
    procedure SyncGameEnd;
    procedure SyncNextPlayer(const aLastError,aOriginalLastError:TLastError);
    procedure UpdateKibitzMode;

    procedure DoSyncMessage(var Msg: TLMessage); message LM_SYNCMESSAGE;
    procedure DoTCPClientTerminate(Sender: TObject);
    procedure DoTCPClientReceive(aMsg:TNetworkMessage);
    procedure DoTCPClientSend(aMsgId:string;aReceiver,aMsg:string);
    //computer
    procedure DoAbortCalculation(Sender: TObject);
    function GetBruteForceSettings(const MaxPerformance:boolean):TBruteForceSettings;
    procedure ComputeMove;
    //scrabblecube
    function DoGetFieldLetter(x, y, z: byte): TLetter;
    function DoGetFieldColor(x, y, z: byte; aLetter:TLetter; var a: single; var FieldType:byte): TColor;
    procedure DoDrawLetter(aLetter: TLetter; aCanvas: TCanvas);
    procedure DoFormClose(Sender: TObject; var CloseAction: TCloseAction);
    //pieces
    procedure DoPieceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    ScrabbleMessages : TScrabbleMessages;
    GameCourse       : TGameCourse;
  end;

var
  fmMain: TfmMain;

implementation

uses
  uconfig, uversion;

{ TfmMain }

procedure TfmMain.FormShow(Sender: TObject);
 procedure FixControlStyles(Parent: TControl);  //show DragImage
 var
   i: Integer;
 begin
   Parent.ControlStyle:=Parent.ControlStyle+[csDisplayDragImage];
   if (Parent is TWinControl) then
    with TWinControl(Parent) do
     for i:=0 to ControlCount-1 do
      FixControlStyles(Controls[i]);
 end;

begin
  Application.OnException:=@DoException;
  Scrabble:=TGame.Create;
  with Scrabble do
  begin
    OnRepaint:=@DoRepaint;
    OnMoveNumberChange:=@DoMoveNumberChange;
    OnNewGame:=@DoNewGame;
    OnAfterNewGame:=@DoAfterNewGame;
    OnAfterNextPlayer:=@DoAfterNextPlayer;
    OnGameEnd:=@DoGameEnd;
    OnIsKnownWord:=@DoCheckWords;
    OnGetLetterValue:=@DoGetLetterValue;
    GameState:=[gsLoading];
  end;
  Dictionary:=TDictionary.Create;
  BruteForce:=TBruteForce.Create;

  Config.ReadWindowPosition(self);
  pnRight.Width:=Config.Read('General/Position/pnRight/Width',pnRight.Width);
  pn3D.Height:=Config.Read('General/Position/pn3D/Height',pn3D.Height);
  pmRack.Items[0].Checked:=Config.Read('General/Position/pnRack/HeaderVisible',pnPiecesHeader.Visible);
  miRackHeaderClick(self);
  //messages
  ScrabbleMessages:=TScrabbleMessages.Create(tsMessages);
  ScrabbleMessages.OnGetPopupSetting:=@DoGetPopupSetting;
  ScrabbleMessages.OnSetPopupSetting:=@DoSetPopupSetting;
  ScrabbleMessages.OnGetMessageFont:=@DoGetMessageFont;
  OnMessage:=@DoMessage;
  //game course
  GameCourse:=TGameCourse.Create(tsGameCourse);
  GameCourse.OnSetCurrentMove:=@DoSetCurrentMove;
  GameCourse.OnReplaceDigraphs:=@Dictionary.ReplaceDigraphs;
  GameCourse.OnIsKnownWord:=@DoCheckWords;
  Scrabble.OnSetHistory:=@GameCourse.DoAddMove;
  //computer
  BruteForce.OnGetRackLetter:=@GameCourse.DoGetRackLetterIndex;
  //3D
  ScrabbleCube:=TScrabbleCube.Create(pn3D);
  ScrabbleCube.OnGetFieldColor:=@DoGetFieldColor;
  ScrabbleCube.OnGetFieldLetter:=@DoGetFieldLetter;
  ScrabbleCube.OnDrawLetter:=@DoDrawLetter;
  //pieces
  PieceList:=TPieceList.Create(pnPieces);

  FixControlStyles(self);       //csDisplayDragImage for each control

  Application.HintPause:=1000;
  Application.HintHidePause:=5000;
  ChatHistory:=TTaggedStringList.Create;
  ChatHistory.Duplicates:=dupIgnore;
  ChatHistory.CaseSensitive:=true;
  Initialized:=false;

  //progress
  OnProgress:=@DoProgress;
  OnUpdatePoints:=@DoUpdatePoints;
  Application.OnUserInput:=@DoUserInput;  //for afk-setting
  Application.OnIdle:=@FormIdle;
  //any config error?
  {$if Defined(Portable) and Defined(Darwin)} //warning if app is not extracted from dmg
   if not DirectoryIsWritable(Config.Path) then
   begin
     raise EInOutError.Create('App directory is not writable! Please extract first.');
   end;
  {$ifend}
  if Config.LastError<>'' then
    OnMessage(smError,Config.LastError);
end;

procedure TfmMain.FormIdle(Sender: Tobject; var Done: boolean);
{$ifdef Debug}
 {$define ShowFPS}
{$endif}
begin
  if not Initialized then
    Initialize;

  {$if (defined(ShowFPS) and defined(UseOpenGl))}
  {$warning ShowFPS}
  if not Application.Terminated and
     assigned(Scrabble) and
     not (gsDestroying in Scrabble.GameState) then
  begin
    if assigned(ScrabbleCube) and assigned(pn3D) and pn3D.Visible then
    begin
      ScrabbleCube.Invalidate;
      pn3DHeader.Caption:=inttostr(ScrabbleCube.Fps)+'fps';
    end;
    if assigned(pn2D) and pn2D.Visible then
    begin
      if ScrabbleGrid is TGlScrabbleGrid then
        ScrabbleGrid.Paint(self);
      pn2DHeader.Caption:=inttostr(ScrabbleGrid.Fps)+'fps';//, cell: '+inttostr(SelectedCell.X)+','+inttostr(SelectedCell.Y);
    end;//assigned
  end;
  {$endif}

  if IsGameServer then
  begin
     if (TCPClient.TimeToAfk>0) and
        (MilliSecondsBetween(Now, LastAction)>TCPClient.TimeToAfk) and
        (TCPClient.PlayerData.ClientState in [csOpenForGames,csNotOpen]) then
     begin
       LastClientState:=TCPClient.PlayerData.ClientState;
       TCPClient.PlayerData.ClientState:=csAfk;
       TCPClient.OnSend('nwRefresh','all','SetState='+ClientStateToString(csAfk))
     end;
     if (GetTickCount-LastMessage>30000) and not Pinged then
     begin
       Pinged:=true;
       TCPClient.OnSend('nwPing',TCPClient.PlayerData.PlayerName,'');
       MsgConnectionLostDone:=false;
     end;
     if (GetTickCount-LastMessage>65000) and not MsgConnectionLostDone then
     begin
       MsgConnectionLostDone:=true;
       OnMessage(smError,rMain_NetworkDown);
     end;
     if (GetTickCount-LastMessage>1000) then
       acNetwork.ImageIndex:=2;
  end;
end;

procedure TfmMain.Initialize;
const
  UpdateInterval:array[-1..4] of integer=(-1,-1,1,7,30,MaxInt);
begin
  Initialized:=true;
  //update
  if DaysBetween(Updater.LastUpdate,Now)>UpdateInterval[fmGameOptions.cbUpdates.ItemIndex] then
    miUpdateClick(self);
  //dictionary
  Dictionary.OnChange:=@DoDictionaryChange;
  Dictionary.OnGetAvailableLetters:=@fmGameOptions.AvailableLetters;
  Dictionary.OnAfterDictionaryLoaded:=@fmGameOptions.DoAfterDictionaryLoaded;

  //load last settings
  fmGameOptions.OnConfigChange:=@DoConfigChange;
  fmGameOptions.OnLanguageChanged:=@DoLanguageChanged;
  fmGameOptions.Configuration:=Config.Read('General/LastPreset','');
  DoConfigChange(self);
  DoLanguageChanged(self);

  tsGameServer.TabVisible:=false;
  Config.Write('General/Demo/Save',Config.Read('General/Demo/Save',false));
  GameCourse.AdjustColWidth;
  Scrabble.GameState:=[];
  OnMessage(smInformation,rMain_InfoAppStarted);

  //welcome assistant
  if not Config.Read('General/Welcome/Done',false) then
  begin
    fmWelcome.ShowModal;
    Config.Write('General/Welcome/Done',true);
  end;
  ScrabbleGrid.Invalidate;
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i:longword;
begin
  Scrabble.GameState:=Scrabble.GameState+[gsDestroying];
  Application.OnIdle:=nil;
  OnMessage:=nil;
  tiMain.Enabled:=false; //timer off
  if (gsNetwork in Scrabble.GameState) then
  begin
    if assigned(TCPServer) then TCPServer.Terminate;
    if assigned(TCPClient) then TCPClient.Terminate;
    i:=GetTickCount;
    while (gsNetwork in Scrabble.GameState) and (GetTickCount-i<5000) do
      Application.ProcessMessages;
  end;
  if (gsComputing in Scrabble.GameState) then Bruteforce.Aborted:=true;
  if Config<>nil then
  begin
    Config.SaveWindowPosition(self);
    Config.SaveWindowPosition(fmRemote);
    Config.SaveWindowPosition(fmWordSearch);
    Config.SaveWindowPosition(fmNewGame);
    Config.Write('General/Position/pnRight/Width',pnRight.Width);
    Config.Write('General/Position/pn3D/Height',pn3D.Height);
  end;
  PieceList.Free;
  ScrabbleCube.Free;
  ScrabbleGrid.Free;
  ScrabbleGrid:=nil;
  GameCourse.Free;
  BruteForce.Free;
  Dictionary.Free;
  with Scrabble do
  begin
    OnRepaint:=nil;
    OnMoveNumberChange:=nil;
    OnNewGame:=nil;
    OnAfterNewGame:=nil;
    OnAfterNextPlayer:=nil;
    OnGameEnd:=nil;
    OnIsKnownWord:=nil;
    OnGetLetterValue:=nil;
  end;
  Scrabble.Free;
  ChatHistory.Free;
  ScrabbleMessages.Free;

  CloseAction:=caFree;
end;

procedure TfmMain.alMainUpdate(aAction: TBasicAction; var Handled: Boolean);
begin
  if not Application.Terminated and
     assigned(Scrabble) and
     not (gsDestroying in Scrabble.GameState) then
  begin
    acExit.Enabled:=(Scrabble.GameState*[gsComputing,gsLoading,gsDestroying,gsDialog]=[]) and
                     not (gsNextPlayer in Scrabble.GameState) and
                     not (gsPaused in Scrabble.GameState) and
                    (not (gsDemo in Scrabble.GameState));
    acLoad.Enabled:=acExit.Enabled and
                    (not (gsNetwork in Scrabble.GameState) or IsGameServer) and
                    not (gsKibitz in Scrabble.GameState);
    acSave.Enabled:=acExit.Enabled and (gsActive in Scrabble.GameState) and (not (gsNetwork in Scrabble.GameState) or (gsGameEnd in Scrabble.GameState));
    acScreenShot.Enabled:=acExit.Enabled and (gsActive in Scrabble.GameState);
    acNewGame.Enabled:=acExit.Enabled and
                       (not (gsNetwork in Scrabble.GameState) or not (gsKibitz in Scrabble.GameState));
    acSettings.Enabled:=acExit.Enabled;
    acNetwork.Enabled:=acExit.Enabled;
    miAssistant.Enabled:=acExit.Enabled;
    acKibitz.Enabled:=(gsNetwork in Scrabble.GameState) and
                       IsGameServer and
                       not (gsDialog in Scrabble.GameState) and
                       not (gsKibitz in Scrabble.GameState);
    acHighscore.Enabled:=not (gsDialog in Scrabble.GameState);

    acChallenge.Enabled:=Scrabble.CanChallenge and not (gsDialog in Scrabble.GameState);

    tbNetwork.Down:=(gsNetwork in Scrabble.GameState) and not (gsDialog in Scrabble.GameState);
    pnChat.Visible:=(gsNetwork in Scrabble.GameState) and not (gsDialog in Scrabble.GameState);
    acRestore.Enabled:=(gsActive in Scrabble.GameState) and
                        not (gsNextPlayer in Scrabble.GameState) and
                        not (gsGameEnd in Scrabble.GameState) and
                        not (gsComputing in Scrabble.GameState) and
                        (Scrabble.LocalPlayer=Scrabble.CurrentPlayer) and
                        not (Scrabble.MoveState=msJokerExchanged) and
                        not (Scrabble.MoveState=msLetterExchange);
    if acRestore.Enabled then
     if Scrabble.MoveState=msLetterPlaced then
      acRestore.ImageIndex:=14 else acRestore.ImageIndex:=13;

    acNextPlayer.Enabled:=(Scrabble.GameState*[gsComputing,gsGameEnd,gsKibitz,gsDialog,gsNextPlayer]=[]) and
                          (not (gsDemo in Scrabble.GameState)) and
                          (gsRunning in Scrabble.GameState) and
                          not (gsPaused in Scrabble.GameState) and
                          (Scrabble.ActualMove=Scrabble.CurrentMove) and
                          (Scrabble.LocalPlayer=Scrabble.CurrentPlayer);
    if (gsNetwork in Scrabble.GameState) and IsGameServer then
      acNextPlayer.Enabled:=acNextPlayer.Enabled and (Scrabble.Player[Scrabble.LocalPlayer].Name=TCPClient.PlayerData.PlayerName);

    acBestMove.Enabled:=(Scrabble.BoardSize>0) and
                        (not (gsDemo in Scrabble.GameState)) and
                        (not (gsNetwork in Scrabble.GameState) or (gsGameEnd in Scrabble.GameState)) and
                        (not (gsGameEnd in Scrabble.GameState) or (Scrabble.ActualMove<Scrabble.CurrentMove)) and
                         not (gsNextPlayer in Scrabble.GameState) and
                         not (gsDialog in Scrabble.GameState) and
                         not (gsPaused in Scrabble.GameState) and
                        (Dictionary.Info[itWordCount]>0);
    acCambioSecco.Enabled:=acNextPlayer.Enabled and
                           Scrabble.Player[Scrabble.LocalPlayer].CambioSecco and
                           (Scrabble.NumberOfLettersLeft>0);
    acGameAnalysis.Enabled:=not (gsDemo in Scrabble.GameState) and
                            not (gsDialog in Scrabble.GameState) and
                            (gsActive in Scrabble.GameState) and
                            (gsGameEnd in Scrabble.GameState) and
                            (Dictionary.Info[itWordCount]>0);
    acDemo.Enabled:=not (gsLoading in Scrabble.GameState) and
                    not (gsNetwork in Scrabble.GameState) and
                    not (gsDialog in Scrabble.GameState) and
                    (not (gsComputing in Scrabble.GameState) or (gsDemo in Scrabble.GameState)) and
                    (Dictionary.Info[itWordCount]>0);
    acWordSearch.Enabled:=not (gsLoading in Scrabble.GameState) and
                          not (gsDialog in Scrabble.GameState);
    seBestMove.Enabled:=(BruteForce.BestMovesCount>0) and
                         not (gsDialog in Scrabble.GameState) and
                         not (gsDemo in Scrabble.GameState);
    if (gsPaused in Scrabble.GameState) then
      acPause.ImageIndex:=16 else
      acPause.ImageIndex:=15;
    acPause.Enabled:=not (gsDialog in Scrabble.GameState) and
                     not (gsGameEnd in Scrabble.GameState) and
                     not (gsNextPlayer in Scrabble.GameState) and
                     (not (gsNetwork in Scrabble.GameState) or not (gsKibitz in Scrabble.GameState));
    lbTimer.Enabled:=(Scrabble.TimeControlSetting<>tcNoLimit) and
                     not (gsGameEnd in Scrabble.GameState);
    acShufflePieces.Enabled:=not (gsDemo in Scrabble.GameState) and
                             not (gsLoading in Scrabble.GameState);
    if gsKibitz in Scrabble.GameState then
    begin
      pnPiecesHeader.Color:=clMaroon;
      if gsActive in Scrabble.GameState then
          pnPiecesHeader.Caption:=Scrabble.Player[Scrabble.CurrentPlayer].Name;
    end else
    begin
      if acNextPlayer.Enabled then
        pnPiecesHeader.Color:=clGreen else
        pnPiecesHeader.Color:=clGray;
      if gsActive in Scrabble.GameState then
      begin
        if (Scrabble.ActualMove<>Scrabble.CurrentMove) then
          pnPiecesHeader.Caption:=Scrabble.Player[Scrabble.LocalPlayer].Name+' ('+Scrabble.Player[Scrabble.ActualMove mod Scrabble.NumberOfPlayers].Name+')' else
          pnPiecesHeader.Caption:=Scrabble.Player[Scrabble.LocalPlayer].Name;
      end;
    end;
  end;
  Handled:=true;
end;

{$I mn_gm.inc} //game
{$I mn_nw.inc} //network
{$I mn_ti.inc} //timer

{$I mn_st.inc} //statusbar
{$I mn_si.inc} //simple actions
{$I mn_cp.inc} //computer

{$I mn_ms.inc} //messages
{$I mn_gs.inc} //game server
{$I mn_cb.inc} //scrabble cube

initialization

 {$I umain.lrs}
 {$I flags.lrs}

 {$ifdef Windows}
  {$I Sounds.res}
 {$endif}

end.
