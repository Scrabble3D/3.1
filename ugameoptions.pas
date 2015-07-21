{ Game options

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

unit ugameoptions;

{$mode objfpc}{$H+}

interface

{$I conditions.inc}

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Spin, Grids, Menus, Buttons,
  LCLType, LCLIntf, LCLProc, MaskEdit, Clipbrd, FileUtil, ActnList,
  ColorBox, ExtDlgs, ValEdit,
  uboard, uupdater, uconfig, zipper, types;

type
  TConfigOptions=(coInfoDictionary,coInfoUsedCategories,coInfoBoard,coInfoLetterset,coInfoPieces,
     coInfoJokers,coInfoRandoms,coInfoTime,coInfoWordCheck,coInfoTimePanalty,coCambioSecco,coTournament);

  TKeyValuePair = record //borrowed from ValEdit
    Key, Value: String;
  end;

  TLettersetType=(lsLetter=0,lsCount=1,lsValue=2);

  { TfmGameOptions }
  TfmGameOptions = class(TForm)
    acSavePreset: TAction;
    acDeletePreset: TAction;
    acLoadDictionary: TAction;
    acDeleteDictionary: TAction;
    acLoadRules: TAction;
    acChangeBoardcolor: TAction;
    acDeleteLang: TAction;
    acDeleteDesign: TAction;
    alConfiguration: TActionList;
    acLoadPreset: TAction;
    btnLocalization: TBitBtn;
    btnChangeBoardcolor: TBitBtn;
    btnDictionaryLoad: TBitBtn;
    btnDesignLoad: TBitBtn;
    btnOptionsClose: TBitBtn;
    btnFont: TButton;
    btnOpenConfigDir: TButton;
    btnUpdate: TButton;
    btnMovieProgram: TButton;
    cbLostByTime: TCheckBox;
    cbProxy: TCheckBox;
    cbSubstractLettersLeft: TCheckBox;
    cbBackGroundColor: TColorButton;
    cbBoard: TComboBox;
    cbShowBonusText: TCheckBox;
    cbCanJokerExchange: TCheckBox;
    cbChangeIsPass: TCheckBox;
    cbCambioSecco: TCheckBox;
    cbChat: TCheckBox;
    cbCompDeterministic: TCheckBox;
    cbCompleteExchange: TCheckBox;
    cbDebug: TCheckBox;
    cbError: TCheckBox;
    cbFixate: TCheckBox;
    cbGameResult: TCheckBox;
    cbInformation: TCheckBox;
    cbLatest: TCheckBox;
    cbLetterSet: TComboBox;
    cbOwnMove: TCheckBox;
    cbParallel: TCheckBox;
    cbPlayer1: TColorButton;
    cbPlayer2: TColorButton;
    cbPlayer3: TColorButton;
    cbPlayer4: TColorButton;
    cbPlayer5: TColorButton;
    cbPlayer6: TColorButton;
    cbPlayer7: TColorButton;
    cbPlayer8: TColorButton;
    cbPlayerColored: TCheckBox;
    cbRoman: TCheckBox;
    cbShaded: TCheckBox;
    cbShowBonusMarkers: TCheckBox;
    cbShowComputer: TCheckBox;
    cbUpdates: TComboBox;
    cbWarnings: TCheckBox;
    cbAddLettersLeft: TCheckBox;
    cbToolbar3D: TCheckBox;
    cbShowScore: TCheckBox;
    cbBoardTransparent: TCheckBox;
    cbFlat: TCheckBox;
    cbSticky: TCheckBox;
    cbJokerization: TCheckBox;
    cbCLABBERS: TCheckBox;
    cbMipmapping: TCheckBox;
    cbReflections: TCheckBox;
    cbStatusbar: TCheckBox;
    clBoardColors: TColorListBox;
    cbPresets: TComboBox;
    cbRackPos: TComboBox;
    dgFieldType: TDrawGrid;
    dgOptionsNavigator: TDrawGrid;
    edConfigDir: TEdit;
    edFont: TEdit;
    edMovieProgram: TEdit;
    edMovieOptions: TEdit;
    edProxyIP: TEdit;
    edProxyPass: TEdit;
    edProxyPort: TEdit;
    edProxyUser: TEdit;
    edSoundCommand: TEdit;
    gb2DLight: TGroupBox;
    gb2DPerspective: TGroupBox;
    gb2DImages: TGroupBox;
    gbCategories: TCheckGroup;
    gbTimeControl: TGroupBox;
    gbTimePenalty: TGroupBox;
    gbWordCheckMode: TGroupBox;
    gbRules: TGroupBox;
    gbLetter: TGroupBox;
    gbLetterOptions: TGroupBox;
    gbRulesOptions: TGroupBox;
    gbBoard: TGroupBox;
    gbBoardConfig: TGroupBox;
    gbPresets: TGroupBox;
    gbDictionary: TGroupBox;
    gbDesign: TGroupBox;
    gbDesignOptions: TGroupBox;
    gbBoardColors: TGroupBox;
    gbPlayerColors: TGroupBox;
    gbComputer: TGroupBox;
    gbCompOpt: TGroupBox;
    gbLocalization: TGroupBox;
    gbMessages: TGroupBox;
    gbNetwork: TGroupBox;
    gbLeftover: TGroupBox;
    il48: TImageList;
    il96: TImageList;
    ilFieldType: TImageList;
    imBoard: TImage;
    imJoker: TImage;
    imRandom: TImage;
    lbRackPos: TLabel;
    lbMovie: TLabel;
    sgLocalization: TStringGrid;
    sgDictionary: TStringGrid;
    sgDesign: TStringGrid;
    sgRules: TStringGrid;
    spDictionary: TSplitter;
    spDesign: TSplitter;
    spLocalization: TSplitter;
    stActivePlaneValue: TStaticText;
    stCompTrustValue: TStaticText;
    lbCompTrust: TLabel;
    lbLetterSet: TLabel;
    lbTranslation: TLabel;
    lbRulesSet: TLabel;
    lbScrabbleBonus: TLabel;
    lbPlane: TLabel;
    lbBoardPreview: TLabel;
    lbPresetsLabel: TLabel;
    lbRandom: TLabel;
    lbJoker: TLabel;
    lb2DAmbient: TLabel;
    lbLightAmbient: TLabel;
    lbLightDiffuse: TLabel;
    lbPitch: TLabel;
    lbChallengeBonus: TLabel;
    lbChallengePenalty: TLabel;
    lbChallengePeriod: TLabel;
    stLuminanceValue: TStaticText;
    lbActivePlaneValue: TLabel;
    lbNumberOfLetters: TLabel;
    lbNumberOfRandomLetters: TLabel;
    lbDistance: TLabel;
    lbReadingDirection: TLabel;
    lbTimePenaltyCount: TLabel;
    lbTimePenaltyCountLabel: TLabel;
    lbTimePenaltyValue: TLabel;
    lbProxyIP: TLabel;
    lbProxyPass: TLabel;
    lbProxyPort: TLabel;
    lbProxyUser: TLabel;
    lbTotalNumberOfLetters: TLabel;
    lbUpdate: TLabel;
    cbRulesSet: TComboBox;
    lvDictionaries: TListView;
    lvLocalizations: TListView;
    lvDesigns: TListView;
    meTimeLimitPerGame: TMaskEdit;
    meTimeLimitPerMove: TMaskEdit;
    miTripleLetterMalus: TMenuItem;
    miQuadLetterMalus: TMenuItem;
    miDoubleLetterMalus: TMenuItem;
    miSingleLetterMalus: TMenuItem;
    ilGlyphs: TImageList;
    lbLimitExchange: TLabel;
    lbCompCambioSecco: TLabel;
    lbBackgroundColor: TLabel;
    lbBoardPreset: TLabel;
    lbBoardSize: TLabel;
    lbChangeLength: TLabel;
    lbChangeValue: TLabel;
    lbCompChange: TLabel;
    lbCompChangeLetter: TLabel;
    lbCompChangeValue: TLabel;
    lbCompExchange: TLabel;
    lbCompExchangeValue: TLabel;
    lbCompCambioSeccoValue: TLabel;
    lbCompPerformance: TLabel;
    stCompPerformanceValue: TStaticText;
    lbCompUseJoker: TLabel;
    lbCompUseJokerValue: TLabel;
    lbConfigFile: TLabel;
    lbDimension: TLabel;
    lbFontName: TLabel;
    lbGameEndBonus: TLabel;
    lbJokerPenalty: TLabel;
    lbLetterSize: TLabel;
    lbLuminance: TLabel;
    lbNumberOfJokers: TLabel;
    lbNumberSize: TLabel;
    lbPass: TLabel;
    lbSoundCommand: TLabel;
    miStart: TMenuItem;
    miNewLetter: TMenuItem;
    miLetter: TMenuItem;
    miQuadWord: TMenuItem;
    miTripleWord: TMenuItem;
    miDoubleWord: TMenuItem;
    miQuadLetter: TMenuItem;
    miTripleLetter: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miDoubleLetter: TMenuItem;
    miNormal: TMenuItem;
    pnJoker: TPanel;
    pnRandom: TPanel;
    pnOptionsLeft: TPanel;
    pcOptions: TPageControl;
    pnBottom: TPanel;
    pmFieldType: TPopupMenu;
    pmCopyPaste: TPopupMenu;
    pnChat: TPanel;
    pnError: TPanel;
    pnGameResult: TPanel;
    pnInformation: TPanel;
    pnOwnMove: TPanel;
    pnScrabble: TPanel;
    pnWarnings: TPanel;
    rg2DMode: TRadioGroup;
    rgOptionsNavigator: TRadioGroup;
    rbWCMTakeback: TRadioButton;
    rbWCMPoll: TRadioButton;
    rbWCMChallenge: TRadioButton;
    rbLeftToRight: TRadioButton;
    rbNoLimit: TRadioButton;
    rbPerGame: TRadioButton;
    rbPerMove: TRadioButton;
    rbDim2D: TRadioButton;
    rbDim3D: TRadioButton;
    rbRightToLeft: TRadioButton;
    rbTimePenaltyBuyTime: TRadioButton;
    rbTimePenaltyEndGame: TRadioButton;
    sbChat: TSpeedButton;
    sbDeleteLang: TSpeedButton;
    sbDeleteDictionary: TSpeedButton;
    sbDeleteDesign: TSpeedButton;
    sbGameResult: TSpeedButton;
    sbOwnMove: TSpeedButton;
    sbSave: TSpeedButton;
    sbScrabble: TSpeedButton;
    seBoardSize: TSpinEdit;
    seChallengeBonus: TSpinEdit;
    seChallengePenalty: TSpinEdit;
    seChallengePeriod: TSpinEdit;
    seGameEndBonus: TSpinEdit;
    seLimitExchange: TSpinEdit;
    seJokerPenalty: TSpinEdit;
    seLetterSize: TSpinEdit;
    seNumberOfJokers: TSpinEdit;
    seNumberOfLetters: TSpinEdit;
    seNumberOfPasses: TSpinEdit;
    seNumberOfRandomLetters: TSpinEdit;
    seNumberSize: TSpinEdit;
    seTimePenaltyCount: TSpinEdit;
    seTimePenaltyValue: TSpinEdit;
    seLettersetMultiplier: TSpinEdit;
    sgLetters: TStringGrid;
    sbDelete: TSpeedButton;
    seScrabbleBonus: TSpinEdit;
    spOptions: TSplitter;
    sgPreset: TStringGrid;
    stTotalLetterCount: TStaticText;
    tbLightAmbient: TTrackBar;
    tbLightDiffuse: TTrackBar;
    tbLightPosX: TTrackBar;
    tbLightPosY: TTrackBar;
    tbLightPosZ: TTrackBar;
    tbTranslation: TTrackBar;
    tbCompTrust: TTrackBar;
    tsExtra: TTabSheet;
    tbPitch: TTrackBar;
    tbDistance: TTrackBar;
    tsPlayerColor: TTabSheet;
    tsCategories: TTabSheet;
    tsGame: TTabSheet;
    tsMessages: TTabSheet;
    tsNetwork: TTabSheet;
    tsCompOpt: TTabSheet;
    ts2DView: TTabSheet;
    tsBoardColor: TTabSheet;
    tsDesign: TTabSheet;
    tsBoardConfig: TTabSheet;
    tsTime: TTabSheet;
    tsRules: TTabSheet;
    tsWordCheckMode: TTabSheet;
    tvOptionsNavigator: TTreeView;
    tsLetter: TTabSheet;
    tbActivePlane: TTrackBar;
    tbCompChangeLetters: TTrackBar;
    tbCompChangeValue: TTrackBar;
    tbCompExchange: TTrackBar;
    tbCompPerformance: TTrackBar;
    tbCompUseJoker: TTrackBar;
    tbLuminance: TTrackBar;
    tbCompCambioSecco: TTrackBar;
    tsBoard: TTabSheet;
    tsComputer: TTabSheet;
    tsDesignGeneral: TTabSheet;
    tsDictionary: TTabSheet;
    tsLocalization: TTabSheet;
    tsRulesOptions: TTabSheet;
    procedure acDeleteDesignExecute(Sender: TObject);
    procedure acDeleteLangExecute(Sender: TObject);
    procedure acDeletePresetExecute(Sender: TObject);
    procedure acDeleteDictionaryExecute(Sender: TObject);
    procedure acChangeBoardcolorExecute(Sender: TObject);
    procedure acLoadDictionaryExecute(Sender: TObject);
    procedure acLoadPresetExecute(Sender: TObject);
    procedure acLoadRulesExecute(Sender: TObject);
    procedure acSavePresetExecute(Sender: TObject);
    procedure alConfigurationUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure btnDesignLoadClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnLocalizationClick(Sender: TObject);
    procedure btnMovieProgramClick(Sender: TObject);
    procedure btnOpenConfigDirClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure cbAddSubstractChange(Sender: TObject);
    procedure cbBoardChange(Sender: TObject);
    procedure cbCambioSeccoChange(Sender: TObject);
    procedure cbCanJokerExchangeChange(Sender: TObject);
    procedure cbCLABBERSChange(Sender: TObject);
    procedure cbPlayerColoredChange(Sender: TObject);
    procedure cbPresetsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbPresetsSelect(Sender: TObject);
    procedure cbRulesSetChange(Sender: TObject);
    procedure clBoardColorsDblClick(Sender: TObject);
    procedure clBoardColorsGetColors(Sender: TCustomColorListBox; Items: TStrings);
    procedure ColorChanged(Sender: TObject);
    procedure cbProxyChange(Sender: TObject);
    procedure dgFieldTypeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbLetterSetChange(Sender: TObject);
    procedure dgFieldTypeDrawCell(Sender: TObject; aCol, aRow: Integer;aRect: TRect; aState: TGridDrawState);
    procedure dgFieldTypeMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure dgOptionsNavigatorDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure dgOptionsNavigatorSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure edFontButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageClick(Sender: TObject);
    procedure imBoardPaint(Sender: TObject);
    procedure kvDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure lbCompLinkClick(Sender: TObject);
    procedure lvConfigurationCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvConfigurationSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvDictionariesDblClick(Sender: TObject);
    procedure pnOptionsLeftResize(Sender: TObject);
    procedure rg2DModeSelectionChanged(Sender: TObject);
    procedure rgOptionsNavigatorClick(Sender: TObject);
    procedure sgSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure sgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure sgDrawCells(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgResize(Sender: TObject);
    procedure VisualChange(Sender: TObject);
    procedure tsBoardShow(Sender: TObject);
    procedure tsBoardLayoutResize(Sender: TObject);
    procedure gbCategoriesItemClick(Sender: TObject; Index: integer);
    procedure rbTimeControlChange(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miFieldTypeClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure rbDimClick(Sender: TObject);
    procedure rbTimePenaltyChange(Sender: TObject);
    procedure sbSndClick(Sender: TObject);
    procedure seBoardSizeChange(Sender: TObject);
    procedure seLettersetMultiplierChange(Sender: TObject);
    procedure seNumberOfPassesEditingDone(Sender: TObject);
    procedure sgLettersResize(Sender: TObject);
    procedure sgLettersKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tbActivePlaneChange(Sender: TObject);
    procedure tbCompExchangeChange(Sender: TObject);
    procedure tbLuminanceChange(Sender: TObject);
    procedure tvOptionsNavigatorChange(Sender: TObject; Node: TTreeNode);
    procedure WCMUpdate(Sender: TObject);
    procedure RulesChanged(Sender: TObject);
  private
    FOnConfigChange    : TNotifyEvent; //notification if config data is changed (via PropertyChanged())
    FOnLanguageChanged : TNotifyEvent; //called from main when language was changed to reload strings
    FOnModelsChanged   : TNotifyEvent;
    FSettingsChanged   : boolean;      //true until save if something was changed
    FFieldType         : TFieldTypeArray; //defined in uboard
    FPoint             : TPoint;          //internally used to store mouse coordinates for popup
    FConfigName        : string;

    procedure SetLetterSet(const aValue: string);
    procedure SetLettersetString(aIndex: TLettersetType; const aValue: string);
    procedure SetRulesSet(const aValue: string);
    procedure SetBoardSet(const aValue: string);
    procedure SetFieldTypeArray(const AValue: TFieldTypeArray);
    procedure SetBoardString(const aValue: string);
    procedure SetConfiguration(const aValue: string);

    function GetRulesSet: string;
    function GetLetterSet: string;
    function GetBoardSet: string;
    function GetFieldColor(index: TFieldType): TColor;
    function GetPlayerColor(index: byte): TColor;
    function GetFieldTypeArray: TFieldTypeArray;
    function GetLettersetString(aIndex: TLettersetType): string;
    function GetLetterValue(aChar: widechar): byte;
    function GetBoardString: string;                      //string of fieldtypes like 001020100,010020010...
    function GetNumberOfLetters: word;                    //sum of count*letters at sgLetters[]

    procedure ResizeFieldTypeGrid;
    procedure ReloadRemoteFiles(Sender:TListView);

    procedure DoAfterDownload(const aItem:TUpdateItem);
    procedure LoadDictionary(const aFileName: string);
    procedure LoadLocalization(const aFileName: string);
    procedure LoadDesign(const aFileName: string);

    procedure LoadItems(const Section:string;aParent:TControl;aConfig:TConfig=nil);
  published
    procedure UpdateLanguage;
    procedure UpdateDictionaryInfo;
    procedure UpdatePresetInfo;
    procedure UpdateLettersetInfo(Sender: TObject);
    procedure DoAfterDictionaryLoaded(Sender: TObject);

    procedure LoadSettings(const aPreset:string);
    procedure SaveSettings(const aPreset:string);

    function AvailableLetters: string;
    property LetterSet:string read GetLetterSet write SetLetterSet; //deutsch|english etc.
    property RulesSet:string read GetRulesSet write SetRulesSet;
    property BoardSet:string read GetBoardSet write SetBoardSet;
  public
    function ConfigString(const aOptions:TConfigOptions):TKeyValuePair;
    property Configuration:string read FConfigName write SetConfiguration;

    property FieldColor[index:TFieldType]:TColor read GetFieldColor;
    property PlayerColor[index:byte]:TColor read GetPlayerColor;
    property FieldtypeArray:TFieldTypeArray read GetFieldTypeArray write SetFieldTypeArray ;

    property LettersetString[aIndex:TLettersetType]:string read GetLettersetString write SetLettersetString ;
    property LetterValue[aChar:widechar]:byte read GetLetterValue;
    property BoardString       : string read GetBoardString write SetBoardString;

    property NumberOfLetters   : word read GetNumberOfLetters;

    property OnConfigChange    : TNotifyEvent read FOnConfigChange write FOnConfigChange;
    property OnLanguageChanged : TNotifyEvent read FOnLanguageChanged write FOnLanguageChanged;
    property OnModelsChanged : TNotifyEvent read FOnModelsChanged write FOnModelsChanged;
  end;

const
  //cWW=#10;//Linux = #10, Windows = #13#10, MacOS = #13
  DefaultFieldColor:array[TFieldType] of TColor=(
    clGreen,
    clAqua,clBlue,$00600000,
    $008080FF,clRed, $00000060,
    clGray,$0080FFFF, clYellow,
    $00B4B4,$009696,$006464,$003232);
  DefaultFieldName:array[TFieldType] of string=(
    'normal field',
    'double letter','triple letter','quad letter',
    'double word','triple word','quad word',
    'start field','placed letter','new letter',
    'single letter malus','double letter malus','triple letter malus','quad letter malus');

  StandardOptions:array[0..9] of TConfigOptions=(coInfoDictionary,coInfoUsedCategories,coInfoBoard,coTournament,coInfoLetterset,coInfoPieces,
    coInfoJokers,coInfoRandoms,coInfoTime,coInfoWordCheck);

  cTS=#7;//cell text + cTS + <TabSheet>.Name
     {$I DefaultFieldType.inc}

var
  fmGameOptions: TfmGameOptions;

implementation

uses
  uscrabble, udictionary, utypes, ulanguage, udefaults;

{ TfmGameOptions }

procedure TfmGameOptions.FormCreate(Sender: TObject);
var
  i:integer;
begin
  inherited;
  edFont.Text:=GetFontData(Application.MainForm.Font.Reference.Handle).Name;

  seLetterSize.Value:=90;
  seNumberSize.Value:=40;
  //position
  Left:=Screen.Width div 2-Width div 2;
  Top:=Screen.Height div 2-Height div 2;
  Config.ReadWindowPosition(self);
  pnOptionsLeft.Width:=Config.Read('General/Position/spOptions/Left',pnOptionsLeft.Width);
  spDictionary.Top:=Config.Read('General/Position/spDictionary/Top',spDictionary.Top);
  spDesign.Top:=Config.Read('General/Position/spDesign/Top',spDesign.Top);
  spLocalization.Top:=Config.Read('General/Position/spLocalization/Top',spLocalization.Top);

  //on dbl clicking a preselected entry the property selected is nil
  lvDictionaries.Selected:=nil;
  //letter set
  cbLetterSet.Sorted:=false;
  for i:=integer(low(TDefaultsLanguage)) to integer(high(TDefaultsLanguage)) do
   with Defaults.Value[TDefaultsLanguage(i)] do
   begin
     cbLetterSet.Items.Add(English+' ('+Native+')');
     if RulesValid then
       cbRulesSet.Items.Add(English+' ('+Native+')');
   end;
  cbLetterSet.ItemIndex:=3;//british english
  cbLetterSetChange(nil);
  with TStringList.Create do
  try
    Add('500100050001005');
    Add('040002000200040');
    Add('004000101000400');
    Add('100400010004001');
    Add('000040000040000');
    Add('020002000200020');
    Add('001000101000100');
    Add('500100070001005');
    Add('001000101000100');
    Add('020002000200020');
    Add('000040000040000');
    Add('100400010004001');
    Add('004000101000400');
    Add('040002000200040');
    Add('500100050001005');
    BoardString:=DelimitedText;
    cbBoard.ItemIndex:=cbBoard.Items.IndexOf(BoardSet); //update label
    UpdatePresetInfo;
  finally
    Free;
  end;
  dgOptionsNavigator.FocusRectVisible:=false;
  dgOptionsNavigator.DoubleBuffered:=true;
  dgOptionsNavigator.DefaultRowHeight:=48+4+dgOptionsNavigator.Canvas.TextHeight('ABC');

  cbPresets.Items.DelimitedText:=Config.Read('Game/Presets','');

  ReloadRemoteFiles(lvDictionaries);
  ReloadRemoteFiles(lvLocalizations);
  ReloadRemoteFiles(lvDesigns);

  {$ifndef UseOpenGl}
  rg2DMode.ItemIndex:=1;
  rg2DMode.Enabled:=false;
  {$endif}
  edConfigDir.Text:=Config.Path;
  tbCompExchangeChange(nil);
  stTotalLetterCount.Caption:=inttostr(NumberOfLetters);
  {$ifdef Linux}
  if (edMovieProgram.Text='') and FileExists('/usr/bin/ffmpeg') then
    edMovieProgram.Text:='/usr/bin/ffmpeg';
  {$endif}

  Updater.OnUpdateItem:=@DoAfterDownload;
  //always have the first node selected
  tvOptionsNavigator.Selected:=tvOptionsNavigator.Items[0];
  FSettingsChanged:=false;
end;

procedure TfmGameOptions.FormDestroy(Sender: TObject);
var
  mi : TMenuItem;
begin
  setlength(FFieldType,0,0,0);
  while pmFieldType.Items.Count>0 do
  begin
    mi:=pmFieldType.Items[0];
    mi.Free;
    pmFieldType.Items.Delete(0);
  end;
  inherited;
end;

procedure TfmGameOptions.pnOptionsLeftResize(Sender: TObject);
begin
  dgOptionsNavigator.DefaultColWidth:=dgOptionsNavigator.ClientWidth;
end;

procedure TfmGameOptions.tvOptionsNavigatorChange(Sender: TObject; Node: TTreeNode);
var
  i:integer;
begin
  i:=Node.StateIndex;//Node.AbsoluteIndex
  if (i>-1) and (i<=pcOptions.PageCount) then
    pcOptions.ActivePage:=pcOptions.Pages[i];
end;

procedure TfmGameOptions.rgOptionsNavigatorClick(Sender: TObject);
const
  cSimpleNavigator:array[0..4] of string=(rOptions_NavigatorGame,rOptions_NavigatorDic,rOptions_NavigatorRules,rOptions_NavigatorDesign,rOptions_NavigatorLang);
var
  i:integer;
begin
  dgOptionsNavigator.Visible:=rgOptionsNavigator.ItemIndex=0;
  tvOptionsNavigator.Visible:=rgOptionsNavigator.ItemIndex=1;
  if dgOptionsNavigator.Visible then
  begin
    pnOptionsLeft.Width:=96;
    for i:=0 to 4 do
     if dgOptionsNavigator.Canvas.TextWidth(cSimpleNavigator[i])>pnOptionsLeft.Width then
      pnOptionsLeft.Width:=dgOptionsNavigator.Canvas.TextWidth(cSimpleNavigator[i])+4;
    dgOptionsNavigator.Row:=0;
    dgOptionsNavigator.Tag:=0;
  end else
    pnOptionsLeft.Width:=160;
end;

procedure TfmGameOptions.WCMUpdate(Sender: TObject);
begin
  lbChallengePeriod.Enabled:=(rbWCMChallenge.Checked);
  seChallengePeriod.Enabled:=(rbWCMChallenge.Checked);
  lbChallengePenalty.Enabled:=(rbWCMChallenge.Checked);
  seChallengePenalty.Enabled:=(rbWCMChallenge.Checked);
  lbChallengeBonus.Enabled:=(rbWCMChallenge.Checked);
  seChallengeBonus.Enabled:=(rbWCMChallenge.Checked);
  UpdatePresetInfo;
  if assigned(Dictionary) then
    UpdateDictionaryInfo;
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not Application.Terminated then
  begin
    if FSettingsChanged then
    begin
      case MessageDlg(Language.Format(rOptions_ChangeNow,[cbPresets.Text]),mtConfirmation,[mbYes,mbNo,mbCancel],0) of
       mrYes : acSavePreset.Execute; //save all setting; game name with current value of cbpreset or 'Standard' if empty
       mrNo  : SaveSettings('');//save only design and non-rules settings
       mrCancel : CanClose:=false;
      end;
    end else
      SaveSettings(''); //always save visual options
  end;
  if CanClose then
  begin
    Config.SaveWindowPosition(fmGameOptions);
    Config.Write('General/Position/spOptions/Left',spOptions.Left);
    Config.Write('General/Position/spDictionary/Top',spDictionary.Top);
    Config.Write('General/Position/spDesign/Top',spDesign.Top);
    Config.Write('General/Position/spLocalization/Top',spLocalization.Top);
  end;
end;

procedure TfmGameOptions.cbProxyChange(Sender: TObject);
begin
  edProxyIP.Enabled:=cbProxy.Checked;
  edProxyPort.Enabled:=cbProxy.Checked;
  edProxyUser.Enabled:=cbProxy.Checked;
  edProxyPass.Enabled:=cbProxy.Checked;
  lbProxyIP.Enabled:=cbProxy.Checked;
  lbProxyPort.Enabled:=cbProxy.Checked;
  lbProxyUser.Enabled:=cbProxy.Checked;
  lbProxyPass.Enabled:=cbProxy.Checked;
end;

function TfmGameOptions.GetNumberOfLetters: word;
var
  i:integer;
begin
  Result:=0;
  for i:=1 to sgLetters.RowCount-1 do
    inc(Result,StrtoIntDef(sgLetters.Cells[1,i],0));
  inc(Result,seNumberOfJokers.Value);
  inc(Result,seNumberOfRandomLetters.Value);
end;

procedure TfmGameOptions.SetConfiguration(const aValue: string);
var
  i:integer;
begin
  i:=cbPresets.Items.IndexOf(aValue);
  if (i>-1) and (i<>cbPresets.ItemIndex) then
  begin
    cbPresets.ItemIndex:=i;
    acLoadPreset.Execute;
  end else
    LoadSettings('');//read only design
end;

function TfmGameOptions.AvailableLetters: string;
var
  i:integer;
begin
  Result:='';
  for i:=1 to sgLetters.RowCount-1 do
    Result:=Result+sgLetters.Cells[0,i];
end;

function TfmGameOptions.ConfigString(const aOptions:TConfigOptions): TKeyValuePair;
var
  s:string;
  cWW:string;
begin
  Result.Key:='';
  Result.Value:='';
  cWW:=LineEnding;
  case aOptions of
   coInfoDictionary:
     if assigned(Dictionary) then
     begin
       Result.Key:=Result.Key+cWW+gbDictionary.Caption+':';
       Result.Value:=Result.Value+cWW+Dictionary.Info[itFileName];
     end;
   coInfoUsedCategories:
     if assigned(Dictionary) then
     begin
       Result.Key:=Result.Key+cWW+gbCategories.Caption+':';
       Result.Value:=Result.Value+cWW+Dictionary.Info[itCategoriesEnabled];
     end;
   coInfoBoard:
     begin
       Result.Key:=Result.Key+cWW+gbBoard.Caption+':';
       s:=cbBoard.Text;
       if cbBoard.ItemIndex=cbBoard.Items.Count-1 then //special
        case rbDim3D.Checked of
          false : s:=s+'(2D/#'+seBoardSize.Text+')';
          true  : s:=s+'(3D/#'+seBoardSize.Text+')';
        end;
       Result.Value:=Result.Value+cWW+s;
     end;
  coTournament:
    begin
      Result.Key:=Result.Key+cWW+gbRules.Caption+':';
      Result.Value:=Result.Value+cWW+cbRulesSet.Text;
    end;
  coInfoLetterset:
    begin
      Result.Key:=Result.Key+cWW+gbLetter.Caption+':';
      if cbLetterSet.Text<>'' then
      begin
        s:=cbLetterSet.Text;
        if seLetterSetMultiplier.Value>1 then
          s:=s+' x'+seLetterSetMultiplier.Text;
      end else
        s:='special ('+inttostr(NumberOfLetters)+' '+rOptions_Letters+')';
      Result.Value:=Result.Value+cWW+s;
    end;
  coInfoPieces:
    begin
      Result.Key:=Result.Key+cWW+rNewGame_InfoPieces;
      Result.Value:=Result.Value+cWW+seNumberOfLetters.Text
    end;
  coInfoJokers:
    begin
      Result.Key:=Result.Key+cWW+rNewGame_InfoJokers;
      Result.Value:=Result.Value+cWW+seNumberOfJokers.Text
    end;
  coInfoRandoms:
    begin
      Result.Key:=Result.Key+cWW+rNewGame_InfoRandoms;
      Result.Value:=Result.Value+cWW+seNumberOfRandomLetters.Text
    end;
  coInfoTime:
    begin
      s:=gbTimeControl.Caption+':';
      s:=StringReplace(s,'&','',[rfReplaceAll]);
      Result.Key:=Result.Key+cWW+s;
      if rbNoLimit.Checked then
        s:=rMain_SBTimeNoLimit else
      if fmGameOptions.rbPerMove.Checked then
        s:=rMain_SBTimePerMove+' ('+meTimeLimitPerMove.Text+')' else
        s:=rMain_SBTimePerGame+' ('+meTimeLimitPerGame.Text+')';
      s:=StringReplace(s,'&','',[rfReplaceAll]);
      Result.Value:=Result.Value+cWW+s;
    end;
  coInfoTimePanalty:
    begin
      s:=gbTimePenalty.Caption+':';
      s:=StringReplace(s,'&','',[rfReplaceAll]);
      Result.Key:=Result.Key+cWW+s;
      if rbPerGame.Checked then
      begin
        if rbTimePenaltyEndGame.Checked then
          s:=rbTimePenaltyEndGame.Caption else
          s:=rbTimePenaltyBuyTime.Caption+' (-'+seTimePenaltyValue.Text+'/#'+seTimePenaltyCount.Text+')';
        s:=StringReplace(s,'&','',[rfReplaceAll]);
      end else
        s:=rOptions_NoPenalty;
      Result.Value:=Result.Value+cWW+s;
    end;
  coCambioSecco:
    begin
      s:=cbCambioSecco.Caption+':';
      s:=StringReplace(s,'&','',[rfReplaceAll]);
      Result.Key:=Result.Key+cWW+s;
      if cbCambioSecco.Checked then
        s:=rOptions_Yes else
        s:=rOptions_No;
      Result.Value:=Result.Value+cWW+s;
    end;
  coInfoWordCheck:
    begin
      Result.Key:=Result.Key+cWW+gbWordCheckMode.Caption+':';
      if rbWCMTakeback.Checked then
        s:=rWordCheck_Ask else
      if rbWCMPoll.Checked then
        s:=rWordCheck_Poll else
        s:=rWordCheck_Challenge+' ('+seChallengePeriod.Text+'/'+seChallengePenalty.Text+'/'+seChallengeBonus.Text+')';
      if cbCLABBERS.Checked then
        s:=s+', '+cbCLABBERS.Caption;
      Result.Value:=Result.Value+cWW+s;
    end;
  end;//case
  Delete(Result.Key,1,length(cWW)); //remove leading cWW
  Delete(Result.Value,1,length(cWW));
end;

procedure TfmGameOptions.VisualChange(Sender: TObject);
begin
  if assigned(FOnConfigChange) and not (gsLoading in Scrabble.GameState) then
    FOnConfigChange(self);
end;

{$include go_actions.inc}
{$include go_dic.inc}    //dictionary
{$include go_let.inc}    //letters    CommaText:=GetBoardString;

{$include go_rul.inc}    //letters
{$include go_bd.inc}     //board
{$include go_dg.inc}     //design
{$include go_cp.inc}     //computer
{$include go_op.inc}     //options, network
{$include go_sg.inc}     //string grid routines

{$include go_ls.inc}     //load/save

initialization
  {$I ugameoptions.lrs}

end.
