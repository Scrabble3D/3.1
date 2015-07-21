{ New game dialog
  number of players and names;
  public property PlayerNames is used as commatext in main.newgame

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

unit unewgame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, LResources,
  LCLIntf, LCLType, Graphics, Buttons,
  urandom, ulanguage, ugameoptions, Grids;

type

{ TfmNewGame }

  TfmNewGame = class(TForm)
    cbConfiguration: TComboBox;
    cbAutoPause: TCheckBox;
    cbPlayer1: TComboBox;
    cbPlayer2: TComboBox;
    cbPlayer3: TComboBox;
    cbPlayer4: TComboBox;
    gbPlayers: TGroupBox;
    rb1Player: TRadioButton;
    rb2Player: TRadioButton;
    rb4Player: TRadioButton;
    rb3Player: TRadioButton;
    lbConfigPresets: TLabel;
    pnBottom: TPanel;
    btnNewGameOK: TButton;
    btnNewGameCancel: TButton;
    gbConfig: TGroupBox;
    sgPreset: TStringGrid;
    Splitter: TSplitter;
    procedure cbConfigurationChange(Sender: TObject);
    procedure cbPlayerExit(Sender: TObject);
    procedure DoTimeOut(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure rbPlayerChange(Sender: TObject);
    procedure sgPresetDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgPresetResize(Sender: TObject);
  private
    FDemoMode: boolean;
    FRandSeed: LongWord;
    FTimer: TTimer;
    function GetNetworkMode: boolean;
    function GetNumberOfPlayers: byte;
    function GetPlayerNames: string;
    procedure SetDemoMode(AValue: boolean);
    procedure SetNetworkMode(const AValue: boolean);
    procedure LoadPlayerFromConfig;
    procedure SavePlayerToConfig;
    procedure SetNumberOfPlayers(AValue: byte);
    procedure UpdateGamePreset;
  published
  public
    function ShowModal(const aCaption:string=''): integer; reintroduce;
    procedure UpdateLanguage;
    property PlayerNames : string read GetPlayerNames;
    property GameSeed:LongWord read FRandSeed write FRandSeed;
    property NetworkMode:boolean read GetNetworkMode write SetNetworkMode;
    property DemoMode:boolean read FDemoMode write SetDemoMode;
    property NumberOfPlayers:byte read GetNumberOfPlayers write SetNumberOfPlayers;
  end;

var
  fmNewGame: TfmNewGame;

implementation

uses
  uconfig;

procedure TfmNewGame.FormCreate(Sender: TObject);
begin
  inherited;
  gbPlayers.Width:=Config.Read('General/Position/NewPlayers/Special',gbPlayers.Width);
  LoadPlayerFromConfig;
  rbPlayerChange(self);
  FTimer:=TTimer.Create(self);
  FTimer.OnTimer:=@DoTimeOut;
  FTimer.Enabled:=false;
end;

procedure TfmNewGame.FormDestroy(Sender: TObject);
begin
  FTimer.Free;
  inherited;
end;

procedure TfmNewGame.FormHide(Sender: TObject);
begin
  if assigned(Config) and not NetworkMode then
  begin
    SavePlayerToConfig;
    Config.Write('Position/NewPlayers',gbPlayers.Width);
  end;
end;

procedure TfmNewGame.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key=chr(VK_ESCAPE) then
    ModalResult:=mrCancel else
  if Key=chr(VK_RETURN) then
    ModalResult:=mrOk;
end;

procedure TfmNewGame.cbConfigurationChange(Sender: TObject);
begin
  if MessageDlg(Language.Format(rNewGame_LoadConfig,[cbConfiguration.Text]),mtConfirmation,[mbYes, mbCancel],0)=mrYes then
    fmGameOptions.Configuration:=cbConfiguration.Text else
    cbConfiguration.Text:=fmGameOptions.Configuration;
  UpdateGamePreset;
end;

procedure TfmNewGame.cbPlayerExit(Sender: TObject);
begin
  with Sender as TComboBox do
   if Items.IndexOf(Text)=-1 then
    Items.Add(Text);
end;

procedure TfmNewGame.DoTimeOut(Sender: TObject);
begin
  Caption:=Language.Plural(rPoll_TimeOut,FTimer.Tag);
  if FTimer.Tag>0 then
  begin
    FTimer.Tag:=FTimer.Tag-1;
    FTimer.Enabled:=true;
    FTimer.Interval:=1000;
  end else
  begin
    ModalResult:=mrCancel;
    FTimer.Enabled:=false;
  end;
end;

procedure TfmNewGame.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i:integer;
begin
  if ModalResult=mrOk then
  begin
    with TStringList.Create do
    try
      Sorted:=true;
      CommaText:=PlayerNames;
      while Find('Computer',i) do
       Delete(i);
      CanClose:=Count>0;
    finally
      Free;
    end;
    if not CanClose then
     MessageDlg(rNewGame_ErrorHuman,mtConfirmation,[mbOk],0);
  end else
    CanClose:=true;
end;

procedure TfmNewGame.FormShow(Sender: TObject);
begin
  {$ifdef Darwin}
  cbConfiguration.OnChange:=nil;
  {$endif}
  cbConfiguration.Items.Clear;
  if not NetworkMode then
  begin
    cbConfiguration.Items.Assign(fmGameOptions.cbPresets.Items);
    cbConfiguration.ItemIndex:=fmGameOptions.cbPresets.ItemIndex;
  end else
    cbConfiguration.Items.Add(fmGameOptions.cbPresets.Text);

  if cbPlayer1.Enabled then //not (gsNetwork in Scrabble.GameState)
  begin
    GenRandomize;  //reliable randomize
    FRandSeed:=genrandom;
  end;

  UpdateGamePreset;
  {$ifdef Darwin}
  cbConfiguration.OnChange:=@cbConfigurationChange;
  {$endif}
end;

function TfmNewGame.GetPlayerNames: string;
var
  z:byte;
begin
  z:=NumberOfPlayers;
  Result:='"'+cbPlayer1.Text+'"';
  if z>1 then
    Result:=Result+',"'+cbPlayer2.Text+'"';
  if z>2 then
    Result:=Result+',"'+cbPlayer3.Text+'"';
  if z>3 then
    Result:=Result+',"'+cbPlayer4.Text+'"';
end;

procedure TfmNewGame.SetDemoMode(AValue: boolean);
begin
  if FDemoMode<>aValue then
  begin
    FDemoMode:=aValue;
    if FDemoMode then
    begin
      rb4Player.Checked:=true;
      cbPlayer1.Text:=rPoll_Player1;//'1st player'
      cbPlayer2.Text:=rPoll_Player2;
      cbPlayer3.Text:=rPoll_Player3;
      cbPlayer4.Text:=rPoll_Player4;
    end else
      LoadPlayerFromConfig;
  end;
end;

function TfmNewGame.GetNetworkMode: boolean;
begin
  Result:=not cbPlayer1.Enabled;
end;

function TfmNewGame.GetNumberOfPlayers: byte;
begin
  if rb4Player.Checked then
    Result:=4 else
  if rb3Player.Checked then
    Result:=3 else
  if rb2Player.Checked then
    Result:=2 else
  if rb1Player.Checked then
    Result:=1;
end;

procedure TfmNewGame.rbPlayerChange(Sender: TObject);
var
  z:byte;
begin
  if not NetworkMode then
  begin
    z:=NumberOfPlayers;
    cbPlayer1.Enabled:=z>0;
    cbPlayer2.Enabled:=z>1;
    cbPlayer3.Enabled:=z>2;
    cbPlayer4.Enabled:=z>3;
  end;
end;

procedure TfmNewGame.sgPresetDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aFlags: Cardinal;
begin
  sgPreset.Canvas.Brush.Style:=bsClear;
  sgPreset.Canvas.Brush.Color:=clForm;  //needed for windows to avoid overrides; not nescessary at gameoptions with tabsheets
  sgPreset.Canvas.FillRect(aRect);
//  sgPreset.Canvas.Brush.Style:=bsClear;
  sgPreset.Canvas.Font.Style:=[];
  sgPreset.Canvas.Font.Color:=clDefault;
  if aCol=0 then
  begin
    aFlags:=DT_RIGHT or DT_TOP;
    InflateRect(aRect,-2,0);
  end else
    aFlags:=DT_LEFT or DT_TOP or DT_WORDBREAK;
  DrawText(sgPreset.Canvas.Handle,PChar(sgPreset.Cells[aCol,aRow]),-1,aRect,aFlags);
end;

procedure TfmNewGame.sgPresetResize(Sender: TObject);
var
  aRow: integer;
  aRect: TRect;
begin
  //col width
  sgPreset.AutoSizeColumn(0);
  if sgPreset.ClientWidth-sgPreset.ColWidths[0]>50 then
    sgPreset.ColWidths[1]:=sgPreset.ClientWidth-sgPreset.ColWidths[0] else
    sgPreset.ColWidths[1]:=50;//min width
  //individual row height
  for aRow:=0 to sgPreset.RowCount-1 do
  begin
    aRect:=Bounds(0,0,sgPreset.ColWidths[1],24);
    DrawText(sgPreset.Canvas.Handle, PChar(sgPreset.Cells[1,aRow]),-1,aRect, DT_LEFT or DT_TOP or DT_WORDBREAK or DT_CALCRECT);
    sgPreset.RowHeights[aRow]:=aRect.Bottom-aRect.Top+2;
  end;
end;

procedure TfmNewGame.SetNetworkMode(const AValue: boolean);
begin
  cbConfiguration.Enabled:=not aValue;
  cbPlayer1.Enabled:=not aValue;
  cbPlayer2.Enabled:=not aValue;
  cbPlayer3.Enabled:=not aValue;
  cbPlayer4.Enabled:=not aValue;
  rb1Player.Enabled:=not aValue;
  rb2Player.Enabled:=not aValue;
  rb3Player.Enabled:=not aValue;
  rb4Player.Enabled:=not aValue;
  cbAutoPause.Enabled:=not aValue;
  if not aValue then  //not (gsNetwork in Gamestate)
  begin
    gbPlayers.Enabled:=true;
    LoadPlayerFromConfig;
  end else
    cbAutoPause.Checked:=false;
end;

procedure TfmNewGame.SavePlayerToConfig;
var
  j:integer;
  sl:TStringList;
begin
  sl:=TStringList.Create;
  try
    sl.Duplicates:=dupIgnore;
    sl.Sorted:=true;
    sl.AddStrings(cbPlayer1.Items);
    sl.AddStrings(cbPlayer2.Items);
    sl.AddStrings(cbPlayer3.Items);
    sl.AddStrings(cbPlayer4.Items);
    while sl.Find(rPoll_Player1,j) do sl.Delete(j);
    while sl.Find(rPoll_Player2,j) do sl.Delete(j);
    while sl.Find(rPoll_Player3,j) do sl.Delete(j);
    while sl.Find(rPoll_Player4,j) do sl.Delete(j);
    while sl.Find('Computer',j) do sl.Delete(j);
    Config.Write('General/LastPlayer/Names',sl.CommaText);
  finally
    sl.Free;
  end;
  Config.Write('General/LastPlayer/Count',NumberOfPlayers);
  Config.Write('General/AutoPause/Value',cbAutoPause.Checked);
  Config.Write('General/LastPlayer/Item:0',cbPlayer1.Text);
  Config.Write('General/LastPlayer/Item:1',cbPlayer2.Text);
  Config.Write('General/LastPlayer/Item:2',cbPlayer3.Text);
  Config.Write('General/LastPlayer/Item:3',cbPlayer4.Text);
end;

procedure TfmNewGame.SetNumberOfPlayers(AValue: byte);
begin
  case aValue of
   1:rb1Player.Checked:=true;
   2:rb2Player.Checked:=true;
   3:rb3Player.Checked:=true;
   4:rb4Player.Checked:=true;
  end;
end;

procedure TfmNewGame.UpdateGamePreset;
var
  i,j: integer;
begin
  sgPreset.Clear;
  sgPreset.RowCount:=high(StandardOptions)+1;
  for i:=0 to high(StandardOptions) do
  with fmGameOptions.ConfigString(StandardOptions[i]) do
  begin
    sgPreset.Cells[0,i]:=Key;
    j:=Pos(cTS,Value);
    if j>0 then
      sgPreset.Cells[1,i]:=Copy(Value,1,j-1) else
      sgPreset.Cells[1,i]:=Value;
    if sgPreset.Cells[1,i]='' then
      sgPreset.Cells[1,i]:=' ';
  end;
  sgPresetResize(self);
end;

procedure TfmNewGame.LoadPlayerFromConfig;
 procedure PreparePlayerNames(aComboBox:TComboBox;const aNames,aDef:string);
 begin
   aComboBox.Clear;
   aComboBox.Items.CommaText:=aNames;
   if aComboBox.Items.IndexOf('Computer')=-1 then
     aComboBox.Items.Add('Computer');
   aComboBox.Items.Add(aDef);
 end;
var
  s:string;
begin
  NumberOfPlayers:=Config.Read('General/LastPlayer/Count',1);

  s:=Config.Read('General/LastPlayer/Names','');
  PreparePlayerNames(cbPlayer1,s,rPoll_Player1);
  PreparePlayerNames(cbPlayer2,s,rPoll_Player2);
  PreparePlayerNames(cbPlayer3,s,rPoll_Player3);
  PreparePlayerNames(cbPlayer4,s,rPoll_Player4);

  cbPlayer1.Text:=Config.Read('General/LastPlayer/Item:0',rPoll_Player1); //'1st player'
  cbPlayer2.Text:=Config.Read('General/LastPlayer/Item:1',rPoll_Player2);
  cbPlayer3.Text:=Config.Read('General/LastPlayer/Item:2',rPoll_Player3);
  cbPlayer4.Text:=Config.Read('General/LastPlayer/Item:3',rPoll_Player4);

  cbAutoPause.Checked:=Config.Read('General/AutoPause/Value',cbAutoPause.Checked);
end;

function TfmNewGame.ShowModal(const aCaption: string): integer;
begin
  if aCaption='' then
    Caption:=rNewGame_Caption else
    Caption:=aCaption;
  FTimer.Interval:=20000;//sec
  FTimer.Tag:=10; //plus n*sec with feedback
  FTimer.Enabled:=NetworkMode;
  Result:=inherited ShowModal;
end;

procedure TfmNewGame.UpdateLanguage;
begin
  BiDiMode:=Language.BiDiMode;
  LoadPlayerFromConfig;
end;

initialization
  {$i unewgame.lrs}

end.
