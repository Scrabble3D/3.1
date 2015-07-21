{ Scrabble data handlung and core routines
  TLetter: a single piece with position, state, caption etc.
  TBoard: list of TLetters with procedures to check move
  TGame: descendent of TBoard with remaing game stuff (NewGame(), NextPlayer() etc.)

  v3.1.3; 2015-Mar-01
  Copyleft (C) GPLv3: 1996-2015 Heiko Tietze heiko_tietze@web.de

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

unit uscrabble;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls,
  Inifiles, LConvEncoding, LazUTF8, LCLIntf, Dateutils, FileUtil, //save|load game in utf8
  urandom, uletter, uboard, utypes;

type

  TPlayer=record
    Name            : string;
    Points          : integer;  //sum of placed points; minus in case of deduction
    CambioSecco     : boolean;  //cs available
    mTimeElapsed    : longword; //msec
    TimePenaltyLeft : byte;     //count
  end;

  TScrabbleDimension=(D2,D3);
  TMoveState=(msNone,msJokerExchanged,msLetterPlaced,msLetterExchange);
  TGameState=set of (gsActive,gsLoading,gsRunning,gsComputing,gsNetwork,gsGameEnd,gsDestroying,
                     gsJokerExchange,gsKibitz,gsChangeIsPass,gsDemo,gsDialog,gsCambioSecco,gsPaused,
                     gsAddLettersLeft,gsSubstractLettersLeft,gsNextPlayer,gsJokerization,gsCLABBERS,
                     gsPoll);

  TWordCheckMode=(wcAsk=0,wcPoll=1,wcChallenge=2);
  TTimeControlSetting=(tcNoLimit=0,tcPerMove=1,tcPerGame=2);

  TScrabbleEvent=(seRackLetterDragging,seExchangeRackLetter,seToggleChangeState,seActiveDimension,
                  seCambioSecco,seJokerize,seReset,seTakeback,sePlaceLetterAt,seRemoveLetterFrom,
                  seNextPlayer,seNewGame,seLoadGame,seUnknown);//seUnknown for external calls
  TRepaintEvent=procedure(aSender:TScrabbleEvent) of object;

  TOnSetHistory=procedure(const aPlacedWord,aConnectedWords,aExchangedLetters,aCambioSecco:string;
                          const aRackLetters: THistoryArray;
                          const aPlayer:byte;
                          const aMoveNumber,aValue:word;
                          const aTimeUsed:Longword;
                          const aIsScrabble:boolean;
                          const aDimension,aPosition:byte) of object;

  TOnGetLetterValue=function(const aChar:widechar):byte of object;
  TChallengeResult=(crNone=0,crValid=1,crInvalid=2,crChecked=3);

  TOnNewGame=procedure(out aPlayerNames      : string;
                       out aRackSize         : byte;
                       out aBoardSize        : byte;
                       out aDimension        : TScrabbleDimension;
                       out aFieldTypeArray   : TFieldTypeArray;
                       out aRandSeed         : LongWord;
                       out aLetters          : TLetterList;
                       out aCanJokerExchange : boolean;
                       out aGameEndBonus     : word;
                       out aNumberOfPasses   : byte;
                       out aJokerPenalty     : byte;
                       out aChangeIsPass     : boolean;
                       out aTimeControl      : TTimeControlSetting;
                       out aTimeControlValue : Longword;
                       out aLimitedExchange  : byte;
                       out aCambioSecco      : boolean;
                       out aJokerizeLetter   : boolean;
                       out aAddLettersLeft   : boolean;
                       out aSubtractLettersLeft : boolean;
                       out aTimePenaltyValue : byte;
                       out aTimePenaltyCount : byte;
                       out aTimeGameLost     : boolean;
                       out aWordCheckMode    : TWordCheckMode;
                       out aWordCheckPeriod  : byte;
                       out aWordCheckPenalty : byte;
                       out aWordCheckBonus   : byte;
                       out aScrabbleBonus    : byte;
                       out aCLABBERS         : boolean) of object;
  { TGame }
  TGame=class(TBoard)
  private
    FCambioSeccoLetters : string;
    FDimension          : TScrabbleDimension;
    FActiveDimension, FPlacedDimension : TActiveDimension;   //dx|dy|dz
    FCurrentMove        : word;               //0..n
    FActualMove         : word;               //FCurrentMove or less
    FPreviousMoveState  : TMoveState;
    FExchangedLetters   : string;
    FHasChallenged      : TChallengeResult;
    FJokerizeLetter     : boolean;            //true = allow to turn one tile into joker, but score with 50+ points
    FJokerPenalty       : byte;
    FNoLettersPlaced    : byte;               //0..FNumberOfPlayers*FNumberOfPasses
    FLastNoLettersPlaced : byte;              //stores FNoLettersPlaced for takeback
    FNumberOfPasses     : byte;               //2 or 3
    FTimePenaltyValue   : byte;               //points per minute
    FTimePenaltyCount   : byte;
    FLimitedExchange    : byte;
    FMoveTime           : TDateTime;          //ms (Now, MSecBetween)
    FPauseTime          : DWord;              //ms (GetTickCount)
    FRevertReading      : boolean;
    FGameState          : TGameState;
    FLettersLeft        : word;
    FTimeControlSetting : TTimeControlSetting;
    FTimeControlValue   : longword;           //ms (GetTimeControlValue in s)
    FTimeGameLost       : boolean;
    FOnMoveNumberChange : TNotifyEvent;
    FOnAfterNewGame     : TNotifyEvent;       //... etc.
    FWordCheckBonus     : byte;
    FWordCheckMode      : TWordCheckMode;
    FWordCheckPeriod    : dword;              //ms
    FWordCheckPenalty   : byte;
    FOnNewGame          : TOnNewGame;
    FOnAfterNextPlayer  : TNotifyEvent;
    FOnGameEnd          : TNotifyEvent;
    FOnRepaint          : TRepaintEvent;
    FOnSetHistory       : TOnSetHistory;
    FOnGetLetterValue   : TOnGetLetterValue;
    FCurrentPlayer      : byte;               //FCurrentMove mod FNumberOfPlayers
    FLocalPlayer        : byte;               //set or not set to FCurrentPlayer at OnNextPlayer
    FRandSeed           : LongWord;
    FPlayers            : array of TPlayer;
    FMoveState          : TMoveState;
    FGameEndBonus       : word;
    FExchangedJoker     : TLetter;
    function GetAllLettersValue: word;
    function GetAvailableLetters: string;     //needed in bruteforce
    function GetCanCallenge: boolean;
    function GetMoveTime: word;  //s
    function GetMoveTime_ms: dword;  //ms
    function GetTimeControlValue: word;
    function GetTimeOut : boolean;
    function GetLettersLeft: string;
    function GetNumberOfMarkedLetters: byte;
    function GetNumberOfPlacedLetters: byte;
    function GetNumberOfPlayers: byte;
    function GetPlayer(index: byte): TPlayer;
    function GetWordAtPos(ax, ay, az: byte;OnlyPlaced:boolean): string;
    function GetWordCheckPeriod: byte; //s
    procedure SetActiveDimension(const aValue: TActiveDimension);
    procedure SetActualMove(const aValue: word);
    function GetRackLetter(aPlayer,aRackPos: byte): TLetter;
    procedure SetRackLetterIsDragging(aPlayer, aRackPos: byte; const aValue: boolean);
    procedure SetPlayerValue(index: byte; const aValue: integer);
    procedure UpdateMoveState;
    procedure ExchangeRackLetter(La, Lb: TLetter); //legacy funtion
  public
    constructor Create;
    destructor Destroy; override;
    function ShowStatus:string;
    function CheckMove(AskForWord:boolean=false):TLastError;override;
    procedure NewGame;
    procedure NextPlayer;  //no check of move here
    procedure Convert2DTo3D(aCol,aRow : byte; var x,y,z : byte);
    procedure Convert2DTo3D(aCol,aRow : byte; var x,y,z : byte; const aDimension:TActiveDimension);overload;
    function CambioSecco:boolean;
    function ToggleChangeState(aRackPos:byte):boolean;
    function PosToString(aAxis:TDimension;aPos:byte;UseRoman:boolean=false):string;

    procedure TogglePaused;
    procedure PlaceLetterAt(aRack : TLetter; x,y,z : Byte);
    procedure RemoveLetterFrom(x,y,z : Byte; Silent:boolean=false);  //ls_Dragging if not silent
    procedure RestoreBoard;
    procedure SaveTo(const aValue:string);
    procedure LoadFrom(const aValue:string);
    procedure Reset;
    procedure SetPlayerTime(const aValue:word);

    procedure Takeback;
    procedure AddPlayerTime(const aPlayer:byte; const aValue: longword;const aTimeOut:boolean=false);
    function DoJokerizeLetter(aLetter:TLetter):boolean;
  public
    //defined on NewGame()
    property Dimension       : TScrabbleDimension read FDimension;  //D2|D3
    property NumberOfPlayers : byte read GetNumberOfPlayers;
    //defined on NextPlayer()
    property CurrentMove      : word read FCurrentMove;
    property ActualMove       : word read FActualMove write SetActualMove; //can be less than currenmove to show history
    property CurrentPlayer    : byte read FCurrentPlayer;
    property LocalPlayer      : byte read FLocalPlayer write FLocalPlayer;
    property Player[index:byte]  : TPlayer read GetPlayer;
    property PlayerValue[index:byte] : integer write SetPlayerValue;
    property RackLetter[aPlayer,aRackPos:byte] : TLetter read GetRackLetter;
    property RackLetterIsDragging[aPlayer,aRackPos:byte]:boolean write SetRackLetterIsDragging;
    property NumberOfLettersLeft : word read FLettersLeft;
    property LettersLeft         : string read GetLettersLeft;
    property MoveState           : TMoveState read FMoveState;
    property GameState           : TGameState read FGameState write FGameState;
    property NoLettersPlaced     : byte read FNoLettersPlaced;
    property NumberOfPasses      : byte read FNumberOfPasses;
    property TimeControlSetting  : TTimeControlSetting read FTimeControlSetting;
    property TimeGameLost        : boolean read FTimeGameLost;
    property ActiveDimension     : TActiveDimension read FActiveDimension write SetActiveDimension;
    property PlacedDimension     : TActiveDimension read FPlacedDimension;
    property JokerizeLetter      : boolean read FJokerizeLetter;
    property WordAtPos[ax,ay,az:byte;OnlyPlaced:boolean] : string read GetWordAtPos;

    property OnMoveNumberChange  : TNotifyEvent read FOnMoveNumberChange write FOnMoveNumberChange;
    property OnNewGame           : TOnNewGame read FOnNewGame write FOnNewGame;
    property OnAfterNewGame      : TNotifyEvent read FOnAfterNewGame write FOnAfterNewGame;
    property OnAfterNextPlayer   : TNotifyEvent read FOnAfterNextPlayer write FOnAfterNextPlayer;
    property OnGameEnd           : TNotifyEvent read FOnGameEnd write FOnGameEnd;
    property OnRepaint           : TRepaintEvent read FOnRepaint write FOnRepaint;
    property OnGetLetterValue    : TOnGetLetterValue read FOnGetLetterValue write FOnGetLetterValue;
    property OnSetHistory        : TOnSetHistory read FOnSetHistory write FOnSetHistory;

    property AllLettersValue     : word read GetAllLettersValue;
    property GameEndBonus        : word read FGameEndBonus;
    property ExchangedJoker      : TLetter read FExchangedJoker;
    property sTimeControlValue    : word read GetTimeControlValue; //s
    property JokerPenalty        : byte read FJokerPenalty;
    property TimePenaltyValue    : byte read FTimePenaltyValue;
    property TimePenaltyCount    : byte read FTimePenaltyCount;
    property LimitedExchange     : byte read FLimitedExchange;
    property RevertReading       : boolean read FRevertReading write FRevertReading;
    property ExchangedLetters    : string read FExchangedLetters;
    property CambioSeccoLetters  : string read FCambioSeccoLetters;
    property WordCheckMode       : TWordCheckMode read FWordCheckMode;
    property CanChallenge        : boolean read GetCanCallenge;
    property HasChallenged       : TChallengeResult read FHasChallenged write FHasChallenged;
    property WordCheckPenalty    : byte read FWordCheckPenalty;
    property WordCheckBonus      : byte read FWordCheckBonus;
    property WordCheckPeriod     : byte read GetWordCheckPeriod; //s
    property IsTimeOut           : boolean read GetTimeOut;
    property sMoveTime           : Word read GetMoveTime;// write SetMoveTime;//s
    property BoardSize;
    property RackSize;
    property BoardLetter;
    property FieldType;
    property LastMove;
    property LeastValue;
    property pausetime:DWord read FPauseTime;
    property movetime:dword read GetMoveTime_ms;
  end;

var
  Scrabble : TGame;

const
  ltJoker='?';
  ltRandom='!';

implementation

{ TGame }

function TGame.GetAvailableLetters: string;
var
  i:integer;
begin
  if LetterList.Count>0 then
    Result:=UTF8Encode(widestring(LetterList[0].What)) else
    Result:='';
  for i:=1 to LetterList.Count-1 do
   if LetterList[i].What<>LetterList[i-1].What then
    Result:=Result+UTF8Encode(widestring(LetterList[i].What));
end;

function TGame.GetCanCallenge: boolean;
begin
  Result:=(FWordCheckMode=wcChallenge) and
          (FHasChallenged=crNone) and
          not (gsDemo in Scrabble.GameState) and
          not (gsComputing in Scrabble.GameState) and
          (gsRunning in Scrabble.GameState) and
          (Scrabble.CurrentMove=Scrabble.ActualMove) and
          (LastMove.PlacedWord<>'') and
          not (gsKibitz in FGameState) and
          (not (gsNetwork in FGameState) or (FLocalPlayer<>((FCurrentMove-1) mod NumberOfPlayers))) and
          (FMoveState=msNone) and
          (GetMoveTime_ms<FWordCheckPeriod);
end;

procedure TGame.TogglePaused;
begin
  if not (gsPaused in FGameState) then
  begin
    FPauseTime:=(GetTickCount-FPauseTime);
    include(FGameState,gsPaused)
  end else
  begin
    FPauseTime:=(GetTickCount-FPauseTime);
    exclude(FGameState,gsPaused);
  end;
end;

function TGame.GetMoveTime_ms: dword;
var
  p:DWord;
begin
  if (gsPaused in FGameState) then
    p:=GetTickCount-FPauseTime else
    p:=FPauseTime;
  Result:=MilliSecondsBetween(Now,FMoveTime);
  if Result>p then
    Result:=Result-p;
end;

function TGame.GetMoveTime: word;
begin
  Result:=round(GetMoveTime_ms/1000);
end;

procedure TGame.SetPlayerTime(const aValue: word);
begin
  FMoveTime:=IncMilliSecond(Now,-(FPlayers[FCurrentPlayer].mTimeElapsed-aValue*1000));
  FPauseTime:=GetTickCount;//=0
end;

function TGame.GetTimeControlValue: word;
begin
  Result:=round(FTimeControlValue/1000);//s
end;

function TGame.GetTimeOut: boolean;
begin
  Result:=false;
  if length(FPlayers)>FCurrentPlayer then
    Result:=(FTimeControlSetting<>tcNoLimit) and
            (FPlayers[FCurrentPlayer].mTimeElapsed+GetMoveTime_ms>=FTimeControlValue);
end;

function TGame.ShowStatus:string;
begin
  Result:='gsActive: '+booltostr(gsActive in FGameState,true)+#13+
          'gsPause: '+booltostr(gsPaused in FGameState,true)+#13+
          'gsLoading: '+booltostr(gsLoading in FGameState,true)+#13+
          'gsRunning: '+booltostr(gsRunning in FGameState,true)+#13+
          'gsComputing: '+booltostr(gsComputing in FGameState,true)+#13+
          'gsNetwork: '+booltostr(gsNetwork in FGameState,true)+#13+
          'gsGameEnd: '+booltostr(gsGameEnd in FGameState,true)+#13+
          'gsDestroying: '+booltostr(gsDestroying in FGameState,true)+#13+
          'gsJokerExchange: '+booltostr(gsJokerExchange in FGameState,true)+#13+
          'gsKibitz: '+booltostr(gsKibitz in FGameState,true)+#13+
          'gsChangeIsPass: '+booltostr(gsChangeIsPass in FGameState,true)+#13+
          'gsDemo: '+booltostr(gsDemo in FGameState,true)+#13+
          'gsDialog: '+booltostr(gsDialog in FGameState,true)+#13+
          'gsAddLettersLeft: '+booltostr(gsAddLettersLeft in FGameState,true)+#13+
          'gsSubstractLettersLeft: '+booltostr(gsSubstractLettersLeft in FGameState,true)+#13+
          'gsNextPlayer: '+booltostr(gsNextPlayer in FGameState,true)+#13+
          'NoLetterplaced/LastNoplaced: '+inttostr(FNoLettersPlaced)+'/'+inttostr(FLastNoLettersPlaced)+#13+
          'MoveTime'+floattostr(GetMoveTime_ms)+#13+
          'PauseTime'+inttostr(FPauseTime);
end;

function TGame.GetAllLettersValue: word;
var i:integer;
begin
  Result:=0;
  for i:=0 to LetterList.Count-1 do
   inc(Result,LetterList[i].Value);
end;

function TGame.GetLettersLeft : string;
var i,j:integer;
begin
  with TStringList.Create do
  try
    Sorted:=true;
    Duplicates:=dupAccept;
    for i:=0 to LetterList.Count-1 do
     with LetterList[i] do
      if (State=lsBag) then
      begin
        if not IsRandom then Add(UTF8Encode(widestring(What)))
                        else Add(UTF8Encode(widestring(ltRandom)));
      end;
    for i:=0 to NumberOfPlayers-1 do
     if i<>FLocalPlayer then
      for j:=0 to RackSize-1 do
       if RackLetter[i,j]<>nil then
       begin
         if not RackLetter[i,j].IsRandom then Add(UTF8Encode(widestring(RackLetter[i,j].What)))
                                         else Add(UTF8Encode(widestring(ltRandom)));
       end;
    if Count>0 then
    begin
      Sort;
      Result:=Strings[0];
      for i:=1 to Count-1 do
      begin
        if Strings[i]<>Strings[i-1] then Result:=Result+',';
        Result:=Result+Strings[i];
      end;
      i:=30;
      while i<UTF8Length(Result) do
      begin
        UTF8Insert(#13+#10,Result,i);
        inc(i,30);
      end;
    end else Result:='';
  finally
    Free;
  end;
end;

function TGame.GetNumberOfMarkedLetters: byte;
var i:integer;
begin
  Result:=0;
  for i:=0 to LetterList.Count-1 do
   with LetterList[i] do
    if (State=lsChange) and (Who=FCurrentPlayer) then
     inc(Result);
end;

function TGame.GetNumberOfPlacedLetters: byte;
var i:integer;
begin
  Result:=0;
  for i:=0 to LetterList.Count-1 do
   with LetterList[i] do
    if (State=lsPlaced) and (Who=FCurrentPlayer) then
     inc(Result);
end;

function TGame.GetNumberOfPlayers: byte;
begin
  if (self<>nil) and (FPlayers<>nil) then
    Result:=length(FPlayers) else
    Result:=0;
end;

function TGame.GetRackLetter(aPlayer,aRackPos: byte): TLetter;
var
  i: integer;
  aLetter: TLetter;
begin
  Result:=nil;
  for i:=0 to LetterList.Count-1 do
  begin
    aLetter:=LetterList[i];
    if (aLetter.State in [lsRack,lsDragging,lsChange]) and
       (aLetter.RackPos=aRackPos) and
       (aLetter.Who=aPlayer) then
     exit(aLetter);
  end;
end;

procedure TGame.SetRackLetterIsDragging(aPlayer, aRackPos: byte;
  const aValue: boolean);
var
  aLetter:TLetter;
begin
  aLetter:=RackLetter[aPlayer,aRackPos];
  if (aLetter<>nil) then
  begin
    case aValue of
     true:aLetter.State:=lsDragging;
     false:aLetter.State:=lsRack;
    end;
    if assigned(FOnRepaint) then
      FOnRepaint(seRackLetterDragging);
  end;
end;

procedure TGame.SetPlayerValue(index: byte; const aValue: integer);
begin
  if index<length(FPlayers) then
    FPlayers[index].Points:=aValue;
end;

procedure TGame.UpdateMoveState;
var
  i: integer;
  x,y,z: integer;
  aLetter: TLetter;
begin
  for x:=0 to BoardSize-1 do
   for y:=0 to BoardSize-1 do
    for z:=0 to BoardSize-1 do
     BoardLetter[x,y,z]:=nil;

  FMoveState:=msNone;
  for i:=0 to LetterList.Count-1 do
  begin
    aLetter:=LetterList[i];
    if aLetter.State=lsChange then
      FMoveState:=msLetterExchange;
    if aLetter.State=lsBoard then
      FMoveState:=msLetterPlaced;
    if aLetter.State in [lsBoard,lsPlaced] then
      BoardLetter[aLetter.Where[dx],aLetter.Where[dy],aLetter.Where[dz]]:=aLetter;
  end;
end;

function TGame.GetPlayer(index: byte): TPlayer;
begin
  if index<length(FPlayers) then
    Result:=FPlayers[index];
end;

function TGame.GetWordAtPos(ax, ay, az: byte; OnlyPlaced:boolean): string;
var s:string;
    i:word;
    IsNew:boolean;
begin
  Result:='';
  s:=inherited WordAtPos(ax,ay,az,dx,i,IsNew);
  if (not IsNew or not OnlyPlaced) and (UTF8Length(s)>1) then Result:=s+',';
  s:=inherited WordAtPos(ax,ay,az,dy,i,IsNew);
  if (not IsNew or not OnlyPlaced) and (UTF8Length(s)>1) then Result:=Result+s+',';
  s:=inherited WordAtPos(ax,ay,az,dz,i,IsNew);
  if (not IsNew or not OnlyPlaced) and (UTF8Length(s)>1) then Result:=Result+s+',';
end;

function TGame.GetWordCheckPeriod: byte;
begin
  Result:=round(FWordCheckPeriod/1000);
end;

function TGame.PosToString(aAxis: TDimension; aPos: byte;UseRoman:boolean=false): string;
 //function intToRoman(Value: Longint): string; @strutils;
 function RomanNumeral(aValue:Word):string;
 const
   cRomanString:array[0..12] of string = ('M','CM','D','CD','C','XC','L','XL','X','IX','V','IV','I');
   cRomanValue:array[0..12] of word = (1000,900,500,400,100,90,50,40,10,9,5,4,1);
 var
   i:byte;
 begin
   i:=0;
   while aValue>0 do
   begin
     Result:='';
     if aValue/cRomanValue[i]>=1 then
     begin
       Result:=Result+cRomanString[i];
       dec(aValue,cRomanValue[i]);
     end else
       inc(i);
   end;
 end;

const GreekLetter:string='αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ';
      LatinLetter:string='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
begin
  case aAxis of
   dx : Result:=IntToStr(aPos+1);
   dy : Result:=UTF8Copy(LatinLetter,aPos+1,1);//chr(ord('A')+aPos);
   dz : if (BoardSize<=UTF8Length(GreekLetter)) and not UseRoman then
          Result:=UTF8Copy(GreekLetter,BoardSize-aPos,1) else
          Result:=RomanNumeral(BoardSize-aPos);
  end;
end;

procedure TGame.ExchangeRackLetter(La, Lb: TLetter);
var i:byte;
begin
  i:=La.RackPos;
  La.RackPos:=Lb.RackPos;
  Lb.RackPos:=i;
  La.State:=lsRack; //from dragging
  Lb.State:=lsRack;
  if assigned(FOnRepaint) then
    FOnRepaint(seExchangeRackLetter);
end;

function TGame.ToggleChangeState(aRackPos: byte): boolean;
var aLetter : TLetter;
begin
  aLetter:=RackLetter[FCurrentPlayer,aRackPos];
  Result:=aLetter<>nil;
  if (aLetter<>nil) and ((FMoveState=msNone) or (FMoveState=msLetterExchange)) then
  begin
    if aLetter.State=lsChange then aLetter.State:=lsRack else
    begin
      if (FLettersLeft>=FLimitedExchange) and
         (FLettersLeft>GetNumberOfMarkedLetters)
         then aLetter.State:=lsChange
         else Result:=false;
    end;
    UpdateMoveState;
  end else Result:=false;
  if assigned(FOnRepaint) then
    FOnRepaint(seToggleChangeState);
end;

procedure TGame.PlaceLetterAt(aRack: TLetter; x, y, z: Byte);
var
  aBoard : TLetter;
begin
  aBoard:=BoardLetter[x,y,z];
  if (aBoard<>nil) and (aBoard.IsJoker) then
  begin
    if (FMoveState=msNone) then
    begin
      aBoard.Who:=FCurrentPlayer;
      aBoard.Value:=0;
      aBoard.IsJoker:=false;
      aBoard.RackPos:=aRack.RackPos;
      aRack.IsJoker:=true;
      aRack.What:=ltJoker;
      aRack.Value:=0;
      aRack.State:=lsRack;
      FExchangedJoker:=aBoard;
      ClearMove;
      FExchangedLetters:=FExchangedLetters+UTF8Encode(widestring(aBoard.What));
      FMoveState:=msJokerExchanged;
    end;
  end else
  if (aBoard=nil) and ((FMoveState=msNone) or (FMoveState=msLetterPlaced)) then
  begin
    aRack.Where[dx]:=x;
    aRack.Where[dy]:=y;
    aRack.Where[dz]:=z;
    aRack.When:=FCurrentMove;
    aRack.State:=lsBoard;
    UpdateMoveState;
  end;
  FPlacedDimension:=FActiveDimension;  //store dimension while placing for gamecourse

  if assigned(FOnRepaint) then
    FOnRepaint(sePlaceLetterAt);
end;

procedure TGame.RemoveLetterFrom(x, y, z: Byte; Silent: boolean);
var
  aLetter : TLetter;
begin
  aLetter:=BoardLetter[x,y,z];
  if (aLetter<>nil) and (aLetter.State=lsBoard) then
  with aLetter do
  begin
    if IsJoker then What:=ltJoker;
    When:=255;
    Where[dx]:=255; Where[dy]:=255; Where[dz]:=255;
    if Silent then
      State:=lsRack else
      State:=lsDragging;
    UpdateMoveState;

    if assigned(FOnRepaint) then
      FOnRepaint(seRemoveLetterFrom);
  end;
end;

procedure TGame.RestoreBoard;
var
  x,y,z : word;
  aLetter : TLetter;
begin
  for x:=0 to BoardSize-1 do
   for y:=0 to BoardSize-1 do
    for z:=0 to BoardSize-1 do
  begin
    aLetter:=BoardLetter[x,y,z];
    if (aLetter<>nil) and (aLetter.State=lsBoard) then
    with aLetter do
    begin
      if IsJoker then What:=ltJoker;
      When:=255;
      Where[dx]:=255; Where[dy]:=255; Where[dz]:=255;
      State:=lsRack;
    end;//with
  end;//for to
  UpdateMoveState;

  if assigned(FOnRepaint) then
    FOnRepaint(seRemoveLetterFrom);
end;

constructor TGame.Create;
begin
  inherited Create;
//  FPaused:=false;
end;

destructor TGame.Destroy;
begin
  setlength(FPlayers,0);
  inherited Destroy; //Board
end;

function TGame.CheckMove(AskForWord: boolean): TLastError;
begin
  if FMoveState=msJokerExchanged then
    Result:=leNoLetter else  //LastMove.UsedLetters have been filled at PlaceLetter; needed for network
    Result:=inherited CheckMove(AskForWord);
end;

procedure TGame.SetActiveDimension(const aValue: TActiveDimension);
begin
  if (aValue.Position<>FActiveDimension.Position) or
     (aValue.Axis<>FActiveDimension.Axis) then
  begin
    FActiveDimension:=aValue;
    if assigned(FOnRepaint) and
       not (gsLoading in FGameState) then
      FOnRepaint(seActiveDimension);
  end;
end;

procedure TGame.SetActualMove(const aValue: word);
begin
  if FMoveState<>msNone then RestoreBoard;
  if aValue<FCurrentMove then
    FActualMove:=aValue else
    FActualMove:=FCurrentMove;
  if assigned(FOnMoveNumberChange) then FOnMoveNumberChange(self);
end;

procedure TGame.Convert2DTo3D(aCol, aRow: byte; var x, y, z: byte);
begin
  Convert2DTo3D(aCol, aRow, x, y, z, ActiveDimension);
end;

procedure TGame.Convert2DTo3D(aCol, aRow: byte; var x, y, z: byte; const aDimension: TActiveDimension);
begin
  if FRevertReading then
    aCol:=BoardSize-aCol-1;
  case aDimension.Axis of
   dx : begin x:=aCol; y:=aRow; z:=aDimension.Position; end;
   dy : begin x:=aCol; y:=aDimension.Position; z:=aRow; end;
   dz : begin x:=aDimension.Position; y:=aRow; z:=aCol; end;
  end;
end;

function TGame.CambioSecco: boolean;
var i,j          : integer;
    aLetter      : TLetter;
    aLettersLeft : TList;
    pInt         : ^integer;
begin
  if FPlayers[FCurrentPlayer].CambioSecco then
  begin
    aLettersLeft:=TList.Create;
    try
      //move all letters from Rack to bag
      for i:=0 to RackSize-1 do
      begin
        aLetter:=RackLetter[FCurrentPlayer,i];
        FCambioSeccoLetters:=FCambioSeccoLetters+UTF8Encode(widestring(aLetter.What));
        aLetter.State:=lsBag;
        aLetter.RackPos:=255;
        aLetter.Who:=255;
        aLetter.When:=FCurrentMove;
        aLetter.Where[dx]:=255;
        aLetter.Where[dy]:=255;
        aLetter.Where[dz]:=255;
      end;
      //create a list with all letters in bag
      for i:=0 to LetterList.Count-1 do
       if LetterList[i].State=lsBag then
       begin
         New(pInt);
         pInt^:=i;
         aLettersLeft.Add(pInt);
       end;
      //move n letters from bag to Rack
      for i:=0 to RackSize-1 do
      begin
        j:=genrandom(aLettersLeft.Count);
        pInt:=aLettersLeft[j];
        with LetterList[pInt^] do
        begin
          State:=lsRack;
          RackPos:=i;
          Who:=FCurrentPlayer;
        end;
        Dispose(pInt);
        aLettersLeft.Delete(j);
      end;
      for i:=0 to aLettersLeft.Count-1 do
      begin
        pInt:=aLettersLeft[i];
        Dispose(pInt);
      end;
      aLettersLeft.Clear;
    finally
      aLettersLeft.Free;
    end;
    FPlayers[FCurrentPlayer].CambioSecco:=false;
    FMoveState:=msNone;
    Result:=true;
    if assigned(FOnRepaint) then
      FOnRepaint(seCambioSecco);
  end else
    Result:=false;
end;

procedure TGame.Reset;
begin
  ClearLetterList;
  ClearMove;
  FCurrentMove:=0;
  FActualMove:=0;
  FLocalPlayer:=0;
  FCurrentPlayer:=0;
  FNoLettersPlaced:=0;
  UpdateMoveState;
//  FPaused:=false;
  setlength(FPlayers,0);
  exclude(FGameState,gsActive);
  exclude(FGameState,gsRunning);
  include(FGameState,gsGameEnd);
  if assigned(FOnRepaint) then
    FOnRepaint(seReset);
end;

{.$define full_takeback}
procedure TGame.Takeback;
var
  i:integer;
  aLetter:TLetter;
begin
  if not (gsDemo in FGameState) and
     (gsRunning in FGameState) and
     {$ifndef full_takeback}
     (FWordCheckMode=wcChallenge) and
     {$endif}
     (FCurrentMove=FActualMove) then
  begin
    RestoreBoard; //if one player has been placed new letters
    //lastmove's value is cleared by restoreboard > placeletters; its unsafe here and done in dochallenge now
//    dec(FPlayers[(Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers].Points,LastMove.Value);
    for i:=0 to LetterList.Count-1 do
     with LetterList[i] do
      if (State=lsPlaced) and (When=FCurrentMove-1) then
      begin
        aLetter:=RackLetter[Who,RackPos];
        if aLetter<>nil then
        with aLetter do
        begin
          State:=lsBag;
          RackPos:=255;
          Who:=255;
          When:=255;
          {$ifdef full_takeback} //why not always?
          Where[dx]:=255;
          Where[dy]:=255;
          Where[dz]:=255;
          {$endif}
        end;
        State:=lsRack;
        When:=255;
        if IsJoker then
          What:=ltJoker;
      end;
    FGameState:=FGameState-[gsGameEnd];
    ClearMove;
    UpdateMoveState;
    {$ifdef full_takeback}
    {$Warning full_takeback}
    dec(FCurrentMove);
    FActualMove:=FCurrentMove;
    {$endif}
    exclude(FGameState, gsGameEnd);
    if (gsChangeIsPass in Scrabble.GameState) then
      FNoLettersPlaced:=FLastNoLettersPlaced+1;
    FLastNoLettersPlaced:=FNoLettersPlaced;
  end;
  if assigned(FOnRepaint) then
    FOnRepaint(seTakeback);
end;

procedure TGame.AddPlayerTime(const aPlayer:byte; const aValue: longword; const aTimeOut: boolean);
begin
  if (aPlayer<length(FPlayers)) then
  begin
    FPlayers[aPlayer].mTimeElapsed:=FTimeControlValue-(aValue*1000);
    FMoveTime:=Now;
    if aTimeOut and (FPlayers[aPlayer].TimePenaltyLeft>0) then
     dec(FPlayers[aPlayer].TimePenaltyLeft);
  end;
end;

function TGame.DoJokerizeLetter(aLetter: TLetter): boolean;
begin
  if FJokerizeLetter then
  begin
    aLetter.IsJoker:=true;
    aLetter.What:=ltJoker;
    aLetter.Value:=0;
    FJokerizeLetter:=false;
    LeastValue:=ScrabbleBonus;
    Result:=true;
    if assigned(FOnRepaint) then
      FOnRepaint(seJokerize);
  end else
    Result:=false;
end;

procedure TGame.NewGame;
var x,y,z   : integer;
    pInt    : ^integer;
    aList   : TList;
    aPlayerNames     : string;
    aBoardSize,aRackSize,aScrabbleBonus, aWordCheckPeriod : byte;
    aTimeControlValue: longword;
    aFieldTypeArray  : TFieldTypeArray;
    aJokerExchange,aChangeIsPass,aCambioSecco,aAdd,aSubstract,
    aJokerizeLetter,aCLABBERS : boolean;
    s:string;
begin
  ClearLetterList;
  ClearMove;
  //pull init var
  FOnNewGame(aPlayerNames,aRackSize,aBoardSize,FDimension,aFieldTypeArray,
             FRandSeed,LetterList,aJokerExchange, FGameEndBonus,
             FNumberOfPasses,FJokerPenalty,aChangeIsPass,
             FTimeControlSetting,aTimeControlValue,
             FLimitedExchange,aCambioSecco,aJokerizeLetter,aAdd,aSubstract,
             FTimePenaltyValue,FTimePenaltyCount,FTimeGameLost,
             FWordCheckMode,aWordCheckPeriod,FWordCheckPenalty,FWordCheckBonus,
             aScrabbleBonus,aCLABBERS);
  ScrabbleBonus:=aScrabbleBonus;
  FWordCheckPeriod:=aWordCheckPeriod*1000; //ms
  if FTimeControlSetting<>tcNoLimit then //ms; convert here for legacy purpose
    FTimeControlValue:=aTimeControlValue*1000 else
    FTimeControlValue:=MaxInt;
  if aJokerizeLetter then include(FGameState,gsJokerization)
                     else exclude(FGameState,gsJokerization);
  if aChangeIsPass then include(FGameState,gsChangeIsPass)
                   else exclude(FGameState,gsChangeIsPass);
  if aJokerExchange then include(FGameState,gsJokerExchange)
                    else exclude(FGameState,gsJokerExchange);
  if aAdd then include(FGameState,gsAddLettersLeft)
          else exclude(FGameState,gsAddLettersLeft);
  if aSubstract then include(FGameState,gsSubstractLettersLeft)
                else exclude(FGameState,gsSubstractLettersLeft);
  if aCambioSecco then include(FGameState,gsCambioSecco)
                  else exclude(FGameState,gsCambioSecco);
  if aCLABBERS then include(FGameState,gsCLABBERS)
               else exclude(FGameState,gsCLABBERS);

  RackSize:=aRackSize;
  BoardSize:=aBoardSize;

  for x:=0 to BoardSize-1 do
   for y:=0 to BoardSize-1 do
    for z:=0 to BoardSize-1 do
     FieldType[x,y,z]:=aFieldTypeArray[x,y,z];

  Seed:=FRandSeed;

  include(FGameState,gsRunning);
  exclude(FGameState,gsGameEnd);

  with TStringList.Create do
  try
    CommaText:=aPlayerNames;
    setlength(FPlayers,Count);
    y:=0;
    repeat
      x:=genrandom(Count);
      with FPlayers[y] do
      begin
        Name:=Strings[x];
        Points:=0;
        mTimeElapsed:=0;
        CambioSecco:=aCambioSecco;
        TimePenaltyLeft:=FTimePenaltyCount;
      end;
      inc(y);
      Delete(x);
    until Count=0;
  finally
    Free;
  end;

  FActiveDimension.Axis:=dx;
  FActiveDimension.Position:=(BoardSize div 2)*integer(FDimension=D3);
  FCurrentMove:=0;
  FActualMove:=0;
  FCurrentPlayer:=0;
  FNoLettersPlaced:=0;
  FLastNoLettersPlaced:=0;
  FMoveState:=msNone;
  FPreviousMoveState:=msNone;

  //create letters and fill bag & Rack
  aList:=TList.Create;
  try
    for x:=0 to LetterList.Count-1 do
    begin
      New(pInt);
      pInt^:=x;
      aList.Add(pInt);
    end;
    s:='';
    //fill Racks
    for x:=0 to length(FPlayers)-1 do
    begin
      for y:=0 to RackSize-1 do
       if aList.Count>0 then
       begin
         z:=genrandom(aList.Count);
         s:=s+inttostr(z)+',';
         pInt:=aList[z];
         with LetterList[pInt^] do
         begin
           State:=lsRack;
           Who:=x;
           RackPos:=y;
         end;
         Dispose(pInt);
         aList.Delete(z);
       end;
    end;
    FLettersLeft:=aList.Count;
    for x:=0 to aList.Count-1 do
    begin
      pInt:=aList[x];
      Dispose(pInt);
    end;
    aList.Clear;
  finally
    aList.Free;
  end;
  FCambioSeccoLetters:='';
  FExchangedLetters:='';
  FJokerizeLetter:=gsJokerization in FGameState;
  LeastValue:=0;
  UpdateMoveState;

  FMoveTime:=Now{GetTickCount};
  FPauseTime:=0;

  include(FGameState,gsActive);
  //repaint -> done by DoRepaint from DoNextPlayer which is called from OnAfterNewGame
  if assigned(FOnAfterNewGame) then
    FOnAfterNewGame(self);
end;

procedure TGame.NextPlayer;
var
  i,j          : integer;
  pInt         : ^integer;
  aLettersLeft : TList;
  aLetter      : TLetter;
  aRackLetters : THistoryArray;
  s            : string;
  mSecUsed     : longword;
begin
  j:=0;
  setlength(aRackLetters,RackSize);
  for i:=0 to RackSize-1 do
    aRackLetters[i]:=-1;
  with TStringList.Create do
  try
    for i:=0 to LetterList.Count-1 do
     with LetterList[i] do
     begin
       if (State in [lsRack,lsChange,lsBoard]) and
          (Who=FCurrentPlayer) then
          aRackLetters[RackPos]:=i;
       if State=lsBoard then
       begin
         State:=lsPlaced;
         inc(j);
       end;
     end;
  finally
    Free;
  end;
  //update Rack
  aLettersLeft:=TList.Create;
  try
    for i:=0 to LetterList.Count-1 do
     if LetterList[i].State=lsBag then
     begin
       New(pInt);
       pInt^:=i;
       aLettersLeft.Add(pInt);
     end;
    s:='';
    for i:=0 to RackSize-1 do
    begin
      aLetter:=RackLetter[FCurrentPlayer,i];
      if ((aLetter=nil) or (aLetter.State=lsChange)) and (aLettersLeft.Count>0) then
      begin
        j:=genrandom(aLettersLeft.Count);
        s:=s+inttostr(j)+',';
        pInt:=aLettersLeft[j];
        with LetterList[pInt^] do
        begin
          State:=lsRack;
          RackPos:=i;
          Who:=FCurrentPlayer;
        end;
        Dispose(pInt);
        aLettersLeft.Delete(j);

        if (aLetter<>nil) then //lsChange
        begin
          FExchangedLetters:=FExchangedLetters+UTF8Encode(widestring(aLetter.What));
          aLetter.State:=lsBag;
          aLetter.RackPos:=255;
          aLetter.Who:=255;
          aLetter.When:=FCurrentMove;
          LastError:=leLetterChanged;
        end;//lsChange
      end;
    end;
    for i:=0 to aLettersLeft.Count-1 do
    begin
      pInt:=aLettersLeft[i];
      Dispose(pInt);
    end;
    aLettersLeft.Clear;
    FLettersLeft:=0;
    for i:=0 to LetterList.Count-1 do
     if LetterList[i].State=lsBag then
      inc(FLettersLeft);
  finally
    aLettersLeft.Free;
  end;
  if FMoveState=msJokerExchanged then LastError:=leLetterChanged;
  inc(FPlayers[FCurrentPlayer].Points,LastMove.Value);

  //set game end
  if not (gsGameEnd in FGameState) then
  begin
    //game end by all letters?
    if (FLettersLeft=0) then
    begin
      include(FGameState,gsGameEnd);
      for i:=0 to RackSize-1 do
        if RackLetter[FCurrentPlayer,i]<>nil then
          exclude(FGameState,gsGameEnd);
    end;
    //game end by passes?
    if (LastError=leNoLetter) or
       ((LastError=leLetterChanged) and (gsChangeIsPass in Scrabble.GameState)) then
    begin
      inc(FNoLettersPlaced);
      FLastNoLettersPlaced:=FNoLettersPlaced;
    end else
    begin
      FNoLettersPlaced:=0;
      if ((FHasChallenged<>crInvalid) and (FPreviousMoveState=msLetterPlaced)) or
         not (gsChangeIsPass in Scrabble.GameState) then
        FLastNoLettersPlaced:=0;
    end;
    if FNoLettersPlaced>=NumberOfPlayers*FNumberOfPasses then
    begin
      FNoLettersPlaced:=NumberOfPlayers*FNumberOfPasses;
      include(FGameState,gsGameEnd);
    end;
  end;

  //update state
  FPreviousMoveState:=FMoveState;
  FMoveState:=msNone;
  mSecUsed:=GetMoveTime_ms;
  //history
  if assigned(FOnSetHistory) then
   FOnSetHistory(LastMove.PlacedWord,
                 LastMove.ConnectedWords,
                 FExchangedLetters,
                 FCambioSeccoLetters,
                 aRackLetters, FCurrentPlayer, FCurrentMove, LastMove.Value, round(mSecUsed/1000), LastMove.IsScrabble,
                 byte(FPlacedDimension.Axis),FPlacedDimension.Position); //not ActiveDimension since it might have been moved
  FExchangedLetters:='';
  FCambioSeccoLetters:='';
  FJokerizeLetter:=gsJokerization in FGameState;
  LeastValue:=0;
  FHasChallenged:=crNone;

  //adjust time
  FPlayers[FCurrentPlayer].mTimeElapsed:=FPlayers[FCurrentPlayer].mTimeElapsed+mSecUsed;

  //move state
  inc(FCurrentMove);
  FActualMove:=FCurrentMove;
  FCurrentPlayer:=FCurrentMove mod NumberOfPlayers;

  if FTimeControlSetting<>tcPerGame then
    FPlayers[FCurrentPlayer].mTimeElapsed:=0;
  exclude(FGameState,gsNextPlayer);

  if (gsPaused in Scrabble.GameState) then
     TogglePaused;

  FMoveTime:=Now;
  FPauseTime:=0;

  //messages etc.
  if //not (gsLoading in FGameState) and
     assigned(FOnAfterNextPlayer) then
    FOnAfterNextPlayer(self);

  //game end bonus calculation elsewhere
  if (gsGameEnd in FGameState) and
     (gsRunning in FGameState) and
     (FWordCheckMode<>wcChallenge) and
     assigned(FOnGameEnd) and
     not (gsLoading in FGameState) then
    FOnGameEnd(self);
end;

{$I sc_ls.inc} //TGame.Load|Save

end.

