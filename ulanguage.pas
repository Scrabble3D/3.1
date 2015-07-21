{ Translation routines and resourcestrings

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

unit ulanguage;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, Forms, Inifiles, StdCtrls, ComCtrls, ExtCtrls,
  Typinfo, FileUtil,   CheckLst, Grids, Buttons, LResources, LazUTF8,
  uTranslations, uVersion;

type
  TLanguageInfo=(liFileName,liAuthor,liVersion,liDate,liLangID,liComment);
  ELaguageError=class(Exception);

  { TLanguage }

  TLanguage=class
    private
      FCurrentLangFile : string;
      FAuthor, FDate, FLangID, FPotDate, FComment,
      FLangTeam, FBugs, FPluralForms  : string;
      FVersion         : integer;
      FBiDiMode        : TBiDiMode;
      FIsPoFile        : boolean;
      function GetInfo(index: TLanguageInfo): string;
      procedure SetLanguage(const aValue: string);
      function HasProp(comp:TComponent;prop:string):boolean;
      function GetProp(Comp:TComponent;Prop:string):string;
      procedure SetProp(comp:TComponent;const prop,value:string);
      procedure ReadFromIni(aValue:string);
      procedure ReadFromPo(aValue:string);
      procedure SaveAsPo(const aFileName:string);
    protected
      OriginalResourceStrings : TStringList;
    public
      constructor Create;
      destructor Destroy;override;
    public
      function Plural(const ResString: string; const Number:integer; const DoFormat:boolean=true): string;
      function Plural(const ResString: string; const aText:string; const DoFormat:boolean=true): string; overload;
      function Format (Const Fmt : String; const Args : Array of const) : String;
      property LangFile:string read FCurrentLangFile write SetLanguage;
      property Info[index:TLanguageInfo]:string read GetInfo;
      property BiDiMode:TBiDiMode read FBiDiMode;
      property IsPoFile: boolean read FIsPoFile;
    end;

var
  Language : TLanguage;

resourcestring
  rMessages_Caption='information';
  rMessages_ChangeFont='Change font';
  rMessages_CopyAll='Copy all messages';
  rMessages_Clear='Clear messages';

  rStat_BestMove='best move';
  rStat_BestGame='best game';
  rStat_NumberOfMoves='number of moves';
  rStat_NumberOfGames='number of games';
  rStat_AverageMove='average move value';
  rStat_AverageGame='average game result';
  rStat_SumTime='overall time on game';
  rStat_AverageTimeMove='average time per move';
  rStat_AverageTimeGame='average time per game';
  rStat_ComputerCaption='Computer';
  rStat_OwnCaption='Local player';
  rStat_GameServer='NSA rating';
  rStat_GameServerName='player name';
  rStat_LeftAxis='Count';
  rStat_BottomAxis='Rating';

  rMain_UpdateLetterset='Current letter set does not match. Updated from dictionary info?';
  rMain_SyncDictionary='Game is played with dictionary %s. Load it now?';
  rMain_WarnDictionary='Dictionaries don''t have same names. Still trying to synchronize...';
  rMain_WarnCategories='Number of categories are different. Synchronzation aborted';
  rMain_AddTimeError1='Time control settings do not allow to add 60 seconds';
  rMain_AddTimeError2='Not enough points to add time';
  rMain_CambioSecco='Do you really want to use the Cambio Secco now?';
  rMain_CambioSeccoUsed='%s utilizes the Cambio Secco.';
  rMain_RecommendCambioSecco='Computer recommends to use Cambio Secco.';
  rMain_InfoAppStarted='Application has been started';
  rMain_StartNewGame='Do you really want to start a new game?';
  rMain_InfoNewGameStarted='New game has been started';
  rMain_LoadDictionary='Please load a dictionary';
  rMain_ErrorUnknownWords='The word %s is not known.|The words %s are unknown.';
  rMain_NotFound='not found';

  rMain_WordInvalid='Challenged move consists of invalid words. A bonus of %d point was added to %s''s result|Challenged move consists of invalid words. A bonus of %d points was added to %s''s result';
  rMain_AllWordsValid='All words are valid. A penalty of %d point was deducted from %s''s result|All words are valid. A penalty of %d points was deducted from %s''s result';
  rMain_GameEndBonus='%s receives %d point bonus.|%s receives %d points bonus.';

  rMain_CalcResult='Only %d valid word found which score with|Out of %d possible words found the best would score with';//+' '+rMain_Points;
  rMain_CalcComp='but computer suggests to place';//=', '+rMainCalcComp+' '+rMain_Points
  rMain_TimePenaltyMsg='One minute has been added to %s time for %d point (%d times left)|One minute has been added to %s time for %d points (%d times left)';
  rMain_ErrorLowScore='Move''s score is below';//+' '+rMain_Points
  rMain_GameEndMinusJoker='%s: %d minus %d, minus %d for jokers =';//+' '+rMain_Points
  rMain_GameEndMinus='%s: %d minus %d =';//+' '+rMain_Points
  rMain_GameEndPlus='%s: %d plus %d =';//+' '+rMain_Points
  rMain_MoveResult='%0:s places %1:s at %2:s and receives';//+' '+rMain_Points+' '+%s+rMain_TimeLeft; "<Scotty> places <ABC> at <A1w> and receives <2 points> (<0:20:00> <left>)."
  rMain_LostByTime='Final time out for %s. Result has been zeroed';//+' ('+rMain_Points+').'
  rMain_Points='%d point|%d points';

  rMain_LettersExchanged='%d letter has been exchanged|%d letters have been exchanged';
  rMain_NumberOfPasses='Only %d pass left until game end|%d passes until game end';
  rMain_GameServerInfo='Game started %s, move #%d';
  rPoll_TimeOut='%d second left before auto close|%d seconds left before auto close';
  rWordSearch_SearchResult='%d word found|%d words found';

  rMain_ReallyPass='Do you really want to pass?';
  rMain_ErrorFirstMove='First move not at the (grey) start point';
  rMain_ErrorDimension='Not in one dimension placed';
  rMain_ErrorConnection='No connection';
  rMain_ErrorSuccessive='Not successively placed';
  rMain_UnknownWords='%s is not found in dictionary. Accept anyway?|%s are not found in dictionary. Accept anyway?';
  rMain_TimeLeft='left';
  rMain_NoLetters='No letters placed';
  rMain_GameEnd='Game end';
  rMain_OwnMove='It''s your turn';
  rMain_GameEndLetterLeft='%s has letters left: %s';
  rMain_GameEndDraw='Draw!';
  rMain_GameEndWin='%s wins the game';
  rMain_CloseNetwork='Really close network?';
  rMain_NetworkDown='No connection to game server for more than one minute!';

  rMain_MenuLoadLocal='Load from...';
  rMain_MenuSaveLocal='Save as...';
  rMain_MenuLoadNetwork='Load remote game...';
  rMain_MenuSaveNetwork='Save local copy...';
  rMain_PieceError1='Not allowed after joker exchange';
  rMain_PieceError2='No placing possible after tiles have been marked for exchange';
  rMain_PieceError3='No tile exchange possible after placing';
  rMain_PieceError4='Not allowed after joker exchange';
  rMain_PieceError5='Not enough letters left';
  rMain_PieceError6='No jokerization after tiles have been marked for exchange';
  rMain_PieceError7='Jokerization is not available';
  rMain_PieceError8='Only one jokerization per move';
  rMain_PieceError9='No exchange allowed after jokerization';
  rMain_PieceError10='You may not jokerize unless its your turn';
  rMain_PieceJokerized='%s jokerized letter %s';
  rMain_DemoPause='Demo has been paused';
  rMain_DemoUnPause='Demo will be continued';
  rMain_NetworkClosed='network closed';
  rMain_Login='%s has been logged in.';
  rMain_Leader='Gameserver welcomes the highscore leader: %s';
  rMain_ChatGroup='group';
  rMain_ChatServer='server';
  rMain_ChatKibitzes='kibitzes';
  rMain_Logout='%s has been logged out.';
  rMain_Join='%s joins the game';
  rMain_InviteKibitz='%s invites you to kibitz.';
  rMain_ReInvite='%s reinvites you to a running game.';
  rMain_InviteGame='%s invites you to a new game.';
  rMain_Leave='%s leaves group';
  rMain_Private='Kibitzed game has been set to private';
  rMain_KickedByAnother='You have been kicked because another player logged in with your name.';
  rMain_ChatShout='%s shouts: %s';
  rMain_ChatSay='%s says: %s';
  rMain_ChatKibitz='%s tattles: %s';
  rMain_ChatWhisper='%s whispers: %s';
  rMain_NetworkStartNewGame='%s wants to start a new game';
  rMain_Invalid='Invalid move:';
  rMain_Exchange2D='%s exchanged joker at %s,%s by %s';
  rMain_Exchange3D='%s exchanged joker at %s,%s,%s by %s';
  rMain_UnknownMessage='Unknown message received:';
  rMain_SBLettersLeft='Letters left';
  rMain_SBTimeLeft='Time left';
  rMain_SBTimePerGame='per game';
  rMain_SBTimePerMove='per move';
  rMain_SBTimeNoLimit='no limit';
  rMain_SBScore='Score';
  rMain_SBStatus='Status of game';
  rMain_SBSettings='Game settings';
  rMain_SBTakebackMode='Word check mode: Takeback';
  rMain_SBPollMode='Word check mode: Poll';
  rMain_SBChallengeMode='Word check mode: Challenge';
  rMain_SBJokerExchange='Placed jokers can be exchanged';
  rMain_SBNoJokerExchange='Placed jokers cannot be exchanged';
  rMain_SBJokerization='Letters can be jokerized';
  rMain_SBNoJokerization='Jokerization is not available';
  rMain_SBPoll='Result of last poll (green->yes, red->no)';
  rMain_SaveGame='Scrabble savegame';
  rMain_AllFiles='All files';

  rMain_OtherDictionary='Game was played with another dictionary: ';
  rMain_LoadSuccess='Savegame %s has been loaded';
  rMain_JokerReplace='Choose letter to insert';
  rMain_GameAnalysis='game analysis';
  rMain_Abort='Abort';
  rMain_Whisper='You are whispering to';
  rMain_UnknownReceiver='Unknown receiver';
  rMain_MessageStored='Recipient is not online. Your message is stored and will be transmitted on next connection';
  rMain_StatSum='Accumulated result:';
  rMain_Chat='Type in message to send to';
  rMain_Joined='joined';
  rMain_HintCountry='Country:';
  rMain_HintCity='City:';
  rMain_HintMenu='Menu lang:';
  rMain_HintRating='Rating:';
  rMain_HintGames='#Games:';
  rMain_HintRegistered='Registered:';
  rMain_HintRelease='Release:';
  rMain_Decline='%s declined';//invite
  rMain_DeclinesGeneral='%s declines'; //poll
  rMain_AutoDeclines='You rejected a poll automatically';
  rMain_InviteLang='Language:';
  rMain_InviteCountry='Country:';
  rMain_InviteDictionary='Dictionary:';
  rMain_MakePrivateF='%s wants to make the game private (no kibitzing allowed). Accept?';
  rMain_MakePublicF='%s wants to make the game public (kibitzes are welcome). Accept?';
  rMain_ResumeF='%s wants to resume a stored game. Accept?';
  rMain_PauseF='%s would like to pause the game. Accept?';
  rMain_UnPauseF='%s would like to resume running game. Accept?';
  rMain_ErrorRules='Rule violation';
  rMain_DemoStop='Stop &demo';
  rMain_DemoRun='Run &demo';
  rMain_WarnRelease='You are using different releases: %s<>%s';
  rMain_LoadGame='Requesting saved games from server...';
  rMain_Challenge='Do you want to challenge last move?';
  rMain_NetworkSettings='Network settings by %s';
  rMain_LoadingFinished='Game has been loaded successfully';

  rCommand_Help1='Commands start with / and have mandatory parameters {} (curly brackets to omit).';
  rCommand_Help2='/whisper {receiver} {message} - Whispers message to receiver. E.g. /whisper Scotty Hello World. '+
                 'If the name of the receiver contains spaces it should be enclosed in double quotes, e.g. /whisper "Miss Liberty" Hello';

  rCommand_Help3='/say {message} - Tells message to current group members. E.g. /say Hello World';
  rCommand_Help4='/shout {message} - Tells message to all players that are connected.';
  rCommand_Help5='/tattle {message} - Tells message to all kibitzes at current group.';
  rCommand_Help6='/finger {player} - Pull information about player.';
  rCommand_Help7='/best - List best players by rating.';
  rCommand_Unknown='Unknown command';
  rCommand_Wrong='Wrong command syntax';
  rUpdaterDic='Dictionaries';
  rUpdaterLocalization='Localization';
  rUpdaterDesign='Designs';
  rUpdaterApplication='Application';

  rOptions_DeletePreset='Really delete the preset %s?';
  rOptions_DeleteDictionary='Really delete the dictionary %s?';
  rOptions_DeleteDesign='Really delete the design %s?';
  rOptions_DeleteLang='Really delete the localization %s?';
  rOptions_ChangeNow='Settings have been changed. Save now in preset %s?';
  rOptions_ChangedWarn='Settings have been changed. Do you want to save it first?';

  rOptions_Update0='on each start';
  rOptions_Update1='daily';
  rOptions_Update2='weekly';
  rOptions_Update3='monthly';
  rOptions_Update4='never';
  rOptions_DictionaryID0='File name:';
  rOptions_DictionaryID1='Author:';
  rOptions_DictionaryID2='Version:';
  rOptions_DictionaryID3='File size:';
  rOptions_DictionaryID4='File date:';
  rOptions_DictionaryID5='Word count:';
  rOptions_DictionaryID6='Used words:';
  rOptions_DictionaryID7='Licence:';
  rOptions_DictionaryID8='Comments:';
  rOptions_DictionaryID9='Categories:';
  rOptions_Points='points';
  rOptions_Letters='letters';
  rOptions_DownloadProceed='Proceed with download of %s?';

  rOptions_Filename='File name:';
  rOptions_Author='Author:';
  rOptions_Date='Date:';
  rOptions_Comment='Comment:';
  rOptions_NavigatorGame='Game';
  rOptions_NavigatorDic='Dictionary';
  rOptions_NavigatorRules='Rules';
  rOptions_NavigatorDesign='Design';
  rOptions_NavigatorLang='Localization';
//  rGameOptions_WordCheckMode='Define how words should be checked\\n* In Takeback mode the player is asked (and can take back) whether an unknown word should be placed\\n* In Poll mode the group is asked about placed words without possibility to take back\\n* In Challenge mode a move can be objected within given period; but if the move was valid,\\nthe specified penalty will be substracted from the objecter''s value';

  rOptions_Yes='yes';
  rOptions_No='no';
  rOptions_NoPenalty='no penalty';

  rSelectColor='Select color for';
  rFieldNormal='normal field';
  rFieldDoubleLetter='double letter';
  rFieldTripleLetter='triple letter';
  rFieldQuadLetter='quad letter';
  rFieldDoubleWord='double word';
  rFieldTripleWord='triple word';
  rFieldQuadWord='quad word';
  rFieldStart='start field';
  rFieldPlacedLetter='placed letter';
  rFieldNewLetter='new letter';
  rFieldSingleMalus='single letter malus';
  rFieldDoubleMalus='double letter malus';
  rFieldTripleMalus='triple letter malus';
  rFieldQuadMalus='quad letter malus';

  rMessage_Error='Don''t popup Errors';
  rMessage_Warning='Don''t popup Warnings';
  rMessage_Information='Don''t popup Informations';
  rMessage_Chat='Don''t popup Chat';
  rMessage_OwnMove='Don''t popup Own-Move';
  rMessage_GameResult='Don''t popup Gameresult';
  rMessage_Debug='Don''t popup Debug';

  rNetwork_ErrorPassword='Please specify a password.';
  rNetwork_ErrorName='Please specify a name';
  rNetwork_ErrorServer='Please select a server to connect to';
  rNetwork_Challenge='%s challenges last move. Placed words are validated...';
  rNetWort_OwnRating='Own rating';
  rNetWort_OwnRank='Own rank: %s of %s';

//  rNewGame_InfoDictionary='dictionary:';
  rNewGame_InfoVersion='Version:';
//  rNewGame_InfoTournament='tournament rules:';
//  rNewGame_InfoLetterset='letter set:';
//  rNewGame_InfoBoard='board:';
  rNewGame_InfoPieces='#Pieces:';
  rNewGame_InfoJokers='#Jokers:';
  rNewGame_InfoRandoms='#Randoms: ';
//  rNewGame_InfoWordCheck='word check:';

  rWordCheck_Ask='Takeback';
  rWordCheck_Poll='Poll';
  rWordCheck_Challenge='Challenge';
  rNewGame_ErrorHuman='At least one human partner should be specified';
  rNewGame_Caption='New Game';
  rNewGame_LoadConfig='Do you want to load %s now?';

  rPoll_Caption='Poll';
  rPoll_Player1='1st player';
  rPoll_Player2='2nd player';
  rPoll_Player3='3rd player';
  rPoll_Player4='4th player';
  rPoll_CheckBox='auto close after poll';

//  rWordSearch_MissingChar='At least one character is not found in letter set. Please check options.';
  rWordSearch_MissingCharEx='The character %s is not part of the current letter set.';

  rGameCourse_Col0='#';
  rGameCourse_Col1='Word';
  rGameCourse_Col2='Value';
  rGameCourse_Col3='Best';
  rGameCourse_Col4='Time';
  rGameCourse_Hint='Click on the header to sort by column';
  rGameCourse_Menu1='Clear best values';

  rScrabbleGrid_SingleLetterScore='single letter score';
  rScrabbleGrid_DoubleLetterScore='double letter score';
  rScrabbleGrid_TripleLetterScore='triple letter score';
  rScrabbleGrid_QuadLetterScore='quad letter score';
  rScrabbleGrid_DoubleWordScore='double word score';
  rScrabbleGrid_TripleWordScore='triple word score';
  rScrabbleGrid_QuadWordScore='quad word score';

  rDictionary_Save='Dictionary has been changed. Save now?';
  rDictionary_Loading='loading dictionary %s';
  rDictionary_Unassigned='unassigned';

  rUpdater_NewRelease='New release for %s %s found: %s<%s. Download now?';
  rUpdater_AllUpToData='all files are up to date';

  rUpdater_InfoError='File has been downloaded but release info is incorrect for %s';
  rUpdater_DownloadError='Error downloading';
  rUpdater_DownloadRunning='Downloading %s ...';
  rUpdater_DownloadFinished='Download of %s finished';

  rBruteforce_NoValidWords='No valid words found';

  rWelcome_Step1='Please select first your preferred language:';
  rWelcome_Step2='Please select the dictionary you want to use:';
  rWelcome_Step3='Select your prefered Scrabble settings (letterset and rules will be applied):';
  rWelcome_Step4='Your first configuration steps have been done. Please check options for additional settings.';
  rWelcome_Next1='&Next';
  rWelcome_Next2='&Ok';

  rDiscoConnections='Too many connections';            //'Error_Connections'
  rDiscoRelease='Please update your application';      //'Error_Release'
  rDiscoName='Please choose another name';             //'Error_Name'
  rDiscoBanned='You are banned on this server';        //'Error_Banned'
  rDiscoPassword='Wrong username or password';         //'Error_Password'
  rDiscoAccounts='Too many accounts';                  //'Error_Accounts'

implementation

uses
  utypes, uconfig;

const
  cLangError='Format settings do not match in "';
  cLineBreak='\n';

{ ---------- from pocheckermain --------------}
function ExtractFormatArgs(S: String; out ArgumentError: boolean): String;
const
  FormatArgs = 'DEFGMNPSUX';
  FormatChar = '%';
  FormatSpecs = ':-.0123456789';
var
  p: PtrInt;
  NewStr, Symb: String;
begin
  NewStr := '';
  ArgumentError := false;
  p := UTF8Pos(FormatChar, S);
  while (Length(S)>0) and (p>0) do
  begin
    UTF8Delete(S, 1, p);
    if Length(S)>0 then
    begin
      Symb := UTF8UpperCase(UTF8Copy(S, 1, 1));
      while (Length(S)>1) and (UTF8Pos(Symb, FormatSpecs)>0) do
      begin
        //weak syntax check for formatting options, skip them if found
        UTF8Delete(S, 1, 1);
        Symb := UTF8UpperCase(UTF8Copy(S, 1, 1));
      end;
      if Symb <> FormatChar then
      begin
        NewStr := NewStr+Symb;
        if UTF8Pos(Symb, FormatArgs)=0 then
          ArgumentError := true;
      end;
      //removing processed symbol
      UTF8Delete(S, 1, 1);
      //searching for next argument
      p := UTF8Pos(FormatChar, S);
    end
    else
      //in this case formatting symbol doesn't have its argument
      ArgumentError := true;
  end;
  Result := NewStr;
end;

function CompareFormatArgs(S1, S2: String): Boolean;
var
  ArgErr1, ArgErr2: boolean;
  p: PtrInt;
begin
  Result := true;
  exit;

  if s1=' %' then s1:='';
  if s2=' %' then s2:='';
  if s1='%' then s1:='';
  if s2='%' then s2:='';
  p := UTF8Pos('[%]', S1);
  if p>0 then
    UTF8Delete(S1, p, 3);
  p := UTF8Pos('[%]', S2);
  if p>0 then
    UTF8Delete(S2, p, 3);

  //do not check arguments if strings are equal to save time and avoid some
  //false positives, e.g. for '{%Region}' string in lazarusidestrconsts
  if S1 <> S2 then
  begin
    Result := CompareText(ExtractFormatArgs(S1, ArgErr1), ExtractFormatArgs(S2, ArgErr2)) = 0;
    //setting result to false if invalid arguments were found even if the match
    Result := Result and not ArgErr1 and not ArgErr2;
  end;
end;
{ ---------- /from pocheckermain --------------}

constructor TLanguage.Create;
begin
  inherited Create;
  FCurrentLangFile:='';//ConfigPath+'default.lang';
  OriginalResourceStrings:=TStringList.Create;
  FIsPoFile:=false;
  FAuthor:='undefined';
  FDate:=DateToStr(Now);
  FPotDate:=FDate;
  FLangID:='en_GB';
  FLangTeam:='undefined';
  FBugs:='undefined';
  FPluralForms:='nplurals=2; plural=n!=1;';
  FVersion:=100000;
end;

destructor TLanguage.Destroy;
begin
  OriginalResourceStrings.Free;
  inherited Destroy;
end;

function SaveResource(Name,Value : AnsiString; Hash : Longint; arg:pointer) : String;
var
  i:integer;
  s:string;
  sl:TStringList;
begin
  Value:=StringReplace(Value,LineBreak,cLineBreak,[rfIgnoreCase, rfReplaceAll]);
  //if OS specific wrap is different
  Value:=StringReplace(Value,LineEnding,cLineBreak,[rfIgnoreCase, rfReplaceAll]);//#10 on linux, #13 macos
  Value:=StringReplace(Value,'"','\"',[rfIgnoreCase, rfReplaceAll]);
  //store original values
  if Language.LangFile='' then
   with Language.OriginalResourceStrings do
    Values[Name]:=Value;
  s:=Language.OriginalResourceStrings.Values[Name];
  //save
  with TStringList(arg) do
  begin
    Add('#: '+Name);
    Add('msgctxt "'+Name+'"');
    i:=pos('|',Value);
    if i>0 then
    begin
      sl:=TStringList.Create;
      try
        sl.Delimiter:='|';
        sl.StrictDelimiter:=true;
        sl.DelimitedText:=Value;
        Add('msgid "'+sl.Strings[0]+'"');
        if sl.Count>1 then
          Add('msgid_plural "'+sl.Strings[1]+'"');
        if s<>Value then
        begin
          sl.DelimitedText:=s;
          for i:=0 to sl.Count-1 do
            Add('msgstr['+inttostr(i)+'] "'+sl.Strings[i]+'"');
        end else
        begin
          Add('msgstr[0] ""');
          Add('msgstr[1] ""');
        end;
      finally
        sl.Free;
      end;
    end else
    begin
      Add('msgid "'+s+'"');
      if s<>Value then
        Add('msgstr "'+Value+'"') else
        Add('msgstr ""');
    end;
    Add('');
  end;
  Result:=Value;
end;

procedure TLanguage.SaveAsPo(const aFileName:string);
var
  aParentComp,aChildComp : TComponent;
  i,j,k: integer;
  s : string;
  sl:TStringList;
begin
  sl:=TStringList.Create;
  try
    sl.Add('msgid ""');
    sl.Add('msgstr ""');

    sl.Add('"Project-Id-Version: '+inttostr(FVersion)+'\n"');
    sl.Add('"Report-Msgid-Bugs-To: '+FBugs+'\n"');
    sl.Add('"POT-Creation-Date: '+FPotDate+'\n"');
    sl.Add('"PO-Revision-Date: '+FDate+'\n"');
    sl.Add('"Last-Translator: '+FAuthor+'\n"');
    sl.Add('"Language-Team: '+FLangTeam+'\n"');
    sl.Add('"Language: '+FLangID+'\n"');
    sl.Add('"MIME-Version: 1.0\n"');
    sl.Add('"Content-Type: text/plain; charset=UTF-8\n"');
    sl.Add('"Content-Transfer-Encoding: 8bit\n"');
    sl.Add('"Plural-Forms: '+FPluralForms+'\n"');
    sl.Add('"X-Generator: Scrabble3D\n"');
    sl.Add('"Comment: '+FComment+'\n"');
    if FBiDiMode=bdRightToLeft then
      sl.Add('"BiDiMode: RightToLeft\n"') else
      sl.Add('"BiDiMode: LeftToRight\n"');

    for i:=0 to Application.ComponentCount-1 do
    begin
      aParentComp:=Application.Components[i];
      if HasProp(aParentComp,'Caption') then
      begin
        s:=getProp(aParentComp,'Caption');
        SaveResource(aParentComp.Name+'.Caption',s,-1,sl);
      end;

      for j:=0 to aParentComp.ComponentCount-1 do
      begin
        aChildComp:=aParentComp.Components[j];
        if aChildComp is TStaticText then
          Continue;

        if HasProp(aChildComp,'Caption') and
           not (getProp(aChildComp,'Caption')='-') and
           not (getProp(aChildComp,'Caption')='') then
        begin
          s:=getProp(aChildComp,'Caption');
          SaveResource(aChildComp.Name+'.Caption',s,-1,sl);
        end;

        if HasProp(aChildComp,'Hint') and
           (GetProp(aChildComp,'Hint')<>'') then
        begin
          s:=getProp(aChildComp,'Hint');
          SaveResource(aChildComp.Name+'.Hint',s,-1,sl);
        end;

        if (aChildComp is TRadioGroup) then
         for k:=0 to TRadioGroup(aChildComp).Items.Count-1 do
         begin
           s:=(aChildComp as TRadioGroup).Items[k];
           SaveResource(aChildComp.Name+'.Items['+IntToStr(k)+']',s,-1,sl);
         end;

        if (aChildComp is TTreeView) then
         for k:=0 to TTreeView(aChildComp).Items.Count-1 do
         begin
           s:=(aChildComp as TTreeView).Items[k].Text;
           SaveResource(aChildComp.Name+'.Items['+IntToStr(k)+']',s,-1,sl);
         end;

        if (aChildComp is TCheckListBox) then
         for k:=0 to TCheckListBox(aChildComp).Items.Count-1 do
         begin
           s:=(aChildComp as TCheckListBox).Items[k];
           SaveResource(aChildComp.Name+'.Items['+IntToStr(k)+']',s,-1,sl);
         end;

        if (aChildComp is TStringGrid) then
         for k:=0 to TStringGrid(aChildComp).Columns.Count-1 do
         begin
           s:=(aChildComp as TStringGrid).Columns[k].Title.Caption;
           SaveResource(aChildComp.Name+'.Column['+IntToStr(k)+']',s,-1,sl);
         end;
      end; //for j
    end; //for i
    SetUnitResourceStrings('ulanguage',@SaveResource, sl);
    sl.SaveToFile(UTF8ToSys(Config.Path+aFileName));
  finally
    sl.Free;
  end;
end;

function TLanguage.HasProp(comp: TComponent; prop: string): boolean;
begin
  Result:=(GetPropInfo(Comp.classInfo,Prop)<>nil) and (Comp.Name<>'');
end;

function TLanguage.GetProp(Comp: TComponent; Prop: string): string;
var
  pi:PPropInfo;
begin
  pi:=GetPropInfo(Comp.ClassInfo,Prop);
  if pi<>nil then Result:=GetStrProp(Comp,pi)
             else Result:='';
end;

procedure TLanguage.SetProp(comp: TComponent; const prop, value: string);
var
  pi:PPropInfo;
begin
  if Value<>'' then
  begin
    pi:=GetPropInfo(Comp.ClassInfo,Prop);
    if pi<>nil then SetStrProp(Comp,pi,Value);
  end;
end;

function TranslateResource(Name,Value : AnsiString; Hash : Longint; arg:pointer) : String;
var
  org,trans:string;
  aIndex,i:integer;
  aItem:TPOFileItem;
begin
  if TObject(arg) is TIniFile then
  begin
    org:=copy(Name,11,length(Name));
    trans:=TInifile(arg).ReadString('Translation',org,'');
    if Result{%H-}='' then
    begin
      TInifile(arg).WriteString('Translation',StringReplace(org, LineBreak, cLineBreak, [rfIgnoreCase, rfReplaceAll]),Value);
      Result:=Value;
    end;
    if not CompareFormatArgs(org,trans) then
      raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
    trans:=StringReplace(trans,'\\n', cLineBreak, [rfIgnoreCase, rfReplaceAll]);
    i:=Language.OriginalResourceStrings.IndexOfName(name);
    if i>-1 then
      Language.OriginalResourceStrings.ValueFromIndex[i]:=trans;
    Result:=StringReplace(trans,cLineBreak, LineBreak, [rfIgnoreCase, rfReplaceAll]);
  end else
  begin
    with Language.OriginalResourceStrings do
    begin
//      TPoFile(arg).ItemByName[Name].Comments:='# Obsolet' else
      aIndex:=IndexOfName(Name);
      aItem:=TPoFile(arg).ItemByName[Name];
      if aItem<>nil then
      begin
        org:=aItem.Original;
        if (aIndex>=0) and (ValueFromIndex[aIndex]<>org) then
          TPoFile(arg).ItemByName[Name].Comments:='# Changed: '+ValueFromIndex[aIndex];
      end;
    end;
    trans:=TPoFile(arg).Translate(Name, Value);
    if not CompareFormatArgs(org,trans) then
      raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
    Result:=StringReplace(trans,cLineBreak, LineBreak, [rfIgnoreCase, rfReplaceAll]);
  end;
end;

procedure TLanguage.ReadFromIni(aValue: string);
var
  i,j,k : integer;
  org, trans : string;
  aParentComp,aChildComp : TComponent;
  aIniFile : TIniFile;
begin
  FIsPoFile:=false;
  aIniFile:=TIniFile.Create(UTF8ToSys(aValue));
  with aIniFile do
  try
    FCurrentLangFile:=aValue;

    FVersion:=ReadInteger('Header','Version',301000);
    FBugs:='undefined';
    FDate:=ReadString('Header','Date',DateToStr(Now));
    FPotDate:=FDate;
    FAuthor:=ReadString('Header','Author','Scrabble3D');
    FLangTeam:='none';
    FLangID:=ReadString('Header','LangID','en_GB');
    FPluralForms:='';
    if ReadString('Header','BiDiMode','LeftToRight')='RightToLeft' then
      FBiDiMode:=bdRightToLeft else
      FBiDiMode:=bdLeftToRight;

    for i:=0 to Application.ComponentCount-1 do
    begin
      aParentComp:=Application.Components[i];

      if HasProp(aParentComp,'Caption') then
      begin
        org:=GetProp(aParentComp,'Caption');
        trans:=ReadString('Translation',aParentComp.Name+'.Caption','');
        trans:=StringReplace(trans,'\\n', cLineBreak, [rfIgnoreCase, rfReplaceAll]);
        if trans='' then
        begin
          trans:=org;
          WriteString('Translation',aParentComp.Name+'.Caption',trans);
        end;
        if not CompareFormatArgs(org,trans) then
          raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
        SetProp(aParentComp,'Caption',trans);
      end;

      for j:=0 to aParentComp.ComponentCount-1 do
      begin
        aChildComp:=aParentComp.Components[j];

        if HasProp(aChildComp,'Caption') and
           not (getProp(aChildComp,'Caption')='-') and
           not (getProp(aChildComp,'Caption')='') then
        begin
          org:=GetProp(aChildComp,'Caption');
          trans:=ReadString('Translation',aChildComp.Name+'.Caption', '');
          trans:=StringReplace(trans,'\\n', cLineBreak, [rfIgnoreCase, rfReplaceAll]);
          if trans='' then
          begin
            trans:=org;
            WriteString('Translation',aChildComp.Name+'.Caption', trans);
          end;
          if not CompareFormatArgs(org,trans) then
            raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
          SetProp(aChildComp,'Caption',trans);
        end;

        if HasProp(aChildComp,'Hint') and
           (GetProp(aChildComp,'Hint')<>'') then
        begin
          org:=GetProp(aChildComp,'Hint');
          trans:=ReadString('Translation',aChildComp.Name+'.Hint','');
          trans:=StringReplace(trans,'\\n', cLineBreak, [rfIgnoreCase, rfReplaceAll]);
          if trans='' then
          begin
            trans:=org;
            trans:=StringReplace(trans, LineBreak, cLineBreak, [rfIgnoreCase, rfReplaceAll]);
            WriteString('Translation',aChildComp.Name+'.Hint',trans);
          end;
          if not CompareFormatArgs(org,trans) then
            raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
          trans:=StringReplace(trans, cLineBreak, LineBreak, [rfIgnoreCase, rfReplaceAll]);
          SetProp(aChildComp,'Hint',trans);
        end;

        if (aChildComp is TTreeView) then
         for k:=0 to TTreeView(aChildComp).Items.Count-1 do
         begin
           org:=TTreeView(aChildComp).Items[k].Text;
           trans:=ReadString('Translation',aChildComp.Name+'.Items['+IntToStr(k)+']','');
           trans:=StringReplace(trans,'\\n', cLineBreak, [rfIgnoreCase, rfReplaceAll]);
           if trans='' then
           begin
             trans:=org;
             WriteString('Translation',aChildComp.Name+'.Items['+IntToStr(k)+']',trans);
           end;
           if not CompareFormatArgs(org,trans) then
             raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
           (aChildComp as TTreeView).Items[k].Text:=trans;
         end;

        if (aChildComp is TRadioGroup) then
         for k:=0 to TRadioGroup(aChildComp).Items.Count-1 do
         begin
           org:=TRadioGroup(aChildComp).Items[k];
           trans:=ReadString('Translation',aChildComp.Name+'.Items['+IntToStr(k)+']','');
           trans:=StringReplace(trans,'\\n', cLineBreak, [rfIgnoreCase, rfReplaceAll]);
           if trans='' then
           begin
             trans:=org;
             WriteString('Translation',aChildComp.Name+'.Items['+IntToStr(k)+']',trans);
           end;
           if not CompareFormatArgs(org,trans) then
             raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
           (aChildComp as TRadioGroup).Items[k]:=trans;
         end;

        if (aChildComp is TCheckListBox) then
         for k:=0 to TCheckListBox(aChildComp).Items.Count-1 do
         begin
           org:=TCheckListBox(aChildComp).Items[k];
           trans:=ReadString('Translation',aChildComp.Name+'.Items['+IntToStr(k)+']','');
           trans:=StringReplace(trans,'\\n', cLineBreak, [rfIgnoreCase, rfReplaceAll]);
           if trans='' then
           begin
             trans:=org;
             WriteString('Translation',aChildComp.Name+'.Items['+IntToStr(k)+']',trans);
           end;
           if not CompareFormatArgs(org,trans) then
             raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
           (aChildComp as TCheckListBox).Items[k]:=trans;
         end;

        if (aChildComp is TStringGrid) then
         for k:=0 to TStringGrid(aChildComp).Columns.Count-1 do
         begin
           org:=TStringGrid(aChildComp).Columns[k].Title.Caption;
           trans:=ReadString('Translation',aChildComp.Name+'.Column['+IntToStr(k)+']','');
           trans:=StringReplace(trans,'\\n', cLineBreak, [rfIgnoreCase, rfReplaceAll]);
           if trans='' then
           begin
             trans:=org;
             WriteString('Translation',aChildComp.Name+'.Column['+IntToStr(k)+']',trans);
           end;
           if not CompareFormatArgs(org,trans) then
             raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
           (aChildComp as TStringGrid).Columns[k].Title.Caption:=trans;
         end;

      end; //for j
     end;//for i
    //translate resourcestrings
    SetUnitResourceStrings('ulanguage',@TranslateResource,aIniFile);
  finally
    aIniFile.Free;
  end;
end;

procedure TLanguage.ReadFromPo(aValue: string);
var
  i,j,k : integer;
  org,trans : string;
  aParentComp,aChildComp : TComponent;
  aPoFile : TPoFile;
begin
  FIsPoFile:=true;
  aPoFile:=TPoFile.Create(aValue,true);  //UTF8toSys already in Create
  try

    FCurrentLangFile:=aValue;
    //header info
    FVersion:=StrToIntDef(aPoFile.HeaderValue['Project-Id-Version: '],0);;
    FBugs:=aPoFile.HeaderValue['Report-Msgid-Bugs-To: '];
    FPotDate:=aPoFile.HeaderValue['POT-Creation-Date: '];
    FDate:=aPoFile.HeaderValue['PO-Revision-Date: '];
    FAuthor:=aPoFile.HeaderValue['Last-Translator: '];
    FLangTeam:=aPoFile.HeaderValue['Language-Team: '];
    FLangID:=aPoFile.HeaderValue['Language: '];
    FPluralForms:=aPoFile.HeaderValue['Plural-Forms: '];
    FComment:=aPoFile.HeaderValue['Comment: '];
    if aPoFile.HeaderValue['BiDiMode: ']='RightToLeft' then
      FBiDiMode:=bdRightToLeft else
      FBiDiMode:=bdLeftToRight;
    //content
    for i:=0 to Application.ComponentCount-1 do
    begin
      aParentComp:=Application.Components[i];

      if HasProp(aParentComp,'Caption') then
      begin
        org:=getProp(aParentComp,'Caption');
        trans:=aPoFile.Translate(aParentComp.Name+'.Caption',org);
        if not CompareFormatArgs(org,trans) then
          raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
        SetProp(aParentComp,'Caption',trans);
      end;

      for j:=0 to aParentComp.ComponentCount-1 do
      begin
        aChildComp:=aParentComp.Components[j];

        if HasProp(aChildComp,'Caption') and
           not (getProp(aChildComp,'Caption')='-') and
           not (getProp(aChildComp,'Caption')='') then
        begin
          org:=getProp(aChildComp,'Caption');
          trans:=aPoFile.Translate(aChildComp.Name+'.Caption',org);
          if not CompareFormatArgs(org,trans) then
            raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
          SetProp(aChildComp,'Caption',trans);
        end;

        if HasProp(aChildComp,'Hint') and
           (GetProp(aChildComp,'Hint')<>'') then
        begin
          org:=getProp(aChildComp,'Hint');
          trans:=aPoFile.Translate(aChildComp.Name+'.Hint',org);
          if not CompareFormatArgs(org,trans) then
            raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
          trans:=StringReplace(trans, cLineBreak, LineBreak, [rfIgnoreCase, rfReplaceAll]);
          SetProp(aChildComp,'Hint',trans);
        end;

        if (aChildComp is TTreeView) then
         for k:=0 to TTreeView(aChildComp).Items.Count-1 do
         begin
           org:=(aChildComp as TTreeView).Items[k].Text;
           trans:=aPoFile.Translate(aChildComp.Name+'.Items['+IntToStr(k)+']',org);
           if not CompareFormatArgs(org,trans) then
             raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
           if trans<>'' then
             (aChildComp as TTreeView).Items[k].Text:=trans;
         end;

        if (aChildComp is TRadioGroup) then
         for k:=0 to TRadioGroup(aChildComp).Items.Count-1 do
         begin
           org:=(aChildComp as TRadioGroup).Items[k];
           trans:=aPoFile.Translate(aChildComp.Name+'.Items['+IntToStr(k)+']',org);
           if not CompareFormatArgs(org,trans) then
             raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
           if trans<>'' then
             (aChildComp as TRadioGroup).Items[k]:=trans;
         end;

        if (aChildComp is TCheckListBox) then
         for k:=0 to TCheckListBox(aChildComp).Items.Count-1 do
         begin
           org:=(aChildComp as TCheckListBox).Items[k];
           trans:=aPoFile.Translate(aChildComp.Name+'.Items['+IntToStr(k)+']',org);
           if not CompareFormatArgs(org,trans) then
             raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
           if trans<>'' then
             (aChildComp as TCheckListBox).Items[k]:=trans;
         end;

        if (aChildComp is TStringGrid) then
         for k:=0 to TStringGrid(aChildComp).Columns.Count-1 do
         begin
           org:=(aChildComp as TStringGrid).Columns[k].Title.Caption;
           trans:=aPoFile.Translate(aChildComp.Name+'.Column['+IntToStr(k)+']',org);
           if not CompareFormatArgs(org,trans) then
             raise ELaguageError.Create(cLangError+org+'<>'+trans+'"');
           if trans<>'' then
             (aChildComp as TStringGrid).Columns[k].Title.Caption:=trans;
         end;
      end; //for j
    end;//for i

    SetUnitResourceStrings('ulanguage',@TranslateResource,aPoFile);
    aPoFile.SaveToFile(aValue);  //UTF8ToSys already in save

{    with TStringList.Create do
    try
      for i:=0 to aPoFile.Items.Count-1 do
       with TPOFileItem(aPoFile.Items[i]) do
        Add(IdentifierLow+','+
            Original+','+
            Translation+','+
            Comments);
      SaveToFile('test.txt');
    finally
      Free;
    end;
}
  finally
    aPoFile.Free;
  end;

end;

function TLanguage.Plural(const ResString: string; const Number: integer; const DoFormat:boolean=true): string;
begin
  Result:=PluralForm(ResString,Number,FLangID);
  if DoFormat then
  try
    if pos('%s',Result)>0 then
      Result:=Format(Result,[IntToStr(Number)]) else
    if pos('%d',Result)>0 then
      Result:=Format(Result,[Number]);
  except
    on E:Exception do OnMessage(smError,'Wrong format for ResString');
  end;
end;

function TLanguage.Plural(const ResString: string; const aText: string;const DoFormat: boolean): string;
begin
  with TStringList.Create do
  try
    StrictDelimiter:=true;
    CommaText:=aText;
    Result:=Plural(ResString,Count,DoFormat);
  finally
    Free;
  end;
end;

function TLanguage.Format(const Fmt: String; const Args: array of const): String;
begin
  try
    Result:=SysUtils.Format(fmt,args);
  except //catch malformated args
    on E:Exception do
      OnMessage(smDebug,E.Message);
  end;
end;

procedure TLanguage.SetLanguage(const aValue: string);
var
  aIsIniFile:boolean;
begin
{  {$Note Do not save default.lang}
  if FCurrentLangFile='' then
    SaveAsPo('default.lang');
}  if (aValue<>FCurrentLangFile) and
     FileExists(UTF8ToSys(aValue)) then
  begin
    //convert legacy lang to po
    with TIniFile.Create(UTF8ToSys(aValue)) do
    try
      aIsIniFile:=SectionExists('Translation');
    finally
      Free;
    end;
    if aIsIniFile then
      ReadFromIni(aValue) else
      ReadFromPo(aValue);
  end;
end;

function TLanguage.GetInfo(index: TLanguageInfo): string;
begin
  case index of
   liFileName: Result:=ExtractFileName(FCurrentLangFile);
   liAuthor  : Result:=FAuthor;
   liVersion : Result:=VersionToString(FVersion);
   liDate    : Result:=FDate;
   liLangID  : Result:=FLangID;
   liComment : Result:=FComment;
   else Result:='';
  end;
end;

initialization
  Language:=TLanguage.Create;
finalization
  Language.Free;
end.

