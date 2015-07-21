// ------ network stuff -------
procedure TfmMain.DoTCPClientTerminate(Sender: TObject);
begin
  BruteForce.Aborted:=true;
  if gsPaused in Scrabble.GameState then
    Scrabble.TogglePaused;
  IsGameServer:=false;
  fmNewGame.NetworkMode:=false; //enable player names
  tsGameServer.TabVisible:=false;
  pcMessages.ActivePage:=tsMessages;
  Scrabble.Reset; //clear history to avoid cheats by browsing trough
  GameCourse.Clear;
  tvGameServer.Items.Clear;
  Poll.Free;
  fmWordSearch.Mode:=wmAll;
  if assigned(OnMessage) then
    OnMessage(smInformation,rMain_NetworkClosed);
  if not (gsDestroying in Scrabble.GameState) then //click on exit
    fmGameOptions.Configuration:=Config.Read('General/LastPreset','');
  Screen.Cursor:=crDefault;
  Scrabble.GameState:=Scrabble.GameState+[gsGameEnd];
  Scrabble.GameState:=Scrabble.GameState-[gsNetwork,gsKibitz,gsRunning];
  miLoad.Caption:=rMain_MenuLoadLocal;
  miSave.Caption:=rMain_MenuSaveLocal;
end;

procedure TfmMain.DoTCPClientReceive(aMsg:TNetworkMessage);
 procedure DoDisconnect;
 var
   s:string;
 begin
   s:=aMsg.Value('Message','');
   if assigned(OnMessage) then
   case s of
    'Error_Connections':OnMessage(smError,rDiscoConnections);
    'Error_Release':OnMessage(smError,rDiscoRelease);
    'Error_Name':OnMessage(smError,rDiscoName);
    'Error_Banned':OnMessage(smError,rDiscoBanned);
    'Error_Password':OnMessage(smError,rDiscoPassword);
    'Error_Accounts':OnMessage(smError,rDiscoAccounts);
    else OnMessage(smError,s);
   end;//case
   acNetworkExecute(nil);
 end;
 procedure DoLogin;
 var
   i:integer;
   aClientInfo:TClientInfo;
   aPlayerName,s:string;
 begin
   OnMessage(smInformation,Language.Format(rMain_Login,[aMsg.Sender]));
   if aMsg.Value('Best',false) then
     OnMessage(smInformation,Language.Format(rMain_Leader,[aMsg.Sender]));

   //add static info
   for i:=0 to aMsg.Value('Count',0)-1 do
   begin
     aPlayerName:=aMsg.Value(inttostr(i)+'_'+'Name','unknown');
     if aPlayerName<>'unknown' then
     begin
       aClientInfo:=TCPClient.Players[aPlayerName];
       with aClientInfo do
       begin
         MenuLang:=aMsg.Value(inttostr(i)+'_'+'MenuLang','gb');
         Country:=aMsg.Value(inttostr(i)+'_'+'Country','gb');
         City:=aMsg.Value(inttostr(i)+'_'+'City','');
         Release:=aMsg.Value(inttostr(i)+'_'+'Release','unknown');
         Registered:=aMsg.Value(inttostr(i)+'_'+'Registered',Now);
       end;
       TCPClient.Players[aPlayerName]:=aClientInfo;
       if cbChatReceiver.Items.IndexOf(aPlayerName)=-1 then
         cbChatReceiver.Items.Add(aPlayerName);
     end;
   end;

   if not IsGameServer then
     Scrabble.Reset;

   //self login
   if aMsg.Sender=TCPClient.PlayerData.PlayerName then
   begin
     TCPClient.TimeToAfk:=aMsg.Value('TimeToAfk',60000);
     s:=aMsg.Value('News','');
     if s<>'' then
       OnMessage(smInformation,'Server news: '+s);
     i:=0;
     repeat
       s:=aMsg.Value('Chat'+inttostr(i),'');
       if s<>'' then
         OnMessage(smChat,s);
       inc(i);
     until s='';
     Scrabble.Reset;
     //push dynamic info to all
     TCPClient.OnSend('nwRefresh','all','');
   end;
 end;
 procedure DoRefresh;
 var
   i:integer;
   aPlayerName:string;
   aClientInfo:TClientInfo;
 begin
   for i:=0 to aMsg.Value('Count',0)-1 do
   begin
     aPlayerName:=aMsg.Value(inttostr(i)+'_'+'Name','');
     if aPlayerName<>'' then
     begin
       aClientInfo:=TCPClient.Players[aPlayerName];
       with aClientInfo do
       begin
         Rating:=aMsg.Value(inttostr(i)+'_'+'Rating',0);
         GroupID:=aMsg.Value(inttostr(i)+'_'+'Group',0);
         GameID:=aMsg.Value(inttostr(i)+'_'+'GameID',0);
         GamesPlayed:=aMsg.Value(inttostr(i)+'_'+'GamesPlayed',0);
         IsKibitz:=aMsg.Value(inttostr(i)+'_'+'IsKibitz',false);
         GameStarted:=aMsg.Value(inttostr(i)+'_'+'GameStarted',0.01);
         MoveNumber:=aMsg.Value(inttostr(i)+'_'+'MoveNumber',-1);
         AllowKibitz:=aMsg.Value(inttostr(i)+'_'+'AllowKibitz',false);
         ClientState:=StringToClientState(aMsg.Value(inttostr(i)+'_'+'ClientState','csOpenForGames'));
       end;
       TCPClient.Players[aPlayerName]:=aClientInfo;
     end;
   end;
   TCPClient.UpdateMates;
   UpdateGameServer;
   if TCPClient.Players[TCPClient.PlayerData.PlayerName].IsKibitz then
     Scrabble.GameState:=Scrabble.GameState+[gsKibitz] else
     Scrabble.GameState:=Scrabble.GameState-[gsKibitz];
 end;
 procedure DoChat;
 begin
   //text
   case aMsg.Receiver of
    'all'     : OnMessage(smChat,Language.Format(rMain_ChatShout,[aMsg.Sender,aMsg.Value('Chat','')]),aMsg.Sender);
    'kibitzes': OnMessage(smChat,Language.Format(rMain_ChatKibitz,[aMsg.Sender,aMsg.Value('Chat','')]),aMsg.Sender);
    'group'   : OnMessage(smChat,Language.Format(rMain_ChatSay,[aMsg.Sender,aMsg.Value('Chat','')]),aMsg.Sender);
    else
      OnMessage(smChat,Language.Format(rMain_ChatWhisper,[aMsg.Sender,aMsg.Value('Chat','')]),aMsg.Sender)
   end;//case
   //sound
   if aMsg.Sender<>TCPClient.PlayerData.PlayerName then
     ScrabblePlaySound(smChat);
 end;
 procedure DoInformation;
 var
   i:integer;
 begin
   case aMsg.Value('Info','') of
    'finger','best':OnMessage(smInformation,aMsg.Value('Result',''));
    'ChatStored': OnMessage(smWarning,rMain_MessageStored);
    'ChatUnknownReceiver': OnMessage(smError,rMain_UnknownReceiver);
    'InviteDeclined' : OnMessage(smWarning,Language.Format(rMain_Decline,[aMsg.Sender]));
    'GroupPrivate' : OnMessage(smWarning,rMain_Private);
    'KickedByAnother':OnMessage(smError,rMain_KickedByAnother);
    'LoadingFinished':
      begin
        Scrabble.GameState:=Scrabble.GameState-[gsLoading];
        OnProgress(self,101);
        OnMessage(smInformation,rMain_LoadingFinished);
        //during gsLoading repaint is adjourned
        if not (gsKibitz in Scrabble.GameState) then
         for i:=0 to Scrabble.NumberOfPlayers-1 do
          if Scrabble.Player[i].Name=TCPClient.PlayerData.PlayerName then
           Scrabble.LocalPlayer:=i;

        DoRepaint(seUnknown);
        if (gsGameEnd in Scrabble.GameState) and
           not aMsg.Value('IsGameEnd',false) then
             DoGameEnd(self);
        TCPClient.OnSend('nwRefresh','all','');
      end;
    else
      OnMessage(smError,aMsg.Value('Info',''));
   end;
 end;
 procedure DoLeave;
 begin
   OnMessage(smInformation,Language.Format(rMain_Leave,[aMsg.Sender]));
   if aMsg.Sender=TCPClient.PlayerData.PlayerName then
   begin
     Bruteforce.Aborted:=true;
     Scrabble.Reset;
     GameCourse.Clear;
     TCPClient.OnSend('nwRefresh','all','');
   end else
   begin
     //last player must not be a kibitz
     if (gsKibitz in Scrabble.GameState) and (aMsg.Value('GroupMembers',0)=0) then
       TCPClient.OnSend('nwLeave',TCPClient.PlayerData.PlayerName,'');
   end;
 end;
 procedure DoLogout;
 var
   s:string;
 begin
   if aMsg.Sender<>TCPClient.PlayerData.PlayerName then
   begin
     TCPClient.RemovePlayer(aMsg.Sender);
     UpdateGameServer;
     s:=cbChatReceiver.Text;
     cbChatReceiver.Items.Delete(cbChatReceiver.Items.IndexOf(aMsg.Sender));
     cbChatReceiver.ItemIndex:=cbChatReceiver.Items.IndexOf(s);
     if aMsg.Value('IsGroup',false) then
     begin
       OnMessage(smWarning,Language.Format(rMain_Logout,[aMsg.Sender]));
       if aMsg.Value('GroupMembers',0)=0 then
         DoLeave;
     end else
       OnMessage(smInformation,Language.Format(rMain_Logout,[aMsg.Sender]));
     if not IsGameServer then
       Scrabble.Reset;
   end;
 end;
 procedure DoInvite;
 var
   b:boolean;
   s1,s2:string;
 begin
   //ask player if he wants to join
   case aMsg.Value('JoinMode',-1) of
     0 : b:=MessageDlg(Language.Format(rMain_InviteGame,[aMsg.Sender])+LineBreak+LineBreak+
                   rMain_InviteLang+' '+aMsg.Value('Language','')+LineBreak+
                   rMain_InviteCountry+' '+aMsg.Value('Country','')+LineBreak+
                   rMain_InviteDictionary+' '+aMsg.Value('Dictionary',''),
                   mtInformation,[mbYes,mbNo],0)=mrYes;
     1 : b:=MessageDlg(Language.Format(rMain_InviteKibitz,[aMsg.Sender]),mtInformation,[mbYes,mbNo],0)=mrYes;
     2 : b:=MessageDlg(Language.Format(rMain_ReInvite,[aMsg.Sender]),mtInformation,[mbYes,mbNo],0)=mrYes;
   end;//case
   if b then
   begin
     //leave current group
     TCPClient.OnSend('nwLeave',TCPClient.PlayerData.PlayerName,'');
     //compare releases
     s1:=TCPClient.Players[aMsg.Sender].Release;
     s1:=Copy(s1,1,Pos('_',s1)-1);
     s2:=TCPClient.PlayerData.Release;
     s2:=Copy(s2,1,Pos('_',s2)-1);
     if s1<>s2 then
       OnMessage(smWarning,Language.Format(rMain_WarnRelease,[s1,s2]));
     //join group
     TCPClient.OnSend('nwJoin',TCPClient.PlayerData.PlayerName,'Target='+aMsg.Sender);
   end else
     TCPClient.OnSend('nwInformation',aMsg.Sender,'Info=InviteDeclined');
 end;
 procedure DoJoin;
 begin
   OnMessage(smInformation,Language.Format(rMain_Join,[aMsg.Sender]));
   if aMsg.Sender=TCPClient.PlayerData.PlayerName then
   begin
     if aMsg.Value('Game',0)<>0 then
     begin
       sleep(500); //process network messages
       TCPClient.OnSend('nwLoadGame',TCPClient.PlayerData.PlayerName,'Name='+aMsg.Value('Game','0')+'.ssv')
     end else
       Scrabble.Reset;
     TCPClient.OnSend('nwRefresh','all','');
   end;
 end;
 procedure DoSyncNewGame;
 var
   i:integer;
   b:boolean;
   s:string;
 begin
   //update kibitz mode
   acKibitz.Checked:=aMsg.Value('AllowKibitz',false);
   UpdateKibitzMode;
   //apply setting
   with fmGameOptions do
   begin
     if aMsg.Sender<>TCPClient.PlayerData.PlayerName then
       cbPresets.Text:=Language.Format(rMain_NetworkSettings,[aMsg.Sender]);
     seNumberOfLetters.Value:=StrToIntDef(aMsg.Values['seNumberOfLetters'],seNumberOfLetters.Value);
     seNumberOfJokers.Value:=StrToIntDef(aMsg.Values['seNumberOfJokers'],seNumberOfJokers.Value);
     cbCanJokerExchange.Checked:=StrToBoolDef(aMsg.Values['cbCanJokerExchange'],cbCanJokerExchange.Checked);
     seGameEndBonus.Value:=StrToIntDef(aMsg.Values['seGameEndBonus'],seGameEndBonus.Value);
     seBoardSize.Value:=StrToIntDef(aMsg.Values['seBoardSize'],seBoardSize.Value);
     rbDim3D.Checked:=StrToBoolDef(aMsg.Values['rbDim3D'],rbDim3D.Checked);
     rbDim2D.Checked:=StrToBoolDef(aMsg.Values['rbDim2D'],rbDim2D.Checked);
     seNumberOfPasses.Value:=StrToIntDef(aMsg.Values['seNumberOfPasses'],seNumberOfPasses.Value);
     seJokerPenalty.Value:=StrToIntDef(aMsg.Values['seJokerPenalty'],seJokerPenalty.Value);
     rbNoLimit.Checked:=StrToBoolDef(aMsg.Values['rbNoLimit'],rbNoLimit.Checked);
     rbPerMove.Checked:=StrToBoolDef(aMsg.Values['rbPerMove'],rbPerMove.Checked);
     rbPerGame.Checked:=StrToBoolDef(aMsg.Values['rbPerGame'],rbPerGame.Checked);
     case aMsg.Values['rgTimeControl'] of
      '0':rbNoLimit.Checked:=true;
      '1':rbPerMove.Checked:=true;
      '2':rbPerGame.Checked:=true;
     end;
     rbTimeControlChange(self);  //enable controls

     meTimeLimitPerMove.Text:=aMsg.Values['meTimeLimitPerMove'];
     meTimeLimitPerGame.Text:=aMsg.Values['meTimeLimitPerGame'];
     cbLostByTime.Checked:=StrToBoolDef(aMsg.Values['cbLostByTime'],cbLostByTime.Checked);
     case aMsg.Values['rgWordCheckMode'] of
      '0':rbWCMTakeback.Checked:=true;
      '1':rbWCMPoll.Checked:=true;
      '2':rbWCMChallenge.Checked:=true;
     end;
     WCMUpdate(nil);  //enable wcm edit fields if mode is challenge
     cbCLABBERS.Checked:=StrToBoolDef(aMsg.Values['cbCLABBERS'],cbCLABBERS.Checked);

     seChallengePeriod.Value:=StrToIntDef(aMsg.Values['seChallengePeriod'],seChallengePeriod.Value);
     seChallengePenalty.Value:=StrToIntDef(aMsg.Values['seChallengePenalty'],seChallengePenalty.Value);
     seChallengeBonus.Value:=StrToIntDef(aMsg.Values['seChallengeBonus'],seChallengeBonus.Value);
     seNumberOfRandomLetters.Value:=StrToIntDef(aMsg.Values['seNumberOfRandomLetters'],seNumberOfRandomLetters.Value);
     seLimitExchange.Value:=StrToIntDef(aMsg.Values['seLimitExchange'],seLimitExchange.Value);
     seScrabbleBonus.Value:=StrToIntDef(aMsg.Values['seScrabbleBonus'],seScrabbleBonus.Value);
     cbChangeIsPass.Checked:=StrToBoolDef(aMsg.Values['cbChangeIsPass'],cbChangeIsPass.Checked);
     cbCambioSecco.Checked:=StrToBoolDef(aMsg.Values['cbCambioSecco'],cbCambioSecco.Checked);
     cbJokerization.Checked:=StrToBoolDef(aMsg.Values['cbJokerization'],cbJokerization.Checked);
     cbSubstractLettersLeft.Checked:=StrToBoolDef(aMsg.Values['cbSubstractLettersLeft'],cbSubstractLettersLeft.Checked);
     cbAddLettersLeft.Checked:=StrToBoolDef(aMsg.Values['cbAddLettersLeft'],cbAddLettersLeft.Checked);
     if fmGameOptions.rbPerGame.Checked then
     begin
       rbTimePenaltyEndGame.Checked:=StrToBoolDef(aMsg.Values['rbTimePenaltyEndGame'],rbTimePenaltyEndGame.Checked);
       rbTimePenaltyBuyTime.Checked:=StrToBoolDef(aMsg.Values['rbTimePenaltyBuyTime'],rbTimePenaltyBuyTime.Checked);
       seTimePenaltyValue.Value:=StrToIntDef(aMsg.Values['seTimePenaltyValue'],seTimePenaltyValue.Value);
       seTimePenaltyCount.Value:=StrToIntDef(aMsg.Values['seTimePenaltyCount'],seTimePenaltyCount.Value);
     end;

     LettersetString[lsLetter]:=aMsg.Values['Letters'];
     LettersetString[lsCount]:=aMsg.Values['LetterCount'];
     LettersetString[lsValue]:=aMsg.Values['LetterValue'];
     if StrToBoolDef(aMsg.Values['rbLeftToRight'],rbLeftToRight.Checked) then
       rbLeftToRight.Checked:=true else
       rbRightToLeft.Checked:=true;
     UpdateLettersetInfo(nil);

     with TStringList.Create do
     try
       StrictDelimiter:=true;
       i:=0;
       repeat
         s:=aMsg.Values['Board'+inttostr(i)];
         if s<>'' then
           Add(s);
         inc(i);
       until s='';
       BoardString:=DelimitedText;
       cbBoard.ItemIndex:=cbBoard.Items.IndexOf(BoardSet); //update label
     finally
       Free; //TStringList
     end;

   end;
   fmNewGame.GameSeed:=StrToIntDef(aMsg.Values['RandSeed'],0);
   fmNewGame.NumberOfPlayers:=StrToIntDef(aMsg.Values['PlayerCount'],1);
   fmNewGame.cbPlayer1.Text:=aMsg.Value('Player1Name','');
   fmNewGame.cbPlayer2.Text:=aMsg.Value('Player2Name','');
   fmNewGame.cbPlayer3.Text:=aMsg.Value('Player3Name','');
   fmNewGame.cbPlayer4.Text:=aMsg.Value('Player4Name','');
   //compare dics
   s:=Trim(ExtractFileNameOnly(aMsg.Value('Dictionary','')));//extractfilenameonly for legacy purpose
   if ((s<>'') and (s<>Dictionary.Info[itFileName])) then
   begin
     OnMessage(smDebug,aMsg.Value('Dictionary','')+'<>'+Dictionary.Info[itFileName]);
     if TimeoutMsg(Language.Format(rMain_SyncDictionary,[s]))=mrYes then
       Dictionary.LoadFrom(Config.Path+s+'.dic') else
       OnMessage(smWarning,rMain_WarnDictionary);
   end;
   //set categories
   with TStringList.Create do
   try
     CommaText:=aMsg.Value('Categories','');
     if Count<>fmGameOptions.gbCategories.Items.Count then
       OnMessage(smError,rMain_WarnCategories) else
       for i:=0 to Count-1 do
       begin
         fmGameOptions.gbCategories.Checked[i]:=StrToBool(Strings[i]);
         fmGameOptions.gbCategoriesItemClick(self,i);
       end;
   finally
     Free;
   end;
   //new game id
   TCPClient.PlayerData.GameID:=aMsg.Value('GameNumber',0);
   if not aMsg.Value('IsLoading',false) and
      not (gsKibitz in Scrabble.GameState) then
   begin
     if (TCPClient.PlayerData.ClientState in [csBusy,csAfk]) then
       b:=false else
       b:=fmNewGame.ShowModal(Language.Format(rMain_NetworkStartNewGame,[aMsg.Sender]))=mrOk;
     TCPClient.OnSend('nwAnswer','group','Answer='+BoolToStr(b,true));
     TCPClient.OnSend('nwRefresh',TCPClient.PlayerData.PlayerName,'');
   end;
 end;
 procedure DoStartNewGame;
 var
   i:integer;
   s:string;
 begin
   if aMsg.Value('IsLoading',false) then
     Scrabble.GameState:=Scrabble.GameState+[gsLoading];//gsloading blocks sound output
   Scrabble.NewGame;
   //only one player should send info about sequence of players to server
   if (Scrabble.Player[0].Name=TCPClient.PlayerData.PlayerName) and
       not (gsLoading in Scrabble.GameState) then
   begin
     s:='';
     for i:=0 to Scrabble.NumberOfPlayers-1 do
       s:=s+Scrabble.Player[i].Name+',';
     delete(s,length(s),1);
     TCPClient.OnSend('nwServerInfo',TCPClient.PlayerData.PlayerName,'Sequence='+s);
   end;
   //update kibitz state
   TCPClient.PlayerData.IsKibitz:=true;
   for i:=0 to Scrabble.NumberOfPlayers-1 do
    if Scrabble.Player[i].Name=TCPClient.PlayerData.PlayerName then
     TCPClient.PlayerData.IsKibitz:=false;
   if TCPClient.PlayerData.IsKibitz then
     Scrabble.GameState:=Scrabble.GameState+[gsKibitz] else
     Scrabble.GameState:=Scrabble.GameState-[gsKibitz];
 end;
 procedure DoNextPlayer;
 var
   s       : string;
   i,j     : integer;
   x,y,z   : byte;
   aLetter : TLetter;
   aActiveDimension:TActiveDimension;
 begin
   //message for invalid moves
   case aMsg.Value('Invalid','') of
    '1': OnMessage(smInformation,rMain_Invalid+' '+rMain_ErrorRules);
    '2': begin
           s:=aMsg.Value('InvalidWords','');
           if s<>'' then
             OnMessage(smInformation,rMain_Invalid+' '+Language.Format(Language.Plural(rMain_ErrorUnknownWords,s,false),[s])) else
             OnMessage(smInformation,rMain_Invalid+' '+Language.Format(Language.Plural(rMain_ErrorUnknownWords,99,false),['']));
         end;
    '3': OnMessage(smInformation,rMain_Invalid+' '+Language.Format(rMain_ErrorLowScore,[Scrabble.LeastValue]));
   end;//case invalid

   //switch off sound if loading
   if aMsg.Value('IsLoading',false) then
   begin
     Scrabble.GameState:=Scrabble.GameState+[gsLoading];
     OnProgress(self,aMsg.Value('LoadingProgress',0));
   end;
   //3D Pos
   aActiveDimension.Axis:=TDimension(aMsg.Value('DimAxis',byte(Scrabble.ActiveDimension.Axis)));
   aActiveDimension.Position:=aMsg.Value('DimPos',Scrabble.ActiveDimension.Position);
   Scrabble.ActiveDimension:=aActiveDimension;
   //place or mark letters
   if (aMsg.Sender<>TCPClient.PlayerData.PlayerName) or
      (gsLoading in Scrabble.GameState) then
   begin
     for i:=0 to aMsg.Value('Count',0)-1 do
     begin
       s:='L'+inttostr(i);
       j:=aMsg.Value(s+'_BankPos',0);//"bank" for compatibility with legacy versions
       x:=aMsg.Value(s+'_dx',7);
       y:=aMsg.Value(s+'_dy',7);
       z:=aMsg.Value(s+'_dz',7);
       aLetter:=Scrabble.RackLetter[Scrabble.CurrentPlayer,j];
       if aLetter=nil then
       begin
         OnMessage(smError,'Unexpected data for letter.');
         Exit;
//         Continue;
       end;
       if (aLetter.IsJoker or aLetter.IsRandom) then
         aLetter.What:=UTF8Decode(aMsg.Value(s+'_What',aLetter.What))[1];
       Scrabble.PlaceLetterAt(aLetter,x,y,z);
       if Scrabble.MoveState=msJokerExchanged then
       begin
         s:=Dictionary.ReplaceDigraphs(UTF8Encode(widestring(Scrabble.BoardLetter[x,y,z].What)));
         case Scrabble.Dimension of
          D2 : OnMessage(smInformation,Language.Format(rMain_Exchange2D,
                                       [Scrabble.Player[Scrabble.CurrentPlayer].Name,
                                        Scrabble.PosToString(dy,y,fmGameOptions.cbRoman.Checked),
                                        Scrabble.PosToString(dx,x,fmGameOptions.cbRoman.Checked),
                                        s]));
          D3 : OnMessage(smInformation,Language.Format(rMain_Exchange3D,
                                       [Scrabble.Player[Scrabble.CurrentPlayer].Name,
                                        Scrabble.PosToString(dy,y,fmGameOptions.cbRoman.Checked),
                                        Scrabble.PosToString(dx,x,fmGameOptions.cbRoman.Checked),
                                        Scrabble.PosToString(dz,z,fmGameOptions.cbRoman.Checked),
                                        s]));
         end;//case
       end;
     end;
     for i:=0 to aMsg.Value('ChangeCount',0)-1 do
       Scrabble.ToggleChangeState(aMsg.Value('L'+inttostr(i),0));
   end;
   //update LastMove.(Value, Words), time and execute
   Scrabble.CheckMove(false);
   Scrabble.SetPlayerTime(aMsg.Value('TimeLeft',Scrabble.Player[Scrabble.CurrentPlayer].mTimeElapsed div 1000));
   Scrabble.NextPlayer;
 end;
 procedure DoAnswer;
 var
   b:boolean;
 begin
   b:=aMsg.Value('Answer',false);
   Poll.Answer[aMsg.Sender]:=b;
   if not b and not aMsg.Value('Silent',false) then
     OnMessage(smInformation,Language.Format(rMain_DeclinesGeneral,[aMsg.Sender]));
   pbStatus.Repaint;
 end;
 procedure DoPoll;
 var
   s:string;
   b:boolean;
 begin
   if not (gsKibitz in Scrabble.GameState) then
   begin
     b:=gsPaused in Scrabble.GameState;//pause clock during poll but do not toggle if active
     if not b then
       Scrabble.TogglePaused;
     Scrabble.GameState:=Scrabble.GameState+[gsPoll];
     if (aMsg.Sender<>TCPClient.PlayerData.PlayerName) then
     begin
       Poll.Init(TCPClient.Players[TCPClient.PlayerData.PlayerName].Mates);
       pbStatus.Repaint;
     end;
     if (aMsg.Sender<>TCPClient.PlayerData.PlayerName) or (aMsg.Value('PollAll',false)) then
     begin
       TCPClientIsWorking:=false;//to pass chat messages
       s:=aMsg.Value('Content','');
       //messages to be translated
       case TPollContent(aMsg.Value('PollContent',-1)) of
        pcPrivate  : s:=Language.Format(rMain_MakePrivateF,[s]);
        pcPublic   : s:=Language.Format(rMain_MakePublicF,[s]);
        pcResume   : s:=Language.Format(rMain_ResumeF,[s]);
        pcNotFound : begin
                       s:=Language.Format(Language.Plural(rMain_UnknownWords,s,false),[s]);
                       Scrabble.HasChallenged:=crChecked; //do not challenge twice
                     end;
        pcPause    : s:=Language.Format(rMain_PauseF,[s]);
        pcUnPause  : s:=Language.Format(rMain_UnPauseF,[s]);
        pcNone     : if s='' then
                      s:=aMsg.Value('Question','') else
                      s:=s+' '+aMsg.Value('Question','');
       end;//case
       //"no" is sent as well if a further dialog pops up
       if not (TCPClient.PlayerData.ClientState in [csBusy,csAfk]) and
          (TimeoutMsg(s)=mrYes) then
       TCPClient.OnSend('nwAnswer','group','Answer='+BoolToStr(true,true)) else
       TCPClient.OnSend('nwAnswer','group','Answer='+BoolToStr(false,true));
     end else
       //self
       TCPClient.OnSend('nwAnswer','group','Answer='+BoolToStr(true));
     if not b then
       Scrabble.TogglePaused;
     Scrabble.GameState:=Scrabble.GameState-[gsPoll];
   end;
end;
 procedure DoKibitz;
 begin
   acKibitz.Checked:=aMsg.Value('SetKibitz',false);
   UpdateKibitzMode;
   if (gsKibitz in Scrabble.GameState) and not acKibitz.Checked then
     TCPClient.OnSend('nwLeave',TCPClient.PlayerData.PlayerName,'Private='+booltostr(true,true)) else
   if aMsg.Sender=TCPClient.PlayerData.PlayerName then
     TCPClient.OnSend('nwRefresh','all','');
 end;
 procedure DoCambioSecco;
 begin
   if Scrabble.CambioSecco then
     OnMessage(smInformation,Language.Format(rMain_CambioSeccoUsed,[Scrabble.Player[Scrabble.CurrentPlayer].Name]));
 end;
 procedure DoCheckWord;
 var
   b:boolean;
   i:integer;
   s:string;
 begin
   s:=aMsg.Value('Word','');
   if (gsCLABBERS in Scrabble.GameState) then
   begin
     with TStringList.Create do
     try
       CommaText:=Dictionary.WordsByLetters(s);
       for i:=Count-1 downto 0 do
        if (UTF8length(Strings[i])<>UTF8Length(s)) then
         Delete(i);
       b:=Count>0;
     finally
       Free;
     end;
   end else
     b:=Dictionary.IsWordInDictionary(s,i)=frInclude;
   TCPClient.OnSend('nwAnswer','group','Answer='+BoolToStr(b,true)+nwDelimiter+'Silent=true');
 end;
 procedure DoRemoteGames;
 var
   i,j:integer;
   sl:TStringList;
 begin
   with fmRemote do
   begin
     sgRemote.RowCount:=aMsg.Value('NumberOfGames',1)+1;
     for i:=0 to aMsg.Value('NumberOfGames',0)-1 do
     begin
       sgRemote.Cells[0,i+1]:='0';         //active game, not on turn
       if aMsg.Value('Game'+inttostr(i)+'.GameEnd',false) then
         sgRemote.Cells[0,i+1]:='1' else   //ended game
       begin
         sl:=TStringList.Create;
         try
           sl.StrictDelimiter:=true;
           sl.Delimiter:=',';
           sl.DelimitedText:=aMsg.Value('Game'+inttostr(i)+'.Sequence','');
           if sl.Count>0 then
           begin
             j:=aMsg.Value('Game'+inttostr(i)+'.Moves',0) mod sl.Count;
             if (sl[j]=TCPClient.PlayerData.PlayerName) then
               sgRemote.Cells[0,i+1]:='2';  //active game, own turn
           end;
         finally
           sl.Free;
         end;
       end;
       sgRemote.Cells[1,i+1]:=DateTimeToStr(aMsg.Value('Game'+inttostr(i)+'.Date',Now));
       sgRemote.Cells[2,i+1]:=DateTimeToStr(aMsg.Value('Game'+inttostr(i)+'.LastAccess',Now));
       sgRemote.Cells[3,i+1]:=UTF8Encode(AnsiReplaceText(UTF8Decode(aMsg.Value('Game'+inttostr(i)+'.Player','')), '"', ''));
       sgRemote.Cells[4,i+1]:=aMsg.Value('Game'+inttostr(i)+'.Moves','');
       sgRemote.Cells[5,i+1]:=aMsg.Value('Game'+inttostr(i)+'.Name','');
     end;
     Show;
   end;
 end;
 procedure DoHighscore;
 var
   i:integer;
 begin
   fmStatistics.Bars.Clear;
   for i:=0 to aMsg.Value('Count',0)-1 do
     fmStatistics.Bars.AddXY(aMsg.Value('Bin'+inttostr(i),0),aMsg.Value('Cnt'+inttostr(i),0));
   fmStatistics.Lines.Position:=TCPClient.PlayerData.Rating;
   if aMsg.Value('Own',0)>0 then
     fmStatistics.chRating.Hint:=rNetWort_OwnRating+': '+inttostr(TCPClient.PlayerData.Rating) +#13+
                                 Language.Format(rNetWort_OwnRank,[aMsg.Value('Own','0'),aMsg.Value('Overall','0')]) else
     fmStatistics.chRating.Hint:='At least 10 finished games are required for an official rating.';
   fmStatistics.chRating.Visible:=true;
   fmStatistics.ShowModal;
 end;
 procedure DoChallengeCall;
 var
   i:integer;
 begin
   for i:=0 to Scrabble.NumberOfPlayers-1 do
    if Scrabble.Player[i].Name=aMsg.Sender then
    begin
      TimeoutMsg('',0);//cancel last msg; needed when asked for challenge at last move but action comes from other player with positive result
      OnMessage(smInformation,Language.Format(rNetwork_Challenge,[Scrabble.Player[i].Name]));
      DoChallenge(aMsg.Value('Result',true),aMsg.Value('Invalid',''),i);
      break;
    end;
 end;
 procedure DoAddTime;
 begin
   AddTime(60);
   UpdateScore;//pbScore.Repaint;
   GameCourse.AddPenalty(Scrabble.CurrentPlayer,Scrabble.CurrentMove,-Scrabble.TimePenaltyValue);
   OnMessage(smInformation,Language.Format(Language.Plural(rMain_TimePenaltyMsg,Scrabble.TimePenaltyValue,false),
       [Scrabble.Player[Scrabble.CurrentPlayer].Name,Scrabble.TimePenaltyValue,Scrabble.Player[Scrabble.CurrentPlayer].TimePenaltyLeft]));
 end;
 procedure DoLostByTime;
 begin
   if aMsg.Sender<>TCPClient.PlayerData.PlayerName then
   begin
     OnMessage(smInformation,Language.Format(Language.Plural(rMain_LostByTime,Scrabble.Player[Scrabble.CurrentPlayer].Points,false),[Scrabble.Player[Scrabble.CurrentPlayer].Name,Scrabble.Player[Scrabble.CurrentPlayer].Points]));
     GameCourse.AddPenalty(Scrabble.CurrentPlayer,Scrabble.CurrentMove{-1},-Scrabble.Player[Scrabble.CurrentPlayer].Points);
     Scrabble.PlayerValue[Scrabble.CurrentPlayer]:=0;
   end;
 end;

begin
  {$ifdef DebugNetworkFull}
  OnMessage(smDebug,aMsg.CommaText);
  {$endif}
  {$ifdef DebugNetwork}
  OnMessage(smDebug,aMsg.MessageType+'(Sender: '+aMsg.Sender+', Receiver: '+aMsg.Receiver+')');
  {$endif}
  acNetwork.ImageIndex:=33;
  try
    case aMsg.MessageType of
     'nwDisconnect'   : DoDisconnect;
     'nwLogin'        : DoLogin;
     'nwRefresh'      : DoRefresh;
     'nwLogout'       : DoLogout;
     'nwChat'         : DoChat;
     'nwInformation'  : DoInformation;
     'nwInvite'       : DoInvite;
     'nwJoin'         : DoJoin;
     'nwLeave'        : DoLeave;
     'nwAnswer'       : DoAnswer;
     'nwPoll'         : DoPoll;
     'nwSyncNewGame'  : DoSyncNewGame;
     'nwStartNewGame' : DoStartNewGame;
     'nwPause'        : acPauseExecute(nil);
     'nwKibitz'       : DoKibitz;
     'nwCambioSecco'  : DoCambioSecco;
     'nwJokerize'     : DoJokerize(TLetter(Scrabble.Letters[aMsg.Value('LetterIndex',-1)]));
     'nwCheckWord'    : DoCheckWord;
     'nwRemoteGames'  : DoRemoteGames;
     'nwHighscore'    : DoHighscore;
     'nwNextPlayer'   : DoNextPlayer;
     'nwChallenge'    : DoChallengeCall;
     'nwEndGame'      : if (gsRunning in Scrabble.GameState) then DoGameEnd(self);
     'nwGameResult'   : OnMessage(smGameResult,'Rating: '+aMsg.Value('OldRating','')+' -> '+aMsg.Value('NewRating',''));
     'nwAddTime'      : DoAddTime;
     'nwBestValues'   : GameCourse.AllBestMovesValues:=aMsg.Value('BestValues','');
     'nwPing'         : Pinged:=false;
     'nwLostByTime'   : DoLostByTime;
     else
       OnMessage(smError,rMain_UnknownMessage+' '+aMsg.DelimitedText);
    end;//case
  except
    on E:Exception do OnMessage(smError,E.Message);
  end;
  LastMessage:=GetTickCount;
end;

procedure TfmMain.DoSyncMessage(var Msg: TLMessage);
var
  aMsg:TNetworkMessage;
begin
  if not TCPClientIsWorking then //tcp's postmessage must not cause parallel execution
  begin
    TCPClientIsWorking:=true;
    aMsg:=TNetworkMessage.Create;
    try
      repeat
        aMsg.DelimitedText:=TCPClient.GetDataIn;
        if aMsg.Count>0 then
          DoTCPClientReceive(aMsg);
      until aMsg.Count=0;
    finally
      aMsg.Free;
      TCPClientIsWorking:=false;
    end;
  end;
end;

procedure TfmMain.DoTCPClientSend(aMsgId: string; aReceiver, aMsg: string);
begin
  if assigned(TCPClient) and not TCPClient.Terminated then
  with TNetworkMessage.Create do
  try
    DelimitedText:=aMsg;
    Values['MessageType']:=aMsgId;
    Values['Sender']:=TCPClient.PlayerData.PlayerName;
    Values['Receiver']:=aReceiver;
    TCPClient.DataOut.Add(DelimitedText+CRLF);
  finally
    Free;
  end;
  acNetwork.ImageIndex:=34;
end;

procedure TfmMain.SyncNewGame;
var
  i  : integer;
  sl : TStringList;
begin
  with TStringList.Create do
  try
    Delimiter:=nwDelimiter;
    StrictDelimiter:=true;
    Add('seNumberOfLetters='+inttostr(fmGameOptions.seNumberOfLetters.Value));
    Add('seNumberOfJokers='+inttostr(fmGameOptions.seNumberOfJokers.Value));
    Add('cbCanJokerExchange='+booltostr(fmGameOptions.cbCanJokerExchange.Checked,'1','0'));
    Add('seGameEndBonus='+inttostr(fmGameOptions.seGameEndBonus.Value));
    Add('seBoardSize='+inttostr(fmGameOptions.seBoardSize.Value));
    Add('rbDim3D='+booltostr(fmGameOptions.rbDim3D.Checked,'1','0'));
    Add('rbDim2D='+booltostr(fmGameOptions.rbDim2D.Checked,'1','0'));
    Add('seNumberOfPasses='+inttostr(fmGameOptions.seNumberOfPasses.Value));
    Add('seJokerPenalty='+inttostr(fmGameOptions.seJokerPenalty.Value));
    Add('rbNoLimit='+booltostr(fmGameOptions.rbNoLimit.Checked));
    Add('rbPerMove='+booltostr(fmGameOptions.rbPerMove.Checked));
    Add('rbPerGame='+booltostr(fmGameOptions.rbPerGame.Checked));
    //legacy function
    if fmGameOptions.rbNoLimit.Checked then
     Add('rgTimeControl=0') else
    if fmGameOptions.rbPerMove.Checked then
     Add('rgTimeControl=1') else
     Add('rgTimeControl=2');

    Add('meTimeLimitPerMove='+fmGameOptions.meTimeLimitPerMove.Text);
    Add('meTimeLimitPerGame='+fmGameOptions.meTimeLimitPerGame.Text);
    Add('cbLostByTime='+booltostr(fmGameOptions.cbLostByTime.Checked,'1','0'));

    if fmGameOptions.rbWCMTakeback.Checked then
      Add('rgWordCheckMode=0') else
    if fmGameOptions.rbWCMPoll.Checked then
      Add('rgWordCheckMode=1') else
      Add('rgWordCheckMode=2');

    Add('cbCLABBERS='+booltostr(fmGameOptions.cbCLABBERS.Checked,'1','0'));

    Add('seChallengePeriod='+inttostr(fmGameOptions.seChallengePeriod.Value));
    Add('seChallengePenalty='+inttostr(fmGameOptions.seChallengePenalty.Value));
    Add('seChallengeBonus='+inttostr(fmGameOptions.seChallengeBonus.Value));

    Add('seNumberOfRandomLetters='+inttostr(fmGameOptions.seNumberOfRandomLetters.Value));
    Add('seLimitExchange='+inttostr(fmGameOptions.seLimitExchange.Value));
    Add('seScrabbleBonus='+inttostr(fmGameOptions.seScrabbleBonus.Value));

    Add('cbChangeIsPass='+booltostr(fmGameOptions.cbChangeIsPass.Checked,'1','0'));
    Add('cbCambioSecco='+booltostr(fmGameOptions.cbCambioSecco.Checked,'1','0'));
    Add('cbJokerization='+booltostr(fmGameOptions.cbJokerization.Checked,'1','0'));
    Add('cbAddLettersLeft='+booltostr(fmGameOptions.cbAddLettersLeft.Checked,'1','0'));
    Add('cbSubstractLettersLeft='+booltostr(fmGameOptions.cbSubstractLettersLeft.Checked,'1','0'));
    if fmGameOptions.rbPerGame.Checked then
    begin
      Add('rbTimePenaltyEndGame='+booltostr(fmGameOptions.rbTimePenaltyEndGame.Checked,'1','0'));
      Add('rbTimePenaltyBuyTime='+booltostr(fmGameOptions.rbTimePenaltyBuyTime.Checked,'1','0'));
      Add('seTimePenaltyValue='+inttostr(fmGameOptions.seTimePenaltyValue.Value));
      Add('seTimePenaltyCount='+inttostr(fmGameOptions.seTimePenaltyCount.Value));
    end;
    //letterset
    Add('Letters='+fmGameOptions.LetterSetString[lsLetter]);
    Add('LetterCount='+fmGameOptions.LetterSetString[lsCount]);
    Add('LetterValue='+fmGameOptions.LetterSetString[lsValue]);
    Add('rbLeftToRight='+booltostr(fmGameOptions.rbLeftToRight.Checked,'1','0'));
    //board
    sl:=TStringList.Create;
    try
      sl.CommaText:=fmGameOptions.BoardString;
      for i:=0 to sl.Count-1 do
        Add('Board'+inttostr(i)+'='+sl[i]);
    finally
      sl.Free;
    end;
    GenRandomize;
    Add('RandSeed='+inttostr(Seed));
    Add('Dictionary='+Dictionary.Info[itFileName]);
    sl:=TStringList.Create;
    try
      for i:=0 to fmGameOptions.gbCategories.Items.Count-1 do
       sl.Add(BoolToStr(fmGameOptions.gbCategories.Checked[i],false));
      Add('Categories='+sl.CommaText);
    finally
      sl.Free;
    end;
    //allow kibitz?
    Add('AllowKibitz='+booltostr(TCPClient.PlayerData.AllowKibitz));
    Poll.Init(TCPClient.Players[TCPCLient.PlayerData.PlayerName].Mates);
    TCPClient.OnSend('nwSyncNewGame','group',DelimitedText);
    if Poll.Waitfor(false) then
      TCPClient.OnSend('nwStartNewGame','group','');
  finally
    Free;
  end;
end;

procedure TfmMain.SyncGameEnd;
var
  i:integer;
begin
  with TStringList.Create do
  try
    Delimiter:=nwDelimiter;
    StrictDelimiter:=true;
    Values['NumberOfPlayers']:=inttostr(Scrabble.NumberOfPlayers);
    for i:=0 to Scrabble.NumberOfPlayers-1 do
    begin
      if Scrabble.Player[i].Name=TCPClient.PlayerData.PlayerName then
        Values['MyResult']:=inttostr(Scrabble.Player[i].Points);
      Values['Player'+inttostr(i)+'Name']:=Scrabble.Player[i].Name;
      Values['Player'+inttostr(i)+'Value']:=inttostr(Scrabble.Player[i].Points);
    end;
    TCPClient.OnSend('nwGameResult',TCPClient.PlayerData.PlayerName,DelimitedText);// else
  finally
    Free;
  end;
end;

procedure TfmMain.SyncNextPlayer(const aLastError,aOriginalLastError:TLastError);
var
  i,j:integer;
  aLetter:TLetter;
begin
  with TStringList.Create do
  try
    Delimiter:=nwDelimiter;
    StrictDelimiter:=true;
    Values['MoveNumber']:=inttostr(Scrabble.ActualMove);
    Values['TimeLeft']:=inttostr(Scrabble.sMoveTime+round(Scrabble.Player[Scrabble.CurrentPlayer].mTimeElapsed/1000));
    if aLastError=leNone then
    begin
      Values['DimAxis']:=inttostr(byte(Scrabble.PlacedDimension.Axis));
      Values['DimPos']:=inttostr(Scrabble.PlacedDimension.Position);
    end else
    begin
      Values['DimAxis']:=inttostr(byte(Scrabble.ActiveDimension.Axis));
      Values['DimPos']:=inttostr(Scrabble.ActiveDimension.Position);
    end;
    if aLastError=leNone then
    begin
      Values['Count']:=inttostr(length(Scrabble.LastMove.UsedLetters));
      for i:=0 to length(Scrabble.LastMove.UsedLetters)-1 do
      begin
        aLetter:=Scrabble.LastMove.UsedLetters[i];
        Values['L'+inttostr(i)+'_BankPos']:=inttostr(aLetter.RackPos);//"bank" for compatibility with legacy versions
        Values['L'+inttostr(i)+'_dx']:=inttostr(aLetter.Where[dx]);
        Values['L'+inttostr(i)+'_dy']:=inttostr(aLetter.Where[dy]);
        Values['L'+inttostr(i)+'_dz']:=inttostr(aLetter.Where[dz]);
        if aLetter.IsJoker or aLetter.IsRandom then
          Values['L'+inttostr(i)+'_What']:=UTF8Encode(widestring(aLetter.What));
      end;
    end else //leNone
    case Scrabble.MoveState of
     msJokerExchanged:
      begin
        Values['Count']:='1';
        Values['L0_dx']:=inttostr(Scrabble.ExchangedJoker.Where[dx]);
        Values['L0_dy']:=inttostr(Scrabble.ExchangedJoker.Where[dy]);
        Values['L0_dz']:=inttostr(Scrabble.ExchangedJoker.Where[dz]);
        Values['L0_BankPos']:=inttostr(Scrabble.ExchangedJoker.RackPos);//"bank" for compatibility with legacy versions
      end;
     msLetterExchange:
      begin
        j:=0;
        for i:=0 to Scrabble.RackSize-1 do
        begin
          aLetter:=Scrabble.RackLetter[Scrabble.CurrentPlayer,i];
          if (aLetter<>nil) and (aLetter.State=lsChange) then
          begin
            Values['L'+inttostr(j)]:=inttostr(aLetter.RackPos);
            inc(j);
          end;
        end;
        if j>0 then
          Values['ChangeCount']:=inttostr(j);
      end;
     else //msNone
      case aOriginalLastError of
       leNone,leNoLetter,leLetterChanged : Values['Invalid']:='';
       leFirstMove,leDimension,leConnection,leSuccessive : Values['Invalid']:='1';
       leUnknownWord :
        begin
          Values['Invalid']:='2';
          Values['InvalidWords']:=Dictionary.ReplaceDigraphs(Scrabble.LastMove.UnknownWords);
        end;
       leLowScore: Values['Invalid']:='3';
      end;
    end;//case
    TCPClient.OnSend('nwNextPlayer','group',DelimitedText);
  finally
    Free;
  end;
end;

procedure TfmMain.UpdateKibitzMode;
begin
  if acKibitz.Checked then
    tbKibitz.ImageIndex:=21 else
    tbKibitz.ImageIndex:=24;
  if not (gsKibitz in Scrabble.GameState) then
    TCPClient.PlayerData.AllowKibitz:=acKibitz.Checked;
end;

procedure TfmMain.acKibitzExecute(Sender: TObject);
begin
  Poll.Init(TCPClient.Players[TCPClient.PlayerData.PlayerName].Mates);
  if acKibitz.Checked then
    TCPClient.OnSend('nwPoll','group',
                   'Content='+TCPClient.PlayerData.PlayerName+nwDelimiter+
                   'PollContent='+inttostr(integer(pcPrivate))) else
    TCPClient.OnSend('nwPoll','group',
                   'Content='+TCPClient.PlayerData.PlayerName+nwDelimiter+
                   'PollContent='+inttostr(integer(pcPublic)));
  if Poll.Waitfor(true) then
  begin
    //inform kibitzes why they are kicked
    if acKibitz.Checked then
      TCPClient.OnSend('nwInformation','kibitzes','Info=GroupPrivate');
    TCPClient.OnSend('nwKibitz','group','SetKibitz='+BoolToStr(not acKibitz.Checked,true));
  end;
end;

procedure TfmMain.acNetworkExecute(Sender: TObject);
begin
  if gsNetwork in Scrabble.GameState then
  begin
    if (Sender=nil) or (TimeoutMsg(rMain_CloseNetwork)=mrYes) then
    begin
      if assigned(TCPServer) then TCPServer.Terminate;
      if assigned(TCPClient) then TCPClient.Terminate;
    end;
  end else //gsNetwork
  try
    if fmNetwork.ShowModal=mrOk then
    begin
      BruteForce.ClearMove;

      IsGameServer:=fmNetwork.IsGameServer;

      if not IsGameServer and fmNetwork.rbServer.Checked then
        TCPServer:=TLanTCPServer.Create(fmNetwork.edNetworkPort.Text);

      {$ifdef TestServer}
      ShowMessage('You will be connected to the test server.'+#13+
                  'To run a normal network game just start the official release');
      {$endif}
      if IsGameServer then
        TCPClient:=TTCPClient.Create(fmNetwork.edLoginWAN.Text) else
        TCPClient:=TTCPClient.Create(fmNetwork.edLoginLAN.Text);
      with TCPClient do
      begin
        //Proxy
        if fmGameOptions.cbProxy.Checked and IsGameServer then
          ProxyIP:=fmGameOptions.edProxyIp.Text else
          ProxyIP:=''; //lan mode always without proxy
        ProxyPort:=fmGameOptions.edProxyPort.Text;
        ProxyUser:=fmGameOptions.edProxyUser.Text;
        ProxyPass:=fmGameOptions.edProxyPass.Text;
        //Ip, port
        if IsGameServer then
        begin
          TCPClient.Ip:=Config.Read('Network/Servers/f'+inttostr(fmNetwork.cbWanServer.ItemIndex)+'/Address','');
          TCPClient.Port:=cPort;
        end else
        begin
          if fmNetwork.rbServer.Checked then
            TCPClient.Ip:='127.0.0.1' else
            TCPClient.Ip:=fmNetwork.edServerAddr.Text;
          TCPClient.Port:=fmNetwork.edNetworkPort.Text;
        end;
        TCPClient.ServerVersion:=inttostr(nwServerVersion);
        //user info
        if fmNetwork.IsGameServer then
        begin
          PlayerData.Country:=fmNetwork.cbCountry.Text;
          PlayerData.City:=fmNetwork.edCity.Text;
          TCPClient.Email:=fmNetwork.edEmail.Text;
          TCPClient.Password:=fmNetwork.edPassword.Text;
          TCPClient.UID:=MAC_Address;
          PlayerData.MenuLang:=Language.Info[liLangID];
          PlayerData.Release:=cVersion;
          {$ifdef win32} PlayerData.Release:=PlayerData.Release+'_win32'; {$endif}
          {$ifdef win64} PlayerData.Release:=PlayerData.Release+'_win64'; {$endif}
          {$ifdef Linux}
           {$IFDEF LCLQt}
            {$ifdef CPU32} PlayerData.Release:=PlayerData.Release+'_i386-qt'; {$endif}
            {$ifdef CPU64} PlayerData.Release:=PlayerData.Release+'_x86-64-qt';{$endif}
           {$ELSE}
            {$ifdef CPU32} PlayerData.Release:=PlayerData.Release+'_i386'; {$endif}
            {$ifdef CPU64} PlayerData.Release:=PlayerData.Release+'_x86-64';{$endif}
           {$ENDIF}
          {$endif} //LInux
          {$ifdef Darwin}
           {$IFDEF LCLQt}  PlayerData.Release:=PlayerData.Release+'_darwin-qt';
           {$ELSE}         PlayerData.Release:=PlayerData.Release+'_darwin';{$endif}
          {$endif} //Darwin
        end;//rbGameServer.Checked

        OnTerminate:=@DoTCPClientTerminate;
        OnSend:=@DoTCPClientSend;
        cbChatReceiver.Visible:=IsGameServer;
      end;

      Poll:=TPoll.Create;
      Pinged:=false;
      MsgConnectionLostDone:=false;
      LastMessage:=GetTickCount;
      TCPClient.Suspended:=false;
      TCPClientIsWorking:=false;
      if IsGameServer then
      begin
        miLoad.Caption:=rMain_MenuLoadNetwork;
        miSave.Caption:=rMain_MenuSaveNetwork;
        miState.Checked:=true;
        cbChatReceiver.Items.Clear;
        cbChatReceiver.Items.Add(rMain_ChatGroup);
        cbChatReceiver.Items.Add(rMain_ChatServer);
        cbChatReceiver.Items.Add(rMain_ChatKibitzes);
        cbChatReceiver.ItemIndex:=0;
        tsGameServer.TabVisible:=true;
        pcMessages.ActivePage:=tsGameServer;
        pnChat.Parent:=tsGameServer;
      end else
        pnChat.Parent:=tsMessages;
      Scrabble.Reset;
      Scrabble.GameState:=Scrabble.GameState+[gsNetwork];
      Scrabble.GameState:=Scrabble.GameState-[gsRunning];
      fmNewGame.NetworkMode:=true;  //disable player names
      if TCPClient.Terminated or TCPClient.Suspended then
        acNetworkExecute(nil);  //connection failed, e.g. too much players
    end;
  except
    on E:Exception do OnMessage(smError,E.Message);
  end;
end;


