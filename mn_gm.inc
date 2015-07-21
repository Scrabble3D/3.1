procedure TfmMain.acNewGameExecute(Sender: TObject);
begin
  if (gsNetwork in Scrabble.GameState) then
  begin
    if (gsGameEnd in Scrabble.GameState) or
       (TimeoutMsg(rMain_StartNewGame)=mrYes) then
      SyncNewGame;
  end else
    if (fmNewGame.ShowModal=mrOk) then
      Scrabble.NewGame;
end;

procedure TfmMain.DoNewGame(
                    out aPlayerNames: string;
                    out aRackSize: byte;
                    out aBoardSize: byte;
                    out aDimension: TScrabbleDimension;
                    out aFieldTypeArray: TFieldTypeArray;
                    out aRandSeed: LongWord;
                    out aLetters: TLetterList;
                    out aCanJokerExchange: boolean;
                    out aGameEndBonus: word;
                    out aNumberOfPasses: byte;
                    out aJokerPenalty: byte;
                    out aChangeIsPass: boolean;
                    out aTimeControl: TTimeControlSetting;
                    out aTimeControlValue: Longword;
                    out aLimitedExchange: byte;
                    out aCambioSecco: boolean;
                    out aWhatif : boolean;
                    out aAdd:boolean;
                    out aSubstract: boolean;
                    out aTimePenaltyValue: byte;
                    out aTimePenaltyCount: byte;
                    out aTimeGameLost     : boolean;
                    out aWordCheckMode    : TWordCheckMode;
                    out aWordCheckPeriod  : byte;
                    out aWordCheckPenalty : byte;
                    out aWordCheckBonus   : byte;
                    out aScrabbleBonus    : byte;
                    out aCLABBERS         : boolean);

 function StrToSeconds(aValue:string):Longword;
 var i:integer;
 begin
   Result:=0;
   i:=pos(':',aValue);
   inc(Result,StrToQWord(copy(aValue,1,i-1))*3600);
   delete(aValue,1,i);
   i:=pos(':',aValue);
   inc(Result,StrToQWord(copy(aValue,1,i-1))*60);
   delete(aValue,1,i);
   inc(Result,StrToQWord(aValue));
 end;

var
  i,j     : integer;
  aLetter : TLetter;
  s       : string;
begin
  aPlayerNames:=fmNewGame.PlayerNames;
  aRackSize:=fmGameOptions.seNumberOfLetters.Value;
  aBoardSize:=fmGameOptions.seBoardSize.Value;
  aLimitedExchange:=fmGameOptions.seLimitExchange.Value;
  aDimension:=TScrabbleDimension(integer(fmGameOptions.rbDim3D.Checked));
  aCambioSecco:=fmGameOptions.cbCambioSecco.Checked;
  aWhatif:=fmGameOptions.cbJokerization.Checked;
  aJokerPenalty:=fmGameOptions.seJokerPenalty.Value;
  aTimePenaltyValue:=fmGameOptions.seTimePenaltyValue.Value;
  aScrabbleBonus:=fmGameOptions.seScrabbleBonus.Value;
  if fmGameOptions.rbTimePenaltyEndGame.Checked then
  begin
    aTimePenaltyCount:=0
  end else
    aTimePenaltyCount:=fmGameOptions.seTimePenaltyCount.Value;

  aChangeIsPass:=fmGameOptions.cbChangeIsPass.Checked;
  aAdd:=fmGameOptions.cbAddLettersLeft.Checked;
  aSubstract:=fmGameOptions.cbSubstractLettersLeft.Checked;
  aFieldTypeArray:=fmGameOptions.FieldTypeArray;
  aRandSeed:=fmNewGame.GameSeed;
  if fmGameOptions.rbWCMTakeback.Checked then
    aWordCheckMode:=wcAsk else
    if fmGameOptions.rbWCMPoll.Checked then
      aWordCheckMode:=wcPoll else
      aWordCheckMode:=wcChallenge;
  aWordCheckPeriod:=fmGameOptions.seChallengePeriod.Value;
  aWordCheckPenalty:=fmGameOptions.seChallengePenalty.Value;
  aWordCheckBonus:=fmGameOptions.seChallengeBonus.Value;
  aCLABBERS:=fmGameOptions.cbCLABBERS.Checked;

  aNumberOfPasses:=fmGameOptions.seNumberOfPasses.Value;
  if fmGameOptions.rbNoLimit.Checked then
  begin
    aTimeControl:=tcNoLimit;
    aTimeControlValue:=MaxInt;
  end else
  if fmGameOptions.rbPerMove.Checked then
  begin
    aTimeControl:=tcPerMove;
    aTimeControlValue:=StrToSeconds(fmGameOptions.meTimeLimitPerMove.Text);
  end else
  begin
    aTimeControl:=tcPerGame;
    aTimeControlValue:=StrToSeconds(fmGameOptions.meTimeLimitPerGame.Text) div (fmNewGame.NumberOfPlayers);
  end;
  aTimeGameLost:=fmGameOptions.cbLostByTime.Checked;
  for i:=1 to fmGameOptions.sgLetters.RowCount-1 do
   for j:=1 to StrToIntDef(fmGameOptions.sgLetters.Cells[1,i],0) do
   if fmGameOptions.sgLetters.Cells[0,i]<>'' then
   begin
     aLetter:=TLetter.Create;
     aLetter.What:=UTF8Decode(fmGameOptions.sgLetters.Cells[0,i])[1];
     aLetter.Value:=StrToIntDef(fmGameOptions.sgLetters.Cells[2,i],0);
     {%H-}aLetters.Add(aLetter);
   end;
  Seed:=aRandSeed;
  s:='';
  if (fmGameOptions.sgLetters.RowCount>1) then
  for i:=0 to fmGameOptions.seNumberOfRandomLetters.Value-1 do
  begin
    aLetter:=TLetter.Create;
    j:=genrandom(fmGameOptions.sgLetters.RowCount-1)+1;
    s:=s+inttostr(j)+',';
    aLetter.What:=UTF8Decode(fmGameOptions.sgLetters.Cells[0,j])[1];
    aLetter.Value:=StrToIntDef(fmGameOptions.sgLetters.Cells[2,j],0);
    aLetter.IsRandom:=true;
    aLetters.Add(aLetter);
  end;
  if (fmGameOptions.sgLetters.RowCount>1) then
  for i:=0 to fmGameOptions.seNumberOfJokers.Value-1 do
  begin
    aLetter:=TLetter.Create;
    aLetter.What:=ltJoker;
    aLetter.IsJoker:=true;
    aLetters.Add(aLetter);
  end;
  aCanJokerExchange:=fmGameOptions.cbCanJokerExchange.Checked;
  aGameEndBonus:=fmGameOptions.seGameEndBonus.Value;
end;

procedure TfmMain.DoAfterNewGame(Sender: TObject);
var
  i:integer;
begin
  ScrabbleGrid.BoardSize:=Scrabble.BoardSize;
  DoConfigChange(self);
  case Scrabble.Dimension of
   D2 : begin
          if assigned(fm3D) and fm3D.Visible then
            fm3D.Close;

          {$if Defined(UseOpenGl) and Defined(Darwin)}
          ScrabbleCube.MakeCurrent;
          ScrabbleCube.Parent:=nil;
          {$endif}
          pn3D.Visible:=false;
          spRight.Visible:=false;
          {$if Defined(UseOpenGl) and Defined(Darwin)}
          ScrabbleCube.Parent:=pn3D;
          {$endif}

          {$ifdef UseOpenGl}
          if ScrabbleGrid is TGlScrabbleGrid then
            TGlScrabbleGrid(ScrabbleGrid).RefreshLetters;
          {$endif}
        end;
   D3 : begin
          pn3D.Visible:=true;
          spRight.Visible:=true;
          tb3D.Visible:=fmGameOptions.cbToolbar3D.Checked;
          {$ifdef Windows}
          ScrabbleCube.BkColor:=fmGameOptions.cbBackGroundColor.ButtonColor;
          {$endif}
          {$ifdef UseOpenGl}
          ScrabbleCube.ClearTextures;
          {$endif}
        end;
  end;
  GameCourse.Clear;
  GameCourse.Strikeout:=(Scrabble.WordCheckMode<>wcChallenge);
  if not (gsKibitz in Scrabble.GameState) then
    OnMessage(smInformation,rMain_InfoNewGameStarted);
  if (Dictionary.Info[itWordCount]=0) then
    OnMessage(smWarning,rMain_LoadDictionary);
  if gsNetwork in Scrabble.GameState then
  begin
    for i:=0 to Scrabble.NumberOfPlayers-1 do
     if Scrabble.Player[i].Name=TCPClient.PlayerData.PlayerName then
      Scrabble.LocalPlayer:=i;
    if pcMessages.ActivePage=pcMessages.Pages[3] then
      pcMessages.ActivePage:=pcMessages.Pages[0];
  end;
  Scrabble.GameState:=Scrabble.GameState+[gsRunning];
  ScrabbleGrid.Paint(self); //update<>repaint
  fmWordSearch.Mode:=TWordSearchMode(integer(Scrabble.WordCheckMode));

  DoAfterNextPlayer(self);//place move if computer
  //start paused on local game
  if not (gsNetwork in Scrabble.GameState) and
     not (gsDemo in Scrabble.GameState) and
     (Scrabble.Player[Scrabble.CurrentPlayer].Name<>'Computer') and
     fmNewGame.cbAutoPause.Checked then
    acPause.Execute;
end;

procedure TfmMain.acNextPlayerExecute(Sender: TObject);
var
  LastError,OriginalLastError:TLastError;
  done:boolean;
begin
  if not (gsPaused in Scrabble.GameState) and //pause to avoid timer event (pieces are not hidden)
     not (gsNextPlayer in Scrabble.GameState) then //avoid double clicks
  try
    if not (gsPaused in Scrabble.GameState) then
       Scrabble.TogglePaused;

    //really pass? if no letters have been placed, no demo or comp active, and no timeout
    if (Scrabble.MoveState=msNone) and
       not (gsDemo in Scrabble.GameState) and
       (Scrabble.Player[Scrabble.CurrentPlayer].Name<>'Computer') and
       not Scrabble.IsTimeout and
       (TimeoutMsg(rMain_ReallyPass)=mrNo) then
    begin
      if (gsPaused in Scrabble.GameState) then
        Scrabble.TogglePaused;
      DoRepaint(seReset);
      exit;
    end;

    Scrabble.GameState:=Scrabble.GameState+[gsNextPlayer];
    done:=false; //done will be true for valid moves or in case of wordcheckmode or timeout

    //check move; callback to Checkwords() for unknown words if mode<>challenge
    LastError:=Scrabble.CheckMove(Scrabble.WordCheckMode<>wcChallenge);
    if (Scrabble.WordCheckMode=wcChallenge) and (LastError=leUnknownWord) then
      LastError:=leNone;
    //report errors
    case LastError of
     leNone,leNoLetter : done:=true;
     leUnknownWord     : OnMessage(smWarning,Language.Format(Language.Plural(rMain_ErrorUnknownWords,Scrabble.LastMove.UnknownWords,false),[Dictionary.ReplaceDigraphs(Scrabble.LastMove.UnknownWords)]));
     leFirstMove       : OnMessage(smWarning,rMain_ErrorFirstMove);
     leDimension       : OnMessage(smWarning,rMain_ErrorDimension);
     leConnection      : OnMessage(smWarning,rMain_ErrorConnection);
     leSuccessive      : OnMessage(smWarning,rMain_ErrorSuccessive);
     leLowScore        : OnMessage(smWarning,rMain_ErrorLowScore+' '+Language.Plural(rMain_Points,Scrabble.LeastValue,true));
    end;
    OriginalLastError:=LastError;
    //force move
    if not done and
       (Scrabble.IsTimeOut or
        ((LastError in [leUnknownWord,leLowScore]) and (Scrabble.WordCheckMode<>wcAsk))
       ) then
    begin
      Scrabble.RestoreBoard;                //remove letters
      LastError:=Scrabble.CheckMove(false); //update lastmove
      done:=true;
    end;

    if done then
    begin
      if (gsNetwork in Scrabble.GameState) then
      begin
        if not (gsLoading in Scrabble.GameState) then
          SyncNextPlayer(LastError,OriginalLastError);
        TCPClient.OnSend('nwRefresh','all','');
      end else
        Scrabble.NextPlayer;
    end else
    begin
      Scrabble.GameState:=Scrabble.GameState-[gsNextPlayer];//if done flag is removed in scrabble.nextplayer
      if (gsPaused in Scrabble.GameState) and not fmNewGame.cbAutoPause.Checked then //unpause at finally
        Scrabble.TogglePaused;
    end;
    alMainUpdate(nil,done);
  finally
    if done and
       not (gsNetwork in Scrabble.GameState) and
       not (gsDemo in Scrabble.GameState) and
       (Scrabble.Player[Scrabble.CurrentPlayer].Name<>'Computer') and
       fmNewGame.cbAutoPause.Checked then
      acPause.Execute;
  end;
end;

procedure TfmMain.DoAfterNextPlayer(Sender: TObject);
 function PlacedWords:string;
 begin
   Result:=Dictionary.ReplaceDigraphs(Scrabble.LastMove.PlacedWord);
   if Scrabble.LastMove.ConnectedWords<>'' then
     Result:=Result+'+'+Dictionary.ReplaceDigraphs(Scrabble.LastMove.ConnectedWords);
   if (Result<>'') and (Result[length(Result)]=',') then
     Delete(Result,length(Result),1);
 end;
 function PlacedPosition:string;
 begin
   Result:=Scrabble.PosToString(dy,Scrabble.LastMove.UsedLetters[0].Where[dy],fmGameOptions.cbRoman.Checked)+
           Scrabble.PosToString(dx,Scrabble.LastMove.UsedLetters[0].Where[dx],fmGameOptions.cbRoman.Checked);
   if Scrabble.Dimension=D3 then
     Result:=Result+Scrabble.PosToString(dz,Scrabble.LastMove.UsedLetters[0].Where[dz],fmGameOptions.cbRoman.Checked);
   case Scrabble.LastMove.Dimension of
    dx:Result:=Result+'x';
    dy:Result:=Result+'y';
    dz:Result:=Result+'z';
   end;
 end;
 function PlacedTime:string;
 begin
   Result:='';
   if Scrabble.TimeControlSetting=tcPerGame then
     Result:=' ('+SecondsToString(Scrabble.sTimeControlValue-round(Scrabble.Player[(Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers].mTimeElapsed/1000))+' '+rMain_TimeLeft+')';
 end;
 function GameScore:string;
 var
   i:integer;
 begin
   Result:='(';
   for i:=0 to Scrabble.NumberOfPlayers-1 do
     Result:=Result+Scrabble.Player[i].Name+'='+inttostr(Scrabble.Player[i].Points)+', ';
   //replace last comma
   Result:=copy(Result,1,length(Result)-2);
   Result:=Result+')';
 end;

var
  s:string;
  b:boolean;
  i:integer;
begin
  //basic move data
  if length(Scrabble.LastMove.UsedLetters)>0 then
  begin
    s:=rMain_MoveResult+' '+Language.Plural(rMain_Points,Scrabble.LastMove.Value,true)+'%s.';
    s:=Language.Format(s,[Scrabble.Player[(Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers].Name,
                          PlacedWords,
                          PlacedPosition,
                          PlacedTime]);
    if fmGameOptions.cbShowScore.Checked then
      s:=s+LineBreak+GameScore;
    OnMessage(smInformation,s);
    if Scrabble.LastMove.IsScrabble and
       not (gsLoading in Scrabble.GameState) then
    begin
      if (Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers=Scrabble.LocalPlayer then
        ScrabblePlaySound(smScrabble_self) else
        ScrabblePlaySound(smScrabble_other,false);
    end;
  end else //not letters
  if (Scrabble.CurrentMove>0) then //0 on game start
  with GameCourse.History[Scrabble.CurrentMove-1] do
  begin
    s:=ExchangedLetters;
    if s='' then
      s:=rMain_NoLetters else
      s:=Language.Plural(rMain_LettersExchanged,UTF8Length(s));
    s:=s+' '+PlacedTime;
    OnMessage(smInformation,s);
  end;
  //user information
  if not (gsGameEnd in Scrabble.GameState) and (Scrabble.NoLettersPlaced>0) then
  begin
    i:=Scrabble.NumberOfPlayers*Scrabble.NumberOfPasses-Scrabble.NoLettersPlaced;
    OnMessage(smInformation,Language.Plural(rMain_NumberOfPasses,i));
  end;
  //override scrabble.localplayer in local games
  if (fmGameOptions.cbShowComputer.Checked or (Scrabble.Player[Scrabble.CurrentPlayer].Name<>'Computer')) and
     (not (gsNetwork in Scrabble.GameState) or (gsKibitz in Scrabble.GameState)) then
    Scrabble.LocalPlayer:=Scrabble.CurrentPlayer;
  //update pnPieces header before computer runs
  alMainUpdate(nil,b);
  //clear and update/repaint
  Bruteforce.ClearMove;
  DoRepaint(seNextPlayer);
  if (gsNetwork in Scrabble.GameState) and
     (Scrabble.Player[Scrabble.CurrentPlayer].Name=TCPClient.PlayerData.PlayerName) and
     not (gsGameEnd in Scrabble.GameState) and
     not (gsLoading in Scrabble.GameState) then
  begin
    ScrabblePlaySound(smOwnMove);
    OnMessage(smOwnMove,rMain_OwnMove);
  end;

  //statistics
  if (Scrabble.CurrentMove>0) and //0 on game start
     not (gsDemo in Scrabble.GameState) and
     not (gsLoading in Scrabble.GameState) and
    (not (gsNetwork in Scrabble.GameState) or (Scrabble.LocalPlayer=Scrabble.CurrentPlayer)) then
    fmStatistics.AddValues(Scrabble.LastMove.Value,
                           round(Scrabble.Player[(Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers].mTimeElapsed/1000),
                           Scrabble.Player[(Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers].Name);

  //compute move
  if not (gsGameEnd in Scrabble.GameState) and
     not (gsNetwork in Scrabble.GameState) and
     not (gsLoading in Scrabble.GameState) and
     not Scrabble.IsTimeOut and
     (Scrabble.Player[Scrabble.CurrentPlayer].Name='Computer') then
  begin
    if not Scrabble.FirstMove and
       ((Scrabble.WordCheckMode=wcChallenge) and (Random>fmGameOptions.tbCompTrust.Position/100)) then
      acChallenge.Execute;
    ComputeMove;
  end;
end;

procedure TfmMain.acPauseExecute(Sender: TObject);
begin
  //running demo needs more attention
  if (gsDemo in Scrabble.GameState) then
  begin
    Scrabble.TogglePaused;
    Bruteforce.Paused:=not Bruteforce.Paused;
    if Bruteforce.Paused then
       OnMessage(smInformation,rMain_DemoPause) else
       OnMessage(smInformation,rMain_DemoUnPause)
  end else
  //local game or setup after poll
  if not (gsNetwork in Scrabble.GameState) or (Sender=nil) then
  begin
    Scrabble.TogglePaused;
    ScrabbleGrid.Paint(self);
    UpdatePieceList;
  end else
  //poll mates
  begin
    Poll.Init(TCPClient.Players[TCPCLient.PlayerData.PlayerName].Mates);
    if (gsPaused in Scrabble.GameState) then
      TCPClient.OnSend('nwPoll','group',
                     'Content='+TCPClient.PlayerData.PlayerName+nwDelimiter+
                     'Question=would like to resume running game. Accept?'+nwDelimiter+
                     'PollContent='+inttostr(integer(pcUnPause))) else
      TCPClient.OnSend('nwPoll','group',
                     'Content='+TCPClient.PlayerData.PlayerName+nwDelimiter+
                     'Question=would like to pause the game. Accept?'+nwDelimiter+
                     'PollContent='+inttostr(integer(pcPause)));
    if Poll.Waitfor(true) then
      TCPClient.OnSend('nwPause','group','');
  end;
end;

function TfmMain.LoadFrom(aFileName:string):boolean;
 function SecondsToStr(aValue:LongWord):string;
 var
   i:integer;
   s:string;
 begin
   //h
   i:=aValue div 3600;
   Result:=inttostr(i)+':';
   aValue:=aValue{%H-}-(i*3600);
   //min
   i:=aValue div 60;
   s:=inttostr(i);
   if length(s)<2 then
     Result:=Result+'0'+s+':' else
     Result:=Result+s+':';
   aValue:=aValue{%H-}-(i*60);
   //s
   s:=inttostr(aValue);
   if length(s)<2 then
     Result:=Result+'0'+s else
     Result:=Result+s;
 end;

var
  s:string;
  aChar:WideChar;
  x,y,z:byte;
  i,j,nJoker,nRandom:integer;
  aFieldTypeArray:TFieldTypeArray;
  aLetter:TLetter;
begin
  Result:=false;
  //load game
  Scrabble.LoadFrom(aFileName);
  //adjust settings
  with fmGameOptions do
  begin
    //letter set should fit loaded game to present available letters for jokers
    fmGameOptions.sgLetters.RowCount:=1;
    j:=0; aChar:=#0;nJoker:=0;nRandom:=0;
    for i:=0 to Scrabble.Letters.Count-1 do
    begin
      aLetter:=TLetter(Scrabble.Letters[i]);
      if (aLetter.What<>aChar) and
         (not aLetter.IsJoker) and
         (not aLetter.IsRandom) or
         (i=Scrabble.Letters.Count-1) then
      begin
         if j>0 then
         begin
           fmGameOptions.sgLetters.RowCount:=fmGameOptions.sgLetters.RowCount+1;
           fmGameOptions.sgLetters.Cells[0,fmGameOptions.sgLetters.RowCount-1]:=UTF8Encode(widestring(aChar));
           fmGameOptions.sgLetters.Cells[1,fmGameOptions.sgLetters.RowCount-1]:=inttostr(j);
           fmGameOptions.sgLetters.Cells[2,fmGameOptions.sgLetters.RowCount-1]:=inttostr(z{%H-});
         end;
         j:=1;
         z:=aLetter.Value;
         aChar:=aLetter.What;
      end else
      if (aLetter.What<>ltJoker) then
        inc(j);
      if aLetter.IsJoker then inc(nJoker);
      if aLetter.IsRandom then inc(nRandom);
    end;
    fmGameOptions.seNumberOfJokers.Value:=nJoker;
    fmGameOptions.seNumberOfRandomLetters.Value:=nRandom;
    with TIniFile.Create(aFileName) do
    try
      if ReadBool('General','LeftToRight',true) then
        fmGameOptions.rbLeftToRight.Checked:=true else
        fmGameOptions.rbRightToLeft.Checked:=true;
    finally
      Free;
    end;
    UpdateLettersetInfo(nil);

    case Scrabble.WordCheckMode of
      wcAsk:rbWCMTakeback.Checked:=true;
      wcPoll:rbWCMPoll.Checked:=true;
      wcChallenge:rbWCMChallenge.Checked:=true;
    end;//case
    WCMUpdate(nil); //enable edit fields
    if Scrabble.WordCheckMode=wcChallenge then
    begin
      seChallengePeriod.Value:=Scrabble.WordCheckPeriod;
      seChallengePenalty.Value:=Scrabble.WordCheckPenalty;
      seChallengeBonus.Value:=Scrabble.WordCheckBonus;
    end;
    case Scrabble.TimeControlSetting of
     tcNoLimit:rbNoLimit.Checked:=true;
     tcPerMove:rbPerMove.Checked:=true;
     tcPerGame:rbPerGame.Checked:=true;
    end;
    rbTimeControlChange(self);
    if Scrabble.TimeControlSetting=tcPerMove then
      meTimeLimitPerMove.Text:=SecondsToStr(Scrabble.sTimeControlValue*Scrabble.NumberOfPlayers);
    if Scrabble.TimeControlSetting=tcPerGame then
      meTimeLimitPerGame.Text:=SecondsToStr(Scrabble.sTimeControlValue*Scrabble.NumberOfPlayers);
    if Scrabble.TimePenaltyCount=0 then
      rbTimePenaltyEndGame.Checked:=true else
    begin
      rbTimePenaltyBuyTime.Checked:=true;
      seTimePenaltyCount.Value:=Scrabble.TimePenaltyCount;
      seTimePenaltyValue.Value:=Scrabble.TimePenaltyValue;
    end;
    seNumberOfLetters.Value:=Scrabble.RackSize;
    seBoardSize.Value:=Scrabble.BoardSize;
    if (Scrabble.Dimension=D2) then
      rbDim2D.Checked:=true else
      rbDim3D.Checked:=true;
    SetLength(aFieldTypeArray,seBoardSize.Value,seBoardSize.Value,seBoardSize.Value);
    try
      for x:=0 to seBoardSize.Value-1 do
       for y:=0 to seBoardSize.Value-1 do
        for z:=0 to seBoardSize.Value-1 do
         aFieldTypeArray[x,y,z]:=Scrabble.FieldType[x,y,z];
      fmGameOptions.FieldtypeArray:=aFieldTypeArray;
    finally
      SetLength(aFieldTypeArray,0,0,0);
    end;
    cbCanJokerExchange.Checked:=(gsJokerExchange in Scrabble.GameState);
    cbJokerization.Checked:=(gsJokerization in Scrabble.GameState);
    seGameEndBonus.Value:=Scrabble.GameEndBonus;
    seNumberOfPasses.Value:=Scrabble.NumberOfPasses;
    seJokerPenalty.Value:=Scrabble.JokerPenalty;
    cbChangeIsPass.Checked:=(gsChangeIsPass in Scrabble.GameState);
    cbCLABBERS.Checked:=(gsCLABBERS in Scrabble.GameState);
    seLimitExchange.Value:=Scrabble.LimitedExchange;
    cbCambioSecco.Checked:=(gsCambioSecco in Scrabble.GameState);
    cbAddLettersLeft.Checked:=(gsAddLettersLeft in Scrabble.GameState);
    cbSubstractLettersLeft.Checked:=(gsSubstractLettersLeft in Scrabble.GameState);
    seScrabbleBonus.Value:=Scrabble.ScrabbleBonus;
  end;

  ScrabbleGrid.BoardSize:=Scrabble.BoardSize;
  DoConfigChange(self);
  case Scrabble.Dimension of
   D2 : begin
          if assigned(fm3D) and fm3D.Visible then
            fm3D.Close;
          pn3D.Visible:=false;
          spRight.Visible:=false;
        end;
   D3 : begin
          pn3D.Visible:=true;
          spRight.Visible:=true;
          tb3D.Visible:=fmGameOptions.cbToolbar3D.Checked;
          {$ifdef Windows}
//          ScrabbleCube.BkColor:=fmGameOptions.cbBackGroundColor.ButtonColor;
          {$endif}
          ScrabbleCube.Invalidate;
        end;
  end;
  Bruteforce.ClearMove;
  GameCourse.LoadFrom(aFileName);
  GameCourse.Strikeout:=(Scrabble.WordCheckMode<>wcChallenge) or (gsGameEnd in Scrabble.GameState);
  with TInifile.Create(aFileName) do
  try
    s:=ReadString('General','Dictionary','');
    if s<>Dictionary.Info[itFileName] then
      OnMessage(smWarning,rMain_OtherDictionary+' '+s);
  finally
    Free;
  end;
  UpdateScore;
  Result:=true;
end;

procedure TfmMain.SaveTo(aFileName:string);
begin
  with TInifile.Create(UTF8ToSys(aFileName)) do
  try
    EraseSection('General');
    WriteString('General','Dictionary',Dictionary.Info[itFileName]);
    WriteDateTime('General','Date',Now);
    WriteBool('General','LeftToRight',fmGameOptions.rbLeftToRight.Checked);
  finally
    Free;
  end;
  Scrabble.SaveTo(aFileName);
  GameCourse.SaveTo(aFileName);
end;

procedure TfmMain.DoGameEnd(Sender: TObject);
var
  aResult : array of integer;
  aLetter : TLetter;
  i,j,k,n : integer;
  s,aMsg  : string;
begin
  Scrabble.GameState:=Scrabble.GameState-[gsRunning];
  Scrabble.GameState:=Scrabble.GameState+[gsGameEnd];
  OnMessage(smInformation,rMain_GameEnd);
  //game end bonus if all letters have been placed
  if (Scrabble.NumberOfLettersLeft=0) and (Scrabble.GameEndBonus>0) then
  begin
    i:=(Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers;
    k:=0;
    for j:=0 to Scrabble.RackSize-1 do
     if Scrabble.RackLetter[i,j]<>nil then
        inc(k);
    if k=0 then
    begin
      Scrabble.PlayerValue[i]:=Scrabble.Player[i].Points+Scrabble.GameEndBonus;
      GameCourse.AddPenalty(i,Scrabble.CurrentMove-1,Scrabble.GameEndBonus);
      OnMessage(smInformation,Language.Format(Language.Plural(rMain_GameEndBonus,Scrabble.GameEndBonus,false),[Scrabble.Player[i].Name,Scrabble.GameEndBonus]));
    end;
  end;
  setlength(aResult,Scrabble.NumberOfPlayers);
  try
    aMsg:='';
    //calculate values to subtract
    for i:=0 to Scrabble.NumberOfPlayers-1 do
    begin
      aResult[i]:=0;
      s:='';
      for j:=0 to Scrabble.RackSize-1 do
      begin
        aLetter:=Scrabble.RackLetter[i,j];
        if (aLetter<>nil) and (aLetter.State=lsRack) then
        begin
          inc(aResult[i],aLetter.Value);
          s:=s+UTF8Encode(widestring(aLetter.What));
        end;
      end;
      if s<>'' then
        aMsg:=Language.Format(rMain_GameEndLetterLeft,[Scrabble.Player[i].Name,Dictionary.ReplaceDigraphs(s)])+LineBreak+aMsg;
    end;

    if (gsSubstractLettersLeft in Scrabble.GameState) then
    begin
      //subtract letters left on Rack
      for i:=0 to Scrabble.NumberOfPlayers-1 do
      begin
        k:=0; n:=0;
        for j:=0 to Scrabble.RackSize-1 do
         if (Scrabble.RackLetter[i,j]<>nil) then
        begin
          inc(n);
          if (Scrabble.RackLetter[i,j].IsJoker) then
            inc(k,Scrabble.JokerPenalty);
        end;
        if n>0 then
        begin
          if k>0 then
          begin
            GameCourse.AddPenalty(i,Scrabble.CurrentMove-1,-k);
            s:=rMain_GameEndMinusJoker+' '+Language.Plural(rMain_Points,Scrabble.Player[i].Points-aResult[i]-k,true);
            aMsg:=Language.Format(s,
                            [Scrabble.Player[i].Name,
                             Scrabble.Player[i].Points,
                             aResult[i],k])+LineBreak+aMsg
          end else
          begin
            s:=rMain_GameEndMinus+' '+Language.Plural(rMain_Points,Scrabble.Player[i].Points-aResult[i],true);
            aMsg:=Language.Format(s,
                            [Scrabble.Player[i].Name,
                             Scrabble.Player[i].Points,
                             aResult[i]])+LineBreak+aMsg;
          end;
          Scrabble.PlayerValue[i]:=Scrabble.Player[i].Points-(aResult[i]+k);
          if i>0 then inc(aResult[0],aResult[i]); //sum
        end;
      end;
      //add opponents letters value
      if (gsAddLettersLeft in Scrabble.GameState) and (Scrabble.NumberOfLettersLeft=0) then
      begin
        //last move's player
        k:=(Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers;
        //if all letter were played
        j:=0;
        for i:=0 to Scrabble.RackSize-1 do
         if Scrabble.RackLetter[k,i]<>nil then
          inc(j);
        if (j=0) then
         with Scrabble.Player[k] do
         begin
           s:=rMain_GameEndPlus+' '+Language.Plural(rMain_Points,Scrabble.Player[k].Points+aResult[0],true);
           aMsg:=Language.Format(s,[Name,Points,aResult[0]])+LineBreak+aMsg;
           Scrabble.PlayerValue[k]:=Points+aResult[0];
         end;
      end;
    end;

    ScrabblePlaySound(smGameResult);
    if (Scrabble.NumberOfPlayers>1) and
        not (gsDemo in Scrabble.GameState) then
    begin
      j:=-1;
      for i:=1 to Scrabble.NumberOfPlayers-1 do
       if Scrabble.Player[i].Points<>Scrabble.Player[i-1].Points then j:=0;
      if j=0 then
       for i:=1 to Scrabble.NumberOfPlayers-1 do
        if Scrabble.Player[i].Points>Scrabble.Player[j].Points then j:=i;
      if j=-1 then
        OnMessage(smGameResult,rMain_GameEndDraw+LineBreak+aMsg) else
        OnMessage(smGameResult,Language.Format(rMain_GameEndWin,[Scrabble.Player[j].Name])+LineBreak+aMsg);
    end;
    GameCourse.Strikeout:=true;
    //statistics
    if not (gsDemo in Scrabble.GameState) then
    begin
      if not (gsNetwork in Scrabble.GameState) then
      begin
        j:=0;k:=0;
        for i:=0 to Scrabble.NumberOfPlayers-1 do
         fmStatistics.AddValues(Scrabble.Player[i].Points,0,Scrabble.Player[i].Name,true);
      end else
      if not (gsLoading in Scrabble.GameState) then
        fmStatistics.AddValues(Scrabble.Player[Scrabble.LocalPlayer].Points,0,Scrabble.Player[Scrabble.LocalPlayer].Name,true);
    end;
    UpdateScore;//pbScore.Repaint;
    fmWordSearch.Mode:=wmAll;
    //sync game end
    if (gsNetwork in Scrabble.GameState) and IsGameServer and
        not (gsKibitz in Scrabble.GameState) and
        not (gsLoading in Scrabble.GameState) and
        (Sender<>nil) then  //DoInformation > LoadingFinished > gsGameEnd > IsGameEnd: self/nil
      SyncGameEnd;
  finally
    setlength(aResult,0);
  end;
end;

procedure TfmMain.acChallengeExecute(Sender: TObject);
var
  s:string;
  v:boolean;
begin
  tbChallenge.Enabled:=false;
  v:=DoCheckWords(Scrabble.LastMove.PlacedWord+','+Scrabble.LastMove.ConnectedWords,s,true);
  if not (gsNetwork in Scrabble.GameState) then
    DoChallenge(v,s,Scrabble.LocalPlayer) else
    TCPClient.OnSend('nwChallenge','group','MoveNumber='+inttostr(Scrabble.ActualMove)+nwDelimiter+'Result='+booltostr(v,true)+nwDelimiter+'Invalid='+s);
end;

procedure TfmMain.DoChallenge(aIsValid:boolean; aInvalid:string; aWho: byte);
var
  i:integer;
begin
  if aIsValid then
  begin
    Scrabble.HasChallenged:=crValid;
    OnMessage(smWarning,Language.Format(Language.Plural(rMain_AllWordsValid,Scrabble.WordCheckPenalty,false),[Scrabble.WordCheckPenalty,Scrabble.Player[aWho].Name]));
    Scrabble.PlayerValue[aWho]:=Scrabble.Player[aWho].Points-Scrabble.WordCheckPenalty;
    GameCourse.AddPenalty(aWho,Scrabble.CurrentMove-1,-Scrabble.WordCheckPenalty);
    UpdateScore;//pbScore.Repaint;
  end else
  begin
    Scrabble.HasChallenged:=crInvalid;
    OnMessage(smWarning,Language.Format(Language.Plural(rMain_ErrorUnknownWords,aInvalid,false),[Dictionary.ReplaceDigraphs(aInvalid)]));
    Scrabble.Takeback;
    i:=(Scrabble.CurrentMove-1) mod Scrabble.NumberOfPlayers;
    Scrabble.PlayerValue[i]:=Scrabble.Player[i].Points-GameCourse.History[GameCourse.HistoryCount-1].Value;
    GameCourse.Takeback;
    if Scrabble.WordCheckBonus>0 then
    begin
      OnMessage(smWarning,Language.Format(Language.Plural(rMain_WordInvalid,Scrabble.WordCheckBonus,false),[Scrabble.WordCheckBonus,Scrabble.Player[aWho].Name]));
      GameCourse.AddPenalty(aWho,Scrabble.CurrentMove-1,Scrabble.WordCheckBonus);
      Scrabble.PlayerValue[aWho]:=Scrabble.Player[aWho].Points+Scrabble.WordCheckBonus;
      UpdateScore;//pbScore.Repaint;
    end;
    if (Scrabble.NumberOfPlayers*Scrabble.NumberOfPasses)>Scrabble.NoLettersPlaced then
    begin
      i:=Scrabble.NumberOfPlayers*Scrabble.NumberOfPasses{%H-}-Scrabble.NoLettersPlaced;
      OnMessage(smInformation,Language.Plural(rMain_NumberOfPasses,i));
    end;
  end;
  //game end or timeout
  if (aWho=Scrabble.LocalPlayer) then
    UpdateMove;
end;

function TfmMain.DoAskForJoker: WideChar;
const
  PiecesPerRow=7;
  PieceSize=30;
var
  i : integer;
  p : TPoint;
  aAvailLetters : string;
  aForm : TForm;
  aButton : array of TButton;
begin
  aForm:=TForm.Create(nil);
  try
    aForm.Parent:=nil;
    aForm.BorderIcons:=[];
    aForm.BorderStyle:=bsToolWindow;
    aForm.FormStyle:=fsStayOnTop;
    aForm.Caption:=rMain_JokerReplace;
    aAvailLetters:=fmGameOptions.AvailableLetters;
    p:=Mouse.CursorPos;
    aForm.SetBounds(p.x,p.y,PiecesPerRow*(PieceSize+1),(UTF8Length(aAvailLetters) div PiecesPerRow)*(PieceSize+1)+PieceSize+1);
    setlength(aButton,UTF8Length(aAvailLetters));
    for i:=0 to UTF8Length(aAvailLetters)-1 do
    begin
      aButton[i]:=TButton.Create(aForm);
      aButton[i].Parent:=aForm;
      aButton[i].SetBounds((i mod PiecesPerRow)*(PieceSize+1),(i div PiecesPerRow)*(PieceSize+1),PieceSize,PieceSize);
      aButton[i].Caption:=Dictionary.ReplaceDigraphs(UTF8Copy(aAvailLetters,i+1,1));
      aButton[i].ModalResult:=i+10;
    end;
    aForm.ModalResult:=mrNone;
    i:=aForm.ShowModal;
    if (i-9>0) and (i-9<=UTF8Length(aAvailLetters)) then
      Result:=UTF8Decode(UTF8Copy(aAvailLetters,i-9,1))[1] else
      OnMessage(smError,'Letter selection failed');
  finally
    for i:=0 to length(aButton)-1 do
      aButton[i].Free;
    aForm.Free;
  end;
end;

function TfmMain.DoCheckWords(const aValue: string; var aNotFound: string; DoAsk: boolean): boolean;
var
  i,j: integer;
  b: boolean;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    //silent check for unkown words
    sl.CommaText:=aValue;
    aNotFound:='';
    for i:=0 to sl.Count-1 do
    if (sl[i]<>'') then
    begin
      if (gsNetwork in Scrabble.GameState) and
         (Scrabble.NumberOfPlayers>1) and
         DoAsk then //CheckMove(ask=false) is called on placing letters to calculate points
      begin
        Poll.Init(TCPClient.Players[TCPClient.PlayerData.PlayerName].Mates);
        TCPClient.OnSend('nwCheckWord','group','Word='+sl[i]);
        if not Poll.Waitfor(false) then
          aNotFound:=aNotFound+sl[i]+',';
      end else
      begin
        if (gsCLABBERS in Scrabble.GameState) then
        begin
          with TStringList.Create do
          try
            b:=false;
            CommaText:=Dictionary.WordsByLetters(sl[i]);
            for j:=0 to Count-1 do
            if (UTF8Length(Strings[j])=UTF8Length(sl[i])) then
            begin
              b:=true;
              break;
            end;
            if not b then
              aNotFound:=aNotFound+sl[i]+',';
          finally
            Free;
          end;
        end else
          if (Dictionary.IsWordInDictionary(sl[i],j)<>frInclude) then
            aNotFound:=aNotFound+sl[i]+',';
      end;
    end;

    //delete trailing ,
    if length(aNotFound)>0 then
      System.Delete(aNotFound,length(aNotFound),1);
  finally
    sl.Free;
  end;

  //explicit poll
  if DoAsk and (length(aNotFound)>0) then
  begin
    aNotFound:=Dictionary.ReplaceDigraphs(aNotFound);
    if not (gsNetwork in Scrabble.GameState) or (Scrabble.NumberOfPlayers=1) then
    begin
      if (TimeoutMsg(Language.Format(Language.Plural(rMain_UnknownWords,aNotFound,false),[aNotFound]))=mrYes) then
        aNotFound:='';
    end else
    if not ((TCPClient.PlayerData.Mates.Count=1) or
            ((Scrabble.WordCheckMode=wcAsk) and not Scrabble.IsTimeOut)
           ) or
           (MessageDlgNoFocus(Language.Format(Language.Plural(rMain_UnknownWords,aNotFound,false),[aNotFound]),
                              mtConfirmation,[mbYes, mbNo])=mrYes) then
    begin
      Poll.Init(TCPClient.Players[TCPCLient.PlayerData.PlayerName].Mates);
      TCPClient.OnSend('nwPoll','group',
                     'Content='+aNotFound+nwDelimiter+
                     'Question=is/are not found in dictionary. Accept anyway?'+nwDelimiter+
                     'PollContent='+inttostr(integer(pcNotFound))+nwDelimiter+
                     'PollAll='+booltostr(Scrabble.WordCheckMode<>wcAsk,true));
      if Poll.Waitfor(true) then
        aNotFound:='';
    end;
  end;

  //result
  Result:=(length(aNotFound)=0);
end;

procedure TfmMain.UpdateMove;
begin
  if not (gsKibitz in Scrabble.GameState) then
  begin
    if ((gsGameEnd in Scrabble.GameState) and not (Scrabble.HasChallenged in [crNone,crChecked])) or
       (Scrabble.NoLettersPlaced>=Scrabble.NumberOfPlayers*Scrabble.NumberOfPasses) then
    begin
      if (gsNetwork in Scrabble.GameState) and
         not (gsLoading in Scrabble.GameState) then
        TCPClient.OnSend('nwEndGame','group','') else
        DoGameEnd(self);
    end else
    if (Scrabble.CurrentPlayer=Scrabble.LocalPlayer) then
    begin
      if Scrabble.IsTimeout and not (gsGameEnd in Scrabble.GameState) then
        acNextPlayerExecute(self);
    end; //gameend
  end;
end;


