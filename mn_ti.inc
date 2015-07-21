procedure TfmMain.tiMainTimer(Sender: TObject);
var
  z:word;
begin
  if assigned(Scrabble) and
     (gsRunning in Scrabble.GameState) and
    not (gsPaused in Scrabble.GameState) then
  with Scrabble.Player[Scrabble.CurrentPlayer] do
  begin
    if (gsGameEnd in Scrabble.GameState) or //in wcChallenge we need one timeout extra
       (Scrabble.IsTimeOut) then
      DoTimeOut else
    begin
      z:=round(mTimeElapsed/1000)+Scrabble.sMoveTime;
      if (Scrabble.TimeControlSetting=tcNoLimit) then
        lbTimer.Caption:=SecondsToString(z) else
        lbTimer.Caption:=SecondsToString(Scrabble.sTimeControlValue-z);
      if (pmStatusInfo.Items[2].Checked) then
        pbInfo.Repaint;
    end;//timeout
  end;//with
end;

procedure TfmMain.AddTime(const aSeconds: word);
begin
  with Scrabble.Player[Scrabble.CurrentPlayer] do
  begin
    if (Points>=Scrabble.TimePenaltyValue) then
    begin
      Scrabble.AddPlayerTime(Scrabble.CurrentPlayer,aSeconds,true);
      Scrabble.PlayerValue[Scrabble.CurrentPlayer]:=Points-Scrabble.TimePenaltyValue;
      DoRepaint(seUnknown);
    end;
  end;
end;

procedure TfmMain.DoTimeOut;
begin
  if gsComputing in Scrabble.GameState then
  begin
    if not (gsDemo in Scrabble.GameState) then
      Bruteforce.Aborted:=true;
  end else
  if (Scrabble.CurrentPlayer=Scrabble.LocalPlayer) and
     (gsRunning in Scrabble.GameState) and
      not (gsPaused in Scrabble.GameState) then
  begin
    CancelDrag;
    if (not (gsNetwork in Scrabble.GameState) or
        not (gsKibitz in Scrabble.GameState)) then
    begin
      if (Scrabble.TimeControlSetting=tcPerGame) and                                    //per game
         (Scrabble.Player[Scrabble.CurrentPlayer].TimePenaltyLeft>0) and
         not (gsGameEnd in Scrabble.GameState) then               //allowed and available
      begin
        //we need at least 1min total time of game to add another minute
        if (Scrabble.sTimeControlValue<60) then
          OnMessage(smError,rMain_AddTimeError1) else
        if (Scrabble.Player[Scrabble.CurrentPlayer].Points<Scrabble.TimePenaltyValue) then //enough points
          OnMessage(smError,rMain_AddTimeError2) else
        begin
          if (gsNetwork in Scrabble.GameState) then
            TCPClient.OnSend('nwAddTime','group','') else
            begin
              AddTime(60);
              GameCourse.AddPenalty(Scrabble.CurrentPlayer,Scrabble.CurrentMove,-Scrabble.TimePenaltyValue);
              OnMessage(smInformation,Language.Format(Language.Plural(rMain_TimePenaltyMsg,Scrabble.TimePenaltyValue,false),
                  [Scrabble.Player[Scrabble.CurrentPlayer].Name,Scrabble.TimePenaltyValue,Scrabble.Player[Scrabble.CurrentPlayer].TimePenaltyLeft]));
            end;
          exit;
        end;
      end else
      if ((Scrabble.TimeControlSetting=tcPerGame) and Scrabble.TimeGameLost) and
         not (gsGameEnd in Scrabble.GameState) and //lost by time as last move
         (Scrabble.Player[Scrabble.CurrentPlayer].Points>0) then
      begin
        Scrabble.RestoreBoard;
        OnMessage(smInformation,Language.Format(rMain_LostByTime+' ('+Language.Plural(rMain_Points,Scrabble.Player[Scrabble.CurrentPlayer].Points,true)+').',[Scrabble.Player[Scrabble.CurrentPlayer].Name]));
        GameCourse.AddPenalty(Scrabble.CurrentPlayer,Scrabble.CurrentMove{-1},-Scrabble.Player[Scrabble.CurrentPlayer].Points);
        Scrabble.PlayerValue[Scrabble.CurrentPlayer]:=0;
        if IsGameServer then
          TCPClient.OnSend('nwLostByTime','group','');
      end;

      //raise dialog for player on turn (others get normal interaction)
      if //(Scrabble.TimeControlSetting<>tcPerMove) and
         (gsGameEnd in Scrabble.GameState) and
         (Scrabble.CanChallenge) then
      begin
        Scrabble.GameState:=Scrabble.GameState+[gsDialog];
        case TimeoutMsg(rMain_Challenge,Scrabble.WordCheckPeriod) of
         mrYes:
          acChallengeExecute(self);
         mrNo,mrTimeout :
          begin
            Scrabble.HasChallenged:=crValid;
            UpdateMove;
          end;
         mrCancel : ;
        end;
        Scrabble.GameState:=Scrabble.GameState-[gsDialog];
      end else
        UpdateMove;
    end;//comp & kibitz
  end;//currentplayer & paused
end;

