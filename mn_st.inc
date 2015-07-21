{ ----- status bar ----- }

procedure TfmMain.DoProgress(Sender: TObject; const Value: byte);
begin
  if Value<100 then
  begin
    pbProgress.Position:=Value;
    Application.ProcessMessages;
  end else
    pbProgress.Position:=0;
end;

procedure TfmMain.pbInfoMouseEnter(Sender: TObject);
var
  s: string;
  i: integer;
begin
  if gsRunning in Scrabble.GameState then
  begin
    s:=rMain_SBLettersLeft+LineBreak+Dictionary.ReplaceDigraphs(Scrabble.LettersLeft)+LineBreak+LineBreak;

    s:=s+rMain_SBTimeLeft+LineBreak;
    if Scrabble.TimeControlSetting=tcPerGame then
     for i:=0 to Scrabble.NumberOfPlayers-1 do
      s:=s+Scrabble.Player[i].Name+' '+SecondsToString(Scrabble.sTimeControlValue-round(Scrabble.Player[i].mTimeElapsed/1000))+LineBreak
    else
    if Scrabble.TimeControlSetting=tcPerMove then
      s:=s+rMain_SBTimePerMove+LineBreak else
      s:=s+rMain_SBTimeNoLimit+LineBreak;
    s:=s+LineBreak;

    s:=s+rMain_SBScore+LineBreak;
    for i:=0 to Scrabble.NumberOfPlayers-1 do
    begin
      s:=s+Scrabble.Player[i].Name+' '+IntToStr(Scrabble.Player[i].Points);
      if i<Scrabble.NumberOfPlayers-1 then s:=s+LineBreak;
    end;
  end else
    s:=rMain_SBStatus;
  pbInfo.Hint:=s;
end;

procedure TfmMain.pbStatusMouseEnter(Sender: TObject);
var
  s: string;
begin
  s:=rMain_SBSettings;
  if gsRunning in Scrabble.GameState then
  begin
    case Scrabble.WordCheckMode of
     wcAsk       : s:=s+LineBreak+'* '+rMain_SBTakebackMode;
     wcPoll      : s:=s+LineBreak+'* '+rMain_SBPollMode;
     wcChallenge : s:=s+LineBreak+'* '+rMain_SBChallengeMode;
    end;
    if (gsCLABBERS in Scrabble.GameState) then
      s:=s+' (CLABBERS)';
    if (gsJokerExchange in Scrabble.GameState) then
      s:=s+LineBreak+'* '+rMain_SBJokerExchange else
      s:=s+LineBreak+'* '+rMain_SBNoJokerExchange;
    if (gsJokerization in Scrabble.GameState) then
      s:=s+LineBreak+'* '+rMain_SBJokerization else
      s:=s+LineBreak+'* '+rMain_SBNoJokerization;
    s:=s+LineBreak+'* '+rMain_SBPoll;
  end;
  pbStatus.Hint:=s;
end;

procedure TfmMain.pmStatusInfoClose(Sender: TObject);
begin
  pbInfo.Invalidate;
end;

procedure TfmMain.pbPlayerPaint(Sender: TObject);
 function DoPaint:integer;
 var
   i,x,y,y2: integer;
 begin
   x:=0;
   y:=(pbPlayer.ClientHeight-pbPlayer.Canvas.TextHeight('ABC')) div 2;
   for i:=0 to Scrabble.NumberOfPlayers-1 do
   begin
     y2:=y;
     //frame for current users
     if (i=Scrabble.CurrentPlayer) then
     begin
       pbPlayer.Canvas.Pen.Color:=clRed;
       pbPlayer.Canvas.Rectangle(Bounds(x,0,pbPlayer.Canvas.TextWidth(Scrabble.Player[i].Name)+3,pbPlayer.Height));
     end;
     //colored background
     if fmGameOptions.cbPlayerColored.Checked then
     begin
       pbPlayer.Canvas.Font.Color:=fmGameOptions.PlayerColor[i];
       pbPlayer.Canvas.TextOut(x+1,y2,Scrabble.Player[i].Name);
       y2:=y2+1;
     end;
     //shaded text
     if not fmGameOptions.cbPlayerColored.Checked or fmGameOptions.cbShaded.Checked then
     begin
       pbPlayer.Canvas.Font.Color:=clWindowText;
       pbPlayer.Canvas.TextOut(x+1,y2,Scrabble.Player[i].Name);
     end;
     //next position
     inc(x,pbPlayer.Canvas.TextWidth(Scrabble.Player[i].Name)+3);
   end;
   Result:=x;
 end;
begin
  {$ifdef DebugStatusBar} writeln('pbPlayerPaint'); {$endif}
  pbPlayer.Canvas.Lock;
  try
    pbPlayer.Canvas.Brush.Color:={$ifdef Darwin}clNone{$else}clBtnFace{$endif};
    pbPlayer.Canvas.Brush.Style:=bsSolid;
    pbPlayer.Canvas.FillRect(pbPlayer.Canvas.ClipRect);
    pbPlayer.Canvas.Brush.Style:=bsClear;
    pbPlayer.Canvas.Font.Color:=clWindowText;
    //paint & calc width
    pbPlayer.Tag:=DoPaint;
    PostMessage(self.Handle,LM_RESIZESTATUSBAR,0,0);
  finally
    pbPlayer.Canvas.UnLock;
  end;
end;

procedure TfmMain.pbPointsPaint(Sender: TObject);
 function DoPaint:integer;
 const
   cWidth=8;
 var
   x,y: integer;
   s: string;
 begin
   //bar
   if sbPoints>99 then
     y:=pbPoints.Height else
     y:=round(pbPoints.Height*(sbPoints/100));
   if sbPoints>0 then
     s:=IntToStr(sbPoints) else
     s:='';

   pbPoints.Canvas.GradientFill(Bounds(0,pbPoints.Height-y,cWidth-1,pbPoints.Height),clGreen,clRed,gdVertical);
   //text
   x:=cWidth+1;
   y:=(pbPoints.ClientHeight-pbPoints.Canvas.TextHeight('ABC')) div 2;
   if fmGameOptions.cbPlayerColored.Checked then
   begin
     pbPoints.Canvas.Font.Color:=fmGameOptions.PlayerColor[sbWho];
     pbPoints.Canvas.TextOut(x,y,s);
     y:=y+1;
   end;
   if not fmGameOptions.cbPlayerColored.Checked or fmGameOptions.cbShaded.Checked then
   begin
     pbPoints.Canvas.Font.Color:=clWindowText;
     pbPoints.Canvas.TextOut(x,y,s);
   end;
   //calc width
   Result:=x+1+pbPoints.Canvas.TextWidth(s)+1;
 end;
begin
  {$ifdef DebugStatusBar} writeln('pbPointsPaint'); {$endif}
  pbPoints.Canvas.Lock;
  try
    pbPoints.Canvas.Brush.Color:={$ifdef Darwin}clNone{$else}clBtnFace{$endif};
    pbPoints.Canvas.Brush.Style:=bsSolid;
    pbPoints.Canvas.FillRect(pbPoints.Canvas.ClipRect);
    pbPoints.Canvas.Brush.Style:=bsClear;
    pbPoints.Canvas.Font.Color:=clWindowText;
    //paint & calc width
    pbPoints.Tag:=DoPaint;
    PostMessage(self.Handle,LM_RESIZESTATUSBAR,0,0);
  finally
    pbPoints.Canvas.UnLock;
  end;
end;

procedure TfmMain.pbInfoPaint(Sender: TObject);
 function DoPaint:integer;
 var
   x,y,y2,i,j,z: integer;
   s: string;
 begin
   x:=0;
   y:=(pbInfo.ClientHeight-pbInfo.Canvas.TextHeight('ABC')) div 2;

   if (pmStatusInfo.Items[0].Checked) and
      (Scrabble.NumberOfLettersLeft>0) then //0 if no more in bag, but maybe on rack(s)
   begin
     s:=inttostr(Scrabble.NumberOfLettersLeft)+' +'+{%H-}inttostr(Scrabble.RackSize*Scrabble.NumberOfPlayers);
     pbInfo.Canvas.TextOut(x,y,s);
     inc(x,pbInfo.Canvas.TextWidth(s));
   end else
   for i:=0 to Scrabble.NumberOfPlayers-1 do
   begin
     s:='';
     y2:=y;
     //value
     if (pmStatusInfo.Items[0].Checked) then
     begin
       z:=0;
       for j:=0 to Scrabble.RackSize-1 do
        if Scrabble.RackLetter[i,j]<>nil then
          inc(z);
        s:=s+inttostr(z);
     end else
     if (pmStatusInfo.Items[1].Checked) then
       s:=s+inttostr(Scrabble.Player[i].Points) else
     if (pmStatusInfo.Items[2].Checked) then
     begin
       if Scrabble.TimeControlSetting<>tcNoLimit then
       begin
         if i=Scrabble.CurrentPlayer then
           s:=s+SecondsToString(Scrabble.sTimeControlValue-(round(Scrabble.Player[i].mTimeElapsed/1000)+Scrabble.sMoveTime)) else
           s:=s+SecondsToString(Scrabble.sTimeControlValue-round(Scrabble.Player[i].mTimeElapsed/1000));
       end else
         s:=s+'no limit';
     end;

     //colored background
     if fmGameOptions.cbPlayerColored.Checked then
     begin
       pbInfo.Canvas.Font.Color:=fmGameOptions.PlayerColor[i];
       pbInfo.Canvas.TextOut(x,y2,s);
       y2:=y2+1;
     end;
     //shaded text
     if not fmGameOptions.cbPlayerColored.Checked or fmGameOptions.cbShaded.Checked then
     begin
       pbInfo.Canvas.Font.Color:=clWindowText;
       pbInfo.Canvas.TextOut(x,y2,s);
     end;
     //next position
     inc(x,pbInfo.Canvas.TextWidth(s));
     //comma
     if i<Scrabble.NumberOfPlayers-1 then
     begin
       pbInfo.Canvas.Font.Color:=clWindowText;
       pbInfo.Canvas.TextOut(x,y2,',');
       inc(x,pbInfo.Canvas.TextWidth(','));
     end;
   end;//NumberOfPlayers
   Result:=x;
 end;
begin
  {$ifdef DebugStatusBar} writeln('pbInfoPaint');  {$endif}
  pbInfo.Canvas.Lock;
  try
    pbInfo.Canvas.Brush.Color:={$ifdef Darwin}clNone{$else}clBtnFace{$endif};
    pbInfo.Canvas.Brush.Style:=bsSolid;
    pbInfo.Canvas.FillRect(pbInfo.Canvas.ClipRect);
    pbInfo.Canvas.Brush.Style:=bsClear;
    pbInfo.Canvas.Font.Color:=clWindowText;
    //paint & calc width
    pbInfo.Tag:=DoPaint;
    PostMessage(self.Handle,LM_RESIZESTATUSBAR,0,0);
  finally
    pbInfo.Canvas.UnLock;
  end;
end;

procedure TfmMain.pbStatusPaint(Sender: TObject);
 function DoPaint:integer;
 var
   x,y: integer;
   gde: TGraphicsDrawEffect;
 begin
   x:=0;
   y:=(pbStatus.ClientHeight-ilSmallImages.Height) div 2;
   //word check mode
   if gsRunning in Scrabble.GameState then
     gde:=gdeNormal else
     gde:=gdeDisabled;
   case Scrabble.WordCheckMode of
     wcAsk       : ilSmallImages.Draw(pbStatus.Canvas,x,y,0,gde);
     wcPoll      : ilSmallImages.Draw(pbStatus.Canvas,x,y,7,gde);
     wcChallenge : ilSmallImages.Draw(pbStatus.Canvas,x,y,8,gde);
   end;
   inc(x,ilSmallImages.Width+1);
   //joker exchange
   if (gsJokerExchange in Scrabble.GameState) then
     gde:=gdeNormal else
     gde:=gdeDisabled;
   ilSmallImages.Draw(pbStatus.Canvas,x,y,1,gde);
   inc(x,ilSmallImages.Width+1);
   //jokerization
   if (gsJokerization in Scrabble.GameState) then
     gde:=gdeNormal else
     gde:=gdeDisabled;
   ilSmallImages.Draw(pbStatus.Canvas,x,y,16,gde);
   inc(x,ilSmallImages.Width+1);
   //network
   if (gsNetwork in Scrabble.Gamestate) and (Scrabble.NumberOfPlayers>1) then
   begin
     pbStatus.Canvas.StretchDraw(Bounds(x,y,x+ilSmallImages.Height,y+ilSmallImages.Height),Poll.AnswerPie);
     Result:=x+ilSmallImages.Height;
   end else
     Result:=x;
 end;
begin
  {$ifdef DebugStatusBar} writeln('pbStatusPaint'); {$endif}
  pbStatus.Canvas.Lock;
  try
    pbStatus.Canvas.Brush.Color:={$ifdef Darwin}clNone{$else}clBtnFace{$endif};
    pbStatus.Canvas.Brush.Style:=bsSolid;
    pbStatus.Canvas.FillRect(pbStatus.Canvas.ClipRect);
    pbStatus.Canvas.Brush.Style:=bsClear;
    pbStatus.Canvas.Font.Color:=clWindowText;
    //paint & adjust width
    pbStatus.Tag:=DoPaint;
    PostMessage(self.Handle,LM_RESIZESTATUSBAR,0,0);
  finally
    pbStatus.Canvas.UnLock;
  end;
end;

procedure TfmMain.DoResizeStatusbar(var Msg: TLMessage);
begin
  pbPlayer.Width:=pbPlayer.Tag;
  pbPoints.Width:=pbPoints.Tag;
  pbInfo.Width:=pbInfo.Tag;
  pbStatus.Width:=pbStatus.Tag;
end;


