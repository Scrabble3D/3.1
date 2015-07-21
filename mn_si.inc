{ ----- simple ----- }
procedure TfmMain.pn2DResize(Sender: TObject);
begin
  if (ScrabbleGrid is TDgScrabbleGrid) then
    (ScrabbleGrid as TDgScrabbleGrid).DoResize(self) else
  {$ifdef UseOpenGl}
  if (ScrabbleGrid is TGlScrabbleGrid) then
    (ScrabbleGrid as TGlScrabbleGrid).Invalidate;
  {$endif}
end;

procedure TfmMain.pnPiecesContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  i:integer;
  r:TRect;
begin
  for i:=0 to PieceList.Count-1 do
   with TPiece(PieceList.Items[i]) do
   begin
     r:=ClientRect;
     OffsetRect(r,Left,Top);
     if PtInRect(r,MousePos) and (Visible) then
       Handled:=true;
   end;
end;

procedure TfmMain.DoDictionaryChange(Sender: TObject);
begin
  fmGameOptions.UpdateDictionaryInfo;
  fmWordSearch.UpdateDictionaryInfo;
end;

procedure TfmMain.acCambioSeccoExecute(Sender: TObject);
begin
  if (MessageDlg(rMain_CambioSecco,mtConfirmation,[mbYes,mbNo],0)=mrYes) then
  begin
    Scrabble.RestoreBoard;
    BruteForce.ClearMove;
    if (gsNetwork in Scrabble.GameState) then
      TCPClient.OnSend('nwCambioSecco','group','MoveNumber='+inttostr(Scrabble.ActualMove)) else
    begin
      Scrabble.CambioSecco;
      OnMessage(smInformation,Language.Format(rMain_CambioSeccoUsed,[Scrabble.Player[Scrabble.CurrentPlayer].Name]));
    end;
  end;
end;

procedure TfmMain.DoMessage(aMsgType:TScrabbleMessageType; aMsg: string; aSender:string='');
var
  s:string;
begin
  if not assigned(ScrabbleMessages) then exit;
  if (aMsgType<>smDebug) {and (pos(#13,aMsg)=0) }then
  begin
    s:=StringReplace(aMsg,#13,'/',[rfReplaceAll]);
    s:=StringReplace(s,#10,'',[rfReplaceAll]);
    s:=StringReplace(s,#9,' ',[rfReplaceAll]);
    lbMessages.Caption:=s;
  end;
  ScrabbleMessages.AddMessage(aMsgType,aMsg,aSender);
end;

procedure TfmMain.miUpdateClick(Sender: TObject);
begin
  try
    OnMessage(smInformation,'Checking for updates...');
    Updater.CheckForUpdates;
  except
    on E:Exception do OnMessage(smError,E.Message);
  end;
end;

procedure TfmMain.miNewsClick(Sender: TObject);
begin
  OpenURL(Config.Read('Network/Links/News','http://twitter.com/scrabble3d'));
end;

procedure TfmMain.miRackHeaderClick(Sender: TObject);
begin
  pnPiecesHeader.Visible:=miRackHeader.Checked;
  pnPieces.Tag:=integer(pnPiecesHeader.Visible)*20; //hack to place pieces properly
  if assigned(PieceList) then
    PieceList.Resize(self);
  Config.Write('General/Position/pnRack/HeaderVisible',pnPiecesHeader.Visible);
end;

procedure TfmMain.miForumClick(Sender: TObject);
begin
  OpenURL(Config.Read('Network/Links/Forum','http://17085.homepagemodules.de'));
end;

procedure TfmMain.miFacebookClick(Sender: TObject);
begin
  OpenURL(Config.Read('Network/Links/SocialMedia','http://www.facebook.com/pages/Scrabble3D/189119127784666'));
end;

procedure TfmMain.acHelpExecute(Sender: TObject);
begin
  {$ifdef Debug}
  {$Warning acHelpExecute:Debug}
  Bruteforce.CopyPerformance;
//  Bruteforce.CopyBestMoves;
//  ShowMessage(Scrabble.ShowStatus);
//  ShowMessage(inttostr(Dictionary.Performance);
  {$else}
  OpenURL(Config.Read('Network/Links/Wiki','http://sourceforge.net/apps/mediawiki/scrabble'));
  {$endif}
end;

procedure TfmMain.miLocalizationClick(Sender: TObject);
begin
  OpenURL(Config.Read('Network/Links/Localization','https://www.transifex.com/projects/p/scrabble3d/'));
end;

procedure TfmMain.tbCubeResetClick(Sender: TObject);
begin
  {$ifdef UseOpenGl}
  ScrabbleCube.ResetPosition;
  {$endif}
end;

procedure TfmMain.pcMessagesPageChanged(Sender: TObject);
begin
  if (gsNetwork in Scrabble.GameState) then
    pnChat.Parent:=pcMessages.ActivePage;
end;

procedure TfmMain.acRestoreExecute(Sender: TObject);
begin
  Scrabble.RestoreBoard;
  pbPoints.Repaint;
end;

procedure TfmMain.acExitExecute(Sender: TObject);
begin
  if not (gsDestroying in Scrabble.GameState) then
  begin
    Scrabble.GameState:=Scrabble.GameState+[gsDestroying];
    self.Close;
  end;
end;

procedure TfmMain.acLoadExecute(Sender: TObject);
begin
  if (gsNetwork in Scrabble.GameState) and IsGameServer then
  begin
    TCPClient.OnSend('nwRemoteGames',TCPClient.PlayerData.PlayerName,'');
    OnMessage(smInformation,rMain_LoadGame);
    Screen.Cursor:=crHourGlass;
  end else
  begin
    OpenDialog.Filter:=rMain_Savegame+'|*.ssg|'+rMain_AllFiles+'|*.*';
    if OpenDialog.Execute then
    begin
      LoadFrom(UTF8ToSys(OpenDialog.FileName));
      OnMessage(smInformation,Language.Format(rMain_LoadSuccess,[ExtractFileName(OpenDialog.FileName)]));
    end;
  end;
end;

procedure TfmMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ifdef darwin}
var
  z: word=VK_RETURN;
{$endif}
begin
  case key of
   VK_RETURN:
    if edChat.CanFocus then
    begin
      edChat.SetFocus;
      edChat.SelStart:=UTF8Length(edChat.Text);
    end;
   VK_PRIOR: //Page up
    if Scrabble.Dimension=D3 then
    begin
      if (ssAlt in Shift) then
      begin
        with Scrabble.ActiveDimension do
         if Axis<high(TDimension) then
           Axis:=Succ(Axis) else
           Axis:=low(TDimension);
      end else
      with Scrabble.ActiveDimension do
       if Position<Scrabble.BoardSize-1 then
        Position:=Position+1;
    end;
   VK_NEXT: //Page down
    if Scrabble.Dimension=D3 then
    begin
      if (ssAlt in Shift) then
      begin
        with Scrabble.ActiveDimension do
         if Axis>low(TDimension) then
           Axis:=Pred(Axis) else
           Axis:=high(TDimension)
      end else
      with Scrabble.ActiveDimension do
       if Position>0 then
        Position:=Position-1;
    end;
   VK_ADD: // Num+
    if seBestMove.Enabled then
    begin
      if seBestMove.Text='' then
        seBestMove.Text:='1' else
      if (StrToIntDef(seBestMove.Text,1)<BruteForce.BestMovesCount) then
        seBestMove.Text:=IntToStr(StrToInt(seBestMove.Text)+1);
      {$ifdef darwin}
      if not seBestMove.Focused then
        seBestMoveKeyUp(self,z,[]);
      {$endif}
    end;
   VK_SUBTRACT: // Num-
    if seBestMove.Enabled then
    begin
      if seBestMove.Text='' then
        seBestMove.Text:='1' else
      if (StrToIntDef(seBestMove.Text,1)>1) then
        seBestMove.Text:=IntToStr(StrToIntDef(seBestMove.Text,2)-1);
      {$ifdef darwin}
      if not seBestMove.Focused then
        seBestMoveKeyUp(self,z,[]);
      {$endif}
    end;
   {$ifdef UseOpenGl}
   VK_UP:
     if (ssAlt in Shift) and (ScrabbleGrid is TGlScrabbleGrid) then
      with TGlScrabbleGrid(ScrabbleGrid).GridView.Translation do
       z+=-1;
   VK_DOWN:
     if (ssAlt in Shift) and (ScrabbleGrid is TGlScrabbleGrid) then
      with TGlScrabbleGrid(ScrabbleGrid).GridView.Translation do
       z+=1;
   VK_LEFT:
     if (ssAlt in Shift) and (ScrabbleGrid is TGlScrabbleGrid) then
      with TGlScrabbleGrid(ScrabbleGrid).GridView.Rotation do
       if x<90 then x+=1;
   VK_RIGHT:
     if (ssAlt in Shift) and (ScrabbleGrid is TGlScrabbleGrid) then
      with TGlScrabbleGrid(ScrabbleGrid).GridView.Rotation do
       if x>0 then x+=-1;
   VK_0, VK_NUMPAD0:
     if (ssAlt in Shift) and (ScrabbleGrid is TGlScrabbleGrid) then
      TGlScrabbleGrid(ScrabbleGrid).ResetPosition;
   {$endif}
  end;
  {$ifdef UseOpenGl}
  if (Key in [VK_UP,VK_DOWN,VK_LEFT,VK_RIGHT,VK_0, VK_NUMPAD0]) and
     (ScrabbleGrid is TGlScrabbleGrid) then
      ScrabbleGrid.Paint(self);
  {$endif}
end;

procedure TfmMain.mi3DToolbarClick(Sender: TObject);
begin
  fmGameOptions.cbToolbar3D.Checked:=mi3DToolbar.Checked;
end;

procedure TfmMain.DoUserInput(Sender: TObject; Msg: Cardinal);
begin
  LastAction:=Now;
  if IsGameServer and
     (TCPClient.PlayerData.ClientState=csAfk) then
  begin
    TCPClient.PlayerData.ClientState:=LastClientState;
    TCPClient.OnSend('nwRefresh','all','SetState='+ClientStateToString(LastClientState))
  end;
end;

procedure TfmMain.acSaveExecute(Sender: TObject);
begin
  SaveDialog.Filter:=rMain_Savegame+'|*.ssg|'+rMain_AllFiles+'|*.*';
  SaveDialog.FileName:='';
  SaveDialog.DefaultExt:='.ssg';
  if SaveDialog.Execute then
    SaveTo(SaveDialog.FileName);
end;

procedure TfmMain.acScreenshotExecute(Sender: TObject);
const
  READ_BYTES = 2048;
var
  i,MoveNumber:word;
  r:TRect;
  aPng:TPortableNetworkGraphic;
  aBmp:TBitmap;
  {$ifdef UseOpenGl}
  aRotation:TPosition;
  {$endif}
  sl:TStringList;
  ms:TMemoryStream;
  BytesRead, NumBytes: LongInt;
begin
  with TSaveDialog.Create(self) do
  try
    Filter:='Portable Network Graphics (*.png)|*.png';
    if (fmGameOptions.edMovieProgram.Text<>'') and
       FileExists(fmGameOptions.edMovieProgram.Text) then
      Filter:=Filter+'|'+'MPEG1 Video (*.mpg)|*.mpg';
    DefaultExt:='.png';
    Options:=Options+[ofOverwritePrompt,ofPathMustExist];
    if Execute then
    begin
      aPng:=TPortableNetworkGraphic.Create;
      if Scrabble.Dimension=D2 then
        aPng.Width:=pn2D.ClientWidth else
        aPng.Width:=pn2D.ClientWidth+pn3D.ClientWidth;
      aPng.Height:=pn2D.ClientHeight;
      aPng.TransparentColor:=clBlack;
      aPng.Transparent:=true;
      aPng.Canvas.Brush.Color:=fmGameOptions.cbBackGroundColor.ButtonColor;
      aPng.Canvas.Brush.Style:=bsSolid;
      try
        if FilterIndex=2 then
        begin
          MoveNumber:=0;
          ForceDirectory(GetTempDir+'Scrabble3D/');
        end;
        repeat
          if FilterIndex=2 then
          begin
            DoSetCurrentMove(MoveNumber);
            OnProgress(self,round((MoveNumber/Scrabble.CurrentMove)*100));
          end;

          aBmp:=ScrabbleGrid.ScreenShot;
          try
            //board
            aPng.Canvas.FillRect(Bounds(0,0,aPng.Width,aPng.Height));
            aPng.Canvas.Draw(0,0,aBmp);
            {$ifdef UseOpenGl}
            if Scrabble.Dimension=D3 then
            begin
              aBmp:=ScrabbleCube.ScreenShot;
              aPng.Canvas.Draw(pn2D.ClientWidth,aPng.Height div 2-aBmp.Height div 2,aBmp);
            end;
            {$endif}
            //letters
            aBmp.Width:=40; aBmp.Height:=40;
            if ((Scrabble.ActualMove=Scrabble.CurrentMove) and (Scrabble.CurrentPlayer=Scrabble.LocalPlayer)) or
               (not (gsNetwork in Scrabble.GameState) or
                (gsGameEnd in Scrabble.GameState)) then
            for i:=0 to Scrabble.RackSize-1 do
            begin
              aBmp.Canvas.Brush.Color:=fmGameOptions.FieldColor[ftNewLetter];
              aBmp.Canvas.Brush.Style:=bsSolid;
              r:=Bounds(0,0,40,40);
              if TPiece(PieceList.Items[i]).Data<>nil then
              begin
                aBmp.Canvas.FillRect(r);
                aBmp.Canvas.Frame3D(r,1,bvRaised);
                DoDrawLetter(TPiece(PieceList.Items[i]).Data,aBmp.Canvas);//Scrabble.RackLetter[Scrabble.CurrentPlayer,i]
                aPng.Canvas.Draw(aPng.Width-(Scrabble.RackSize-i)*41-40,aPng.Height-41,aBmp);
              end;
            end;

            //save, and adjust cube's rotation for movie
            if FilterIndex=2 then
            begin
              aPng.SaveToFile(GetTempDir+'Scrabble3D/'+'img'+inttostr(MoveNumber)+'.png');
              {$ifdef UseOpenGl}
              if Scrabble.Dimension=D3 then
              begin
                aRotation:=ScrabbleCube.Rotation;
                aRotation.X:=aRotation.X+0.5;
                aRotation.Y:=aRotation.Y+360/Scrabble.CurrentMove;
                aRotation.Z:=aRotation.Y+0.25;
                ScrabbleCube.Rotation:=aRotation;
              end;
              {$endif}
            end else
              aPng.SaveToFile(FileName);

            inc(MoveNumber);
          finally
            aBmp.Free;
          end; //aBmp
        until (FilterIndex=1) or (MoveNumber>Scrabble.CurrentMove-1);
      finally
        OnProgress(self,101);//clear
        aPng.Free;
      end;//aPng
      //create movie from screenshots
      if FilterIndex=2 then
      begin
        if FileExistsUTF8(FileName) then
          DeleteFileUTF8(FileName);
        with TProcess.Create(nil) do
        try
          Options:=Options+[poUsePipes]-[poNoConsole,poWaitOnExit];
          Executable:=fmGameOptions.edMovieProgram.Text;
          Parameters.CommaText:=fmGameOptions.edMovieOptions.Text;
          Parameters.Add('-i');
          Parameters.Add(GetTempDir+'Scrabble3D/'+'img%d.png');
          Parameters.Add('-r');
          Parameters.Add('25');
          Parameters.Add(FileName);

          sl:=TStringList.Create;
          ms:=TMemoryStream.Create;
          //run
          Execute;

          BytesRead:=0;
          while true do
          begin
            ms.SetSize(BytesRead + READ_BYTES);
            NumBytes := StdErr.Read((ms.Memory + BytesRead)^, READ_BYTES);
            if NumBytes > 0 then
              inc(BytesRead, NumBytes) else
              break;
          end;
          ms.SetSize(BytesRead);
          sl.LoadFromStream(ms);

          //output
          if ExitStatus>0 then
           for i:=0 to sl.Count-1 do
            OnMessage(smDebug,sl[i]);
        finally
          sl.Free;
          ms.Free;
          Free; //TProcess
        end;
        DeleteDirectory(GetTempDir+'Scrabble3D/',false);
      end; //FilterIndex=2, aka create movie
    end; //Execute
  finally
    Free;//TSaveDialog
  end;
end;

procedure TfmMain.acSettingsExecute(Sender: TObject);
  {$ifdef UseOpenGl}
var
  aPos:TPosition;
  {$endif}
begin
  {$ifdef UseOpenGl}
  if (ScrabbleGrid is TGlScrabbleGrid) then
  begin
    //disconnect OnChange handler
    fmGameOptions.tbPitch.OnChange:=nil;
    fmGameOptions.tbDistance.OnChange:=nil;
    fmGameOptions.tbTranslation.OnChange:=nil;
    //apply
    aPos:=(ScrabbleGrid as TGlScrabbleGrid).Rotation;
    fmGameOptions.tbPitch.Position:=round(aPos.X);
    aPos:=(ScrabbleGrid as TGlScrabbleGrid).Translation;
    fmGameOptions.tbDistance.Position:=round(abs(aPos.Z)*10);
    fmGameOptions.tbTranslation.Position:=round(aPos.Y);
    fmGameOptions.cbMipmapping.Enabled:=(ScrabbleGrid as TGlScrabbleGrid).GridView.CanUseMipmaps;
    if not fmGameOptions.cbMipmapping.Enabled then
      fmGameOptions.cbMipmapping.Checked:=false;
    //connect OnChange handler and apply all settings immediately
    fmGameOptions.tbPitch.OnChange:=@fmGameOptions.VisualChange;
    fmGameOptions.tbDistance.OnChange:=@fmGameOptions.VisualChange;
    fmGameOptions.tbTranslation.OnChange:=@fmGameOptions.VisualChange;
  end;
  {$endif}
  fmGameOptions.ShowModal;
end;

procedure TfmMain.acShufflePiecesExecute(Sender: TObject);
var
  i:integer;
begin
  for i:=0 to Scrabble.RackSize-1 do
    PieceList.Exchange(i,random(Scrabble.RackSize));
  PieceList.Resize(nil); //to repaint
end;

procedure TfmMain.acHighscoreExecute(Sender: TObject);
begin
  if (gsNetwork in Scrabble.GameState) and IsGameServer then
  begin
    TCPClient.OnSend('nwHighscore',TCPClient.PlayerData.PlayerName,'');
    pcMessages.ActivePage:=pcMessages.Pages[0];
  end else
  begin
    fmStatistics.chRating.Visible:=false;
    fmStatistics.ShowModal;
  end;
end;

procedure TfmMain.DoJokerize(aLetter: TLetter);
begin
  if aLetter<>nil then
  begin
    OnMessage(smInformation,Language.Format(rMain_PieceJokerized,[Scrabble.Player[Scrabble.CurrentPlayer].Name,aLetter.What]));
    Scrabble.DoJokerizeLetter(aLetter);
  end else
    OnMessage(smError,'Error on jokerization: unknown letter');
end;

procedure TfmMain.acWordSearchExecute(Sender: TObject);
begin
  fmWordSearch.Show;
end;

procedure TfmMain.miAboutClick(Sender: TObject);
begin
  fmAbout.ShowModal;
end;

procedure TfmMain.miAssistantClick(Sender: TObject);
begin
  fmWelcome.ShowModal;
end;

procedure TfmMain.ScrabblePlaySound(aMsg:TScrabbleMessageType;async:boolean);
{$IFNDEF Windows}
const
  READ_BYTES = 2048;
{$endif}
var
  s:string;
  {$IFNDEF Windows}
  sl:TStringList;
  ms:TMemoryStream;
  BytesRead, NumBytes: LongInt;
  i:integer;
  {$ENDIF}
begin
  try
    s:='';
    case aMsg of
     smChat     : if fmGameOptions.sbChat.Down then
                  begin
                    if FileExistsUTF8(Config.Path+'Chat.wav') then
                      s:=UTF8ToSys(Config.Path)+'Chat.wav'
                    {$ifdef Windows} else
                      s:=LazarusResources.Find('Chat').Value;
                    {$endif}
                  end;
     smOwnMove   :if fmGameOptions.sbOwnMove.Down then
                  begin
                    if FileExistsUTF8(Config.Path+'OwnMove.wav') then
                      s:=UTF8ToSys(Config.Path)+'OwnMove.wav'
                    {$ifdef Windows} else
                      s:=LazarusResources.Find('OwnMove').Value;
                    {$endif}
                  end;
     smGameResult:if fmGameOptions.sbGameResult.Down then
                  begin
                    if FileExistsUTF8(Config.Path+'GameResult.wav') then
                      s:=UTF8ToSys(Config.Path)+'GameResult.wav'
                    {$ifdef Windows} else
                      s:=LazarusResources.Find('GameResult').Value;
                    {$endif}
                  end;
     smScrabble_self  :if fmGameOptions.sbScrabble.Down then
                  begin
                    if FileExistsUTF8(Config.Path+'Scrabble_self.wav') then
                      s:=UTF8ToSys(Config.Path)+'Scrabble_self.wav'
                    {$ifdef Windows} else
                      s:=LazarusResources.Find('Scrabble_self').Value;
                    {$endif}
                  end;
     smScrabble_other  :if fmGameOptions.sbScrabble.Down then
                  begin
                    if FileExistsUTF8(Config.Path+'Scrabble_other.wav') then
                      s:=UTF8ToSys(Config.Path)+'Scrabble_other.wav'
                    {$ifdef Windows} else
                      s:=LazarusResources.Find('Scrabble_other').Value;
                    {$endif}
                  end;
    end;//case
  except end;

  if (s<>'') then
  begin
   {$IFDEF Windows}
   if FileExistsUTF8(SysToUTF8(s)) then
   begin
     if async then
      sndPlaySound(PChar(s),SND_ASYNC or SND_NOWAIT or SND_NODEFAULT) else
      sndPlaySound(PChar(s),SND_NOWAIT or SND_NODEFAULT);
   end else
   begin
    if async then
      sndPlaySound(PChar(s),SND_ASYNC or SND_NOWAIT or SND_MEMORY) else
      sndPlaySound(PChar(s),SND_NOWAIT or SND_MEMORY);
   end;
   {$ELSE}
   if fmGameOptions.edSoundCommand.Text<>'' then
   with TProcess.Create(nil) do
   try
     Options:=Options+[poUsePipes]-[poNoConsole,poWaitOnExit];
     Executable:=fmGameOptions.edSoundCommand.Text;
     Parameters.Add(s);
     sl:=TStringList.Create;
     ms:=TMemoryStream.Create;

     try
       Execute;

       BytesRead:=0;
       while true do
       begin
         ms.SetSize(BytesRead + READ_BYTES);
         NumBytes := StdErr.Read((ms.Memory + BytesRead)^, READ_BYTES);
         if NumBytes > 0 then
           inc(BytesRead, NumBytes) else
           break;
       end;
       ms.SetSize(BytesRead);
       sl.LoadFromStream(ms);

       //output
       if ExitStatus>0 then
        for i:=0 to sl.Count-1 do
         OnMessage(smDebug,sl[i]);
     finally
       sl.Free;
       ms.Free;
     end;

   finally
     Free; //TProcess
   end;
   {$ENDIF}
  end;
end;

procedure TfmMain.DoConfigChange(Sender: TObject); //Sender=GameOptions
const
  cBevel:array[boolean] of TGraphicsBevelCut=(bvRaised,bvNone);
{$ifdef UseOpenGl}
var
  aLight:TLight;
  aPos:TPosition;
{$endif}
begin
//    UpdateChatReceiver;
  with GameCourse do
  begin
    PlayerColor[0]:=fmGameOptions.cbPlayer1.ButtonColor;
    PlayerColor[1]:=fmGameOptions.cbPlayer2.ButtonColor;
    PlayerColor[2]:=fmGameOptions.cbPlayer3.ButtonColor;
    PlayerColor[3]:=fmGameOptions.cbPlayer4.ButtonColor;
    Shaded:=fmGameOptions.cbShaded.Checked;
    Colored:=fmGameOptions.cbPlayerColored.Checked;
  end;
  ScrabbleMessages.ShowDebugMessages:=fmGameOptions.cbDebug.Checked;
  pn2D.BevelOuter:=cBevel[fmGameOptions.cbFlat.Checked];
  pn3D.BevelOuter:=cBevel[fmGameOptions.cbFlat.Checked];
  pn2D.Color:=fmGameOptions.cbBackGroundColor.Color;
  pn3D.Color:=fmGameOptions.cbBackGroundColor.Color;
  pnPieces.Color:=fmGameOptions.cbBackGroundColor.Color;
  pnPieces.BevelOuter:=cBevel[fmGameOptions.cbFlat.Checked];
  PieceWidth:=0;
  UpdatePieceList;
  pnStatus.Visible:=fmGameOptions.cbStatusbar.Checked;
  pbPlayer.Repaint;
  pbPoints.Repaint;

  SquareSize:=0;
  pnMessages.BevelOuter:=cBevel[fmGameOptions.cbFlat.Checked];
  mi3DToolbar.Checked:=fmGameOptions.cbToolbar3D.Checked;
  case fmGameOptions.cbRackPos.ItemIndex of
   0: begin
        pnPieces.Parent:=pnLeft;
        pnPieces.Align:=alBottom;
      end;
   1: begin
        pnPieces.Parent:=pnRight;
        pnPieces.Align:=alTop;
      end;
  end;//case
  case fmGameOptions.rg2DMode.ItemIndex of
    0 : {$ifdef UseOpenGl}
        begin
          if not assigned(ScrabbleGrid) or not (ScrabbleGrid is TGlScrabbleGrid) then
          begin
            if assigned(ScrabbleGrid) then
            begin
              ScrabbleGrid.Free;
              ScrabbleGrid:=nil;
            end;
            ScrabbleGrid:=TGlScrabbleGrid.Create(pn2D);
            ScrabbleGrid.OnDrawLetter:=@DoDrawLetter;
            ScrabbleGrid.OnAskForJoker:=@DoAskForJoker;
            ScrabbleGrid.OnGetFieldColor:=@DoGetFieldColor;
            ScrabbleGrid.OnGetFieldLetter:=@DoGetFieldLetter;
            ScrabbleGrid.OnGetHint:=@DoGetHint;
          end;
          //light
          aLight.Position[0]:=fmGameOptions.tbLightPosX.Position;
          aLight.Position[1]:=fmGameOptions.tbLightPosY.Position;
          aLight.Position[2]:=fmGameOptions.tbLightPosZ.Position;
          aLight.Ambient:=fmGameOptions.tbLightAmbient.Position;
          aLight.Diffuse:=fmGameOptions.tbLightDiffuse.Position;
          (ScrabbleGrid as TGlScrabbleGrid).Light:=aLight;
          //pitch, zoom
          aPos:=(ScrabbleGrid as TGlScrabbleGrid).Rotation;
          aPos.X:=fmGameOptions.tbPitch.Position;
          (ScrabbleGrid as TGlScrabbleGrid).Rotation:=aPos;
          aPos:=(ScrabbleGrid as TGlScrabbleGrid).Translation;
          aPos.Z:=-fmGameOptions.tbDistance.Position/10;
          aPos.Y:=fmGameOptions.tbTranslation.Position;
          (ScrabbleGrid as TGlScrabbleGrid).Translation:=aPos;
          (ScrabbleGrid as TGlScrabbleGrid).ShowMarkers:=fmGameOptions.cbShowBonusMarkers.Checked;
          (ScrabbleGrid as TGlScrabbleGrid).TransparentBoard:=fmGameOptions.cbBoardTransparent.Checked;
          (ScrabbleGrid as TGlScrabbleGrid).Reflection:=fmGameOptions.cbReflections.Checked;
          (ScrabbleGrid as TGlScrabbleGrid).Mipmapping:=fmGameOptions.cbMipmapping.Checked; //refreshletters below to update
          //size
          ScrabbleGrid.BoardSize:=Scrabble.BoardSize;
          (ScrabbleGrid as TGlScrabbleGrid).AdjustByMouse:=fmGameOptions.cbSticky.Checked;

          (ScrabbleGrid as TGlScrabbleGrid).BkColor:=fmGameOptions.cbBackGroundColor.ButtonColor;
          if not (gsLoading in Scrabble.GameState) then //to avoid repaint on loading a game
            (ScrabbleGrid as TGlScrabbleGrid).RefreshLetters; //in case of font changes
          if fmGameOptions.lvDesigns.Selected<>nil then
            (ScrabbleGrid as TGlScrabbleGrid).Design:=fmGameOptions.lvDesigns.Selected.SubItems[3];
          ScrabbleGrid.Paint(self);
        end{$endif};
    1 :begin
          if not assigned(ScrabbleGrid) or not (ScrabbleGrid is TDgScrabbleGrid) then
          begin
            if assigned(ScrabbleGrid) then
            begin
              ScrabbleGrid.Free;
              ScrabbleGrid:=nil;
            end;

            ScrabbleGrid:=TDgScrabbleGrid.Create(pn2D);
            ScrabbleGrid.OnDrawLetter:=@DoDrawLetter;
            ScrabbleGrid.OnAskForJoker:=@DoAskForJoker;
            ScrabbleGrid.OnGetFieldColor:=@DoGetFieldColor;
            ScrabbleGrid.OnGetFieldLetter:=@DoGetFieldLetter;
            ScrabbleGrid.OnGetHint:=@DoGetHint;
            (ScrabbleGrid as TDgScrabbleGrid).OnGetBonusText:=@DoGetBonusText;
          end;
          with TDgScrabbleGrid(ScrabbleGrid) do
          begin
            ShowBonusMarkers:=fmGameOptions.cbShowBonusMarkers.Checked;
            ShowBonusText:=fmGameOptions.cbShowBonusText.Checked;
          end;

          ScrabbleGrid.BoardSize:=Scrabble.BoardSize;
          ScrabbleGrid.Paint(self);
        end;
  end; //case
  if fmGameOptions.rbRightToLeft.Checked then
    ScrabbleGrid.BiDiMode:=bdRightToLeft else
    ScrabbleGrid.BiDiMode:=bdLeftToRight;
  ScrabbleGrid.UseGreekLetter:=not fmGameOptions.cbRoman.Checked;
  {$ifdef UseOpenGl}
  if Scrabble.Dimension=D3 then
  begin
   ScrabbleCube.UseGreekLetter:=not fmGameOptions.cbRoman.Checked;
   ScrabbleCube.BkColor:=fmGameOptions.cbBackGroundColor.ButtonColor;
   ScrabbleCube.ClearTextures; //in case of font changes
  end;
  {$endif}
  tb3D.Visible:=pn3D.Visible and fmGameOptions.cbToolbar3D.Checked;

  Scrabble.RevertReading:=fmGameOptions.rbRightToLeft.Checked;
  GameCourse.MaskExchanged:=((gsNetwork in Scrabble.GameState) or not fmGameOptions.cbShowComputer.Checked) and
                            not (gsGameEnd in Scrabble.GameState);
  if fmGameOptions.rbLeftToRight.Checked then
    GameCourse.BiDiMode:=bdLeftToRight else
    GameCourse.BiDiMode:=bdRightToLeft;
end;

procedure TfmMain.DoRepaint(aSender: TScrabbleEvent);
{$ifdef DebugRepaint}
var
  s:string;
{$endif}
begin
  {$ifdef DebugRepaint}
  {$Warning DoRepaint:Debug}
  Str(aSender,s);
  if not (gsDestroying in Scrabble.GameState) then //click on exit
    OnMessage(smDebug,s);
  {$endif}
  if not (gsLoading in Scrabble.GameState) then //to avoid repaint on loading a game
  begin
    ScrabbleGrid.Paint(self);
    {$ifdef UseOpenGl}
    if (Scrabble.Dimension=D3) and
       assigned(ScrabbleCube) and ScrabbleCube.Visible then
      ScrabbleCube.Invalidate;
    {$endif}
    UpdatePieceList;
    {$ifdef UseOpenGl}
    if DragManager.IsDragging then
      (ScrabbleGrid as TGlScrabbleGrid).AdjustByMouse:=false else
      (ScrabbleGrid as TGlScrabbleGrid).AdjustByMouse:=fmGameOptions.cbSticky.Checked;
    {$endif}

    UpdateScore;//pbScore.Repaint;

    if aSender in [seRemoveLetterFrom,sePlaceLetterAt,seNextPlayer] then
    begin
      Scrabble.CheckMove(false);
      sbPoints:=Scrabble.LastMove.Value;
      sbWho:=Scrabble.CurrentPlayer;
    end;

    pbPlayer.Repaint;
    pbInfo.Repaint;
    pbStatus.Repaint;
    pbPoints.Repaint;

    ScrabbleMessages.Repaint;
  end;
end;

function TfmMain.DoGetLetterValue(const aChar: widechar): byte;
begin
  Result:=fmGameOptions.LetterValue[aChar];
end;

procedure TfmMain.DoLanguageChanged(Sender: TObject);
begin
  fmMain.BiDiMode:=Language.BiDiMode;
//  FormResize(self);//panel placement
  ScrabbleMessages.BiDiMode:=Language.BiDiMode;
  fmGameOptions.BiDiMode:=Language.BiDiMode;
  fmAbout.BiDiMode:=Language.BiDiMode;
  fmNetwork.BiDiMode:=Language.BiDiMode;
  fmRemote.BiDiMode:=Language.BiDiMode;
  fmStatistics.BiDiMode:=Language.BiDiMode;
  fmWelcome.BiDiMode:=Language.BiDiMode;
  fmWordSearch.BiDiMode:=Language.BiDiMode;
  if IsGameServer then
  begin
    miLoad.Caption:=rMain_MenuLoadNetwork;
    miSave.Caption:=rMain_MenuSaveNetwork;
  end;

  cbChatReceiver.Items[0]:=rMain_ChatGroup;
  cbChatReceiver.Items[1]:=rMain_ChatServer;
  cbChatReceiver.Items[2]:=rMain_ChatKibitzes;
  if (gsNetwork in Scrabble.GameState) and IsGameServer then
    cbChatReceiver.Text:=cbChatReceiver.Items[cbChatReceiver.ItemIndex];

  if assigned(GameCourse) then
    GameCourse.UpdateLanguage;
  if assigned(ScrabbleMessages) then
    ScrabbleMessages.UpdateLanguage;
  fmGameOptions.UpdateLanguage;
  fmStatistics.UpdateLanguage;
  fmNewGame.UpdateLanguage;
  fmWordSearch.UpdateLanguage;
  ScrabbleGrid.Paint(self);
end;

procedure TfmMain.miFormizeClick(Sender: TObject);
begin
  fm3D:=TForm.Create(nil);
  fm3D.Parent:=nil;
  fm3D.Name:=pn3D.Name;
  fm3D.BorderStyle:=bsSizeToolWin;
  fm3D.FormStyle:=fsStayOnTop;
  fm3D.ShowHint:=true;
  fm3D.Caption:=pn3DHeader.Caption;
  fm3D.SetBounds(pn3D.Left+Left,pn3D.Top+Top,pn3D.ClientWidth,pn3D.ClientHeight);
  fm3D.OnClose:=@DoFormClose;

  pn3D.Parent:=fm3D; //attach 3d panel
  pn3DHeader.Visible:=false; //hide panel header
  pn3D.BevelOuter:=bvNone; //use form's border
  pn3D.Align:=alClient; //full sized

  {$IFDEF UseOpenGl}
  ScrabbleCube.MakeCurrent;
  ScrabbleCube.Free;
  ScrabbleCube:=TScrabbleCube.Create(pn3D);
  ScrabbleCube.OnGetFieldColor:=@DoGetFieldColor;
  ScrabbleCube.OnGetFieldLetter:=@DoGetFieldLetter;
  ScrabbleCube.OnDrawLetter:=@DoDrawLetter;
  ScrabbleCube.MakeCurrent;
  ScrabbleCube.Invalidate;
  ScrabbleCube.BkColor:=fmGameOptions.cbBackGroundColor.ButtonColor;
  ScrabbleCube.UseGreekLetter:=not fmGameOptions.cbRoman.Checked;
  {$endif}

  spRight.Visible:=false;
  fm3D.Visible:=true;
end;

procedure TfmMain.DoFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  pn3D.Parent:=pnRight;  //reassign to right panel
  pn3D.Align:=alTop;
  pn3DHeader.Visible:=true;
  tb3D.Align:=alNone;
  pn3DHeader.Align:=alTop;
  tb3D.Align:=alTop;
  tb3D.Visible:=fmGameOptions.cbToolbar3D.Checked;
  spRight.Visible:=true;
  if fmGameOptions.cbFlat.Checked then
    pn3D.BevelOuter:=bvRaised;

  {$IFDEF UseOpenGl}
  ScrabbleCube.MakeCurrent;
  ScrabbleCube.Free;
  ScrabbleCube:=TScrabbleCube.Create(pn3D);
  ScrabbleCube.OnGetFieldColor:=@DoGetFieldColor;
  ScrabbleCube.OnGetFieldLetter:=@DoGetFieldLetter;
  ScrabbleCube.OnDrawLetter:=@DoDrawLetter;
  ScrabbleCube.MakeCurrent;
  ScrabbleCube.Invalidate;
  ScrabbleCube.BkColor:=fmGameOptions.cbBackGroundColor.ButtonColor;
  ScrabbleCube.UseGreekLetter:=not fmGameOptions.cbRoman.Checked;
  {$endif}

  CloseAction:=caFree;
end;

procedure TfmMain.DoPieceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Scrabble.CurrentMove=Scrabble.ActualMove) then
   case Button of
    mbLeft:
     begin
       if Scrabble.MoveState=msJokerExchanged then
         OnMessage(smWarning,rMain_PieceError1) else
       if Scrabble.MoveState=msLetterExchange then
         OnMessage(smWarning,rMain_PieceError2) else
       if (Scrabble.RackLetter[Scrabble.LocalPlayer,(Sender as TPiece).Data.RackPos]<>nil) then
       begin
//         (Sender as TPiece).Visible:=false;
         if (gsKibitz in Scrabble.GameState) then
           Scrabble.RackLetterIsDragging[Scrabble.CurrentPlayer,(Sender as TPiece).Data.RackPos]:=true else//State:=lsDragging
           Scrabble.RackLetterIsDragging[Scrabble.LocalPlayer,(Sender as TPiece).Data.RackPos]:=true;//State:=lsDragging
       end;
     end;
    mbRight:
     if (Scrabble.CurrentPlayer=Scrabble.LocalPlayer) and
         not (gsKibitz in Scrabble.GameState) and
         not Scrabble.IsTimeout then
     begin
       if Scrabble.MoveState=msLetterPlaced then
         OnMessage(smWarning,rMain_PieceError3) else
       if Scrabble.MoveState=msJokerExchanged then
         OnMessage(smWarning,rMain_PieceError4) else
       begin
         if (ssCtrl in Shift) then
         begin
           if not (gsJokerization in Scrabble.GameState) then //configured at all
             OnMessage(smWarning,rMain_PieceError7) else
           if Scrabble.LocalPlayer<>Scrabble.CurrentPlayer then
             OnMessage(smWarning,rMain_PieceError10) else
           if Scrabble.MoveState=msLetterExchange then  //exchange before
             OnMessage(smWarning,rMain_PieceError6) else
           if not Scrabble.JokerizeLetter then          //has been jokerized
             OnMessage(smWarning,rMain_PieceError8) else
           if gsNetwork in Scrabble.GameState then
             TCPClient.OnSend('nwJokerize','group','LetterIndex='+inttostr(Scrabble.Letters.IndexOf((Sender as TPiece).Data))) else
             DoJokerize((Sender as TPiece).Data);    //exec
         end else
         begin
           if (Scrabble.NumberOfLettersLeft<Scrabble.LimitedExchange) then //letters left
             OnMessage(smWarning,rMain_PieceError5) else
           if (gsJokerization in Scrabble.GameState) and not Scrabble.JokerizeLetter then //jokerized
             OnMessage(smWarning,rMain_PieceError9) else
           if not Scrabble.ToggleChangeState((Sender as TPiece).Data.RackPos) then //letters left
             OnMessage(smWarning,rMain_PieceError5);
         end;
       end;
     end;
   end;//case
   pmRack.ShortcutHandled:=true;
end;

procedure TfmMain.UpdatePieceList;
var
  i,j    : integer;
  aPiece : TPiece;
begin
  CancelDrag;
  if PieceList.Count<>Scrabble.RackSize then
  begin
    while PieceList.Count>Scrabble.RackSize do
    begin
      TPiece(PieceList.Items[0]).Free;
      PieceList.Delete(0);
    end;
    while PieceList.Count<Scrabble.RackSize do
    begin
      aPiece:=TPiece.Create(pnPieces);
      aPiece.OnMouseDown:=@DoPieceMouseDown;
      aPiece.PieceList:=PieceList;
      PieceList.Add(aPiece);
    end;
    for i:=0 to PieceList.Count-1 do
      TPiece(PieceList.Items[i]).Tag:=i;
    PieceList.Resize(nil);
  end;

  for i:=0 to PieceList.Count-1 do
   with TPiece(PieceList.Items[i]) do
   begin
     if (Scrabble.ActualMove<>Scrabble.CurrentMove) and
        (not (gsNetwork in Scrabble.GameState) or
         (gsGameEnd in Scrabble.GameState)) then
     begin
       j:=GameCourse.DoGetRackLetterIndex(Scrabble.ActualMove,i);
       if j>-1 then
         Data:=TLetter(Scrabble.Letters[j]) else
         Data:=nil;
     end else
     begin
      if gsKibitz in Scrabble.GameState then
        Data:=Scrabble.RackLetter[Scrabble.CurrentPlayer,Tag] else
        Data:=Scrabble.RackLetter[Scrabble.LocalPlayer,Tag];
     end;
     if Data<>nil then
     begin
       if Data.State=lsDragging then
       begin
         {$ifndef LCLQt} //Qt stucks at begindrag
         Visible:=false;
         {$endif}
         {$ifdef Windows}
         Application.ProcessMessages;//update piece
         {$endif}
         {$ifndef Darwin}
         Invalidate;
         {$endif}
         BeginDrag(true);
         exit;
       end else
       begin
         Visible:=true;

         if (gsPaused in Scrabble.GameState) and
             not (gsPoll in Scrabble.GameState) and
             not (gsDemo in Scrabble.GameState) and
             not (gsNextPlayer in Scrabble.GameState) then //do not hide during nextplayer
           Caption:='' else
           Caption:=Dictionary.ReplaceDigraphs(UTF8Encode(widestring(Data.What)));
         if (Scrabble.ActualMove=Scrabble.CurrentMove) then
         case Data.State of
          lsChange : Color:=clSilver;
          lsPlaced : Color:=fmGameOptions.FieldColor[ftLetter];
          else Color:=fmGameOptions.FieldColor[ftNewLetter];
         end else
         begin
           Color:=clGray;//fmGameOptions.Color[ftNewLetter];
           if (Data.When=Scrabble.ActualMove) then Color:=fmGameOptions.FieldColor[ftNewLetter];
         end;
         Repaint;
       end;
     end else //Data<>nil
       Visible:=false;
   end;//for to, with
end;

function TfmMain.DoSetCurrentMove(aIndex: word):word;
var
  aActiveDimension:TActiveDimension;
begin
  if not (gsComputing in Scrabble.GameState) and
  //3.0.2-10: selection of past move during network games is active
//    (not (gsNetwork in Scrabble.GameState) or (gsGameEnd in Scrabble.GameState)) and
    (Scrabble.CurrentMove>=aIndex) then
  begin
    if Scrabble.ActualMove=aIndex then
      Scrabble.ActualMove:=Scrabble.CurrentMove else
      Scrabble.ActualMove:=aIndex;
  end;
  Result:=Scrabble.ActualMove;
  if (Scrabble.Dimension=D3) then
  begin
    aActiveDimension.Axis:=TDimension(GameCourse.History[aIndex].Dimension);
    aActiveDimension.Position:=GameCourse.History[aIndex].Position;
    Scrabble.ActiveDimension:=aActiveDimension;
  end;
  if (Scrabble.ActualMove<Scrabble.CurrentMove) and
      assigned(OnUpdatePoints) then
    OnUpdatePoints(GameCourse.History[aIndex].MoveNumber mod Scrabble.NumberOfPlayers,
                   GameCourse.History[aIndex].Value);
end;


