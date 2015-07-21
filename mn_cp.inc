{ ----- computer ----- }
procedure TfmMain.acDemoExecute(Sender: TObject);
var aBest:word;
    b:boolean;
    i:integer;
    s:string;
    fs:TFormatSettings;
begin
  if gsDemo in Scrabble.GameState then
    Bruteforce.Aborted:=true else
  begin
    if gsRunning in Scrabble.GameState then
      SaveTo(Config.Path+'demo.ssg');
    acDemo.Caption:=rMain_DemoStop;
    try
      Scrabble.GameState:=Scrabble.GameState+[gsDemo];
      fmNewGame.DemoMode:=true;
      repeat
        GenRandomize;
        fmNewGame.GameSeed:=genrandom;
        Scrabble.NewGame;
        Bruteforce.ClearMove;
        repeat
          aBest:=Bruteforce.ComputeMove(GetBruteForceSettings(true));
          //utilize cambio secco?
          if (aBest<fmGameOptions.tbCompCambioSecco.Position) and
             (Scrabble.Player[Scrabble.CurrentPlayer].CambioSecco) and
             (Scrabble.NumberOfLettersLeft>0) then
          begin
            //joker?
            b:=false;
            for i:=0 to Scrabble.RackSize-1 do
             if Scrabble.RackLetter[Scrabble.CurrentPlayer,i].IsJoker then
              b:=true;
            //execute
            if not b and Scrabble.CambioSecco then
            begin
              BruteForce.ClearMove;
              OnMessage(smInformation,Language.Format(rMain_CambioSeccoUsed,[Scrabble.Player[Scrabble.CurrentPlayer].Name]));
              aBest:=BruteForce.ComputeMove(GetBruteForceSettings(true));
            end;
          end;
          if not BruteForce.Aborted and (BruteForce.BestMovesCount>0) then
            GameCourse.BestValue[Scrabble.ActualMove]:=aBest;
          while BruteForce.Paused do
          begin
            sleep(20);
            Application.ProcessMessages;
          end;
          b:=(fmGameOptions.tbCompPerformance.Position=100) and
             (Scrabble.NumberOfLettersLeft>0) and
             (Scrabble.NumberOfPlayers*Scrabble.NumberOfPasses{%H-}-Scrabble.NoLettersPlaced>1);
          BruteForce.PlaceBestMove(0,b);
          acNextPlayerExecute(self);
          if (Scrabble.Dimension=D3) and (pn3D.Visible) then
            ScrabbleCube.Invalidate;
        until not (gsDemo in Scrabble.GameState) or
              (gsGameEnd in Scrabble.GameState) or
              Bruteforce.Aborted;
        if Config.Read('General/Demo/Save',false) then
        begin
          fs:=FormatSettings;
          fs.DateSeparator:='-';
          fs.TimeSeparator:='-';
          {$ifdef Windows}
          s:=GetWindowsSpecialDir(CSIDL_PERSONAL);
          {$else}
          s:=GetUserDir;
          {$endif}
          s:=s+DateTimeToStr(Now,fs)+'.ssg';
          SaveTo(s);
          OnMessage(smInformation,'Demo saved as '+s);
        end;
      until not (gsDemo in Scrabble.GameState) or
            Bruteforce.Aborted;
    finally
      if (gsPaused in Scrabble.GameState) then
        Scrabble.TogglePaused;
      acDemo.Caption:=rMain_DemoRun;
      fmNewGame.DemoMode:=false;
      Scrabble.GameState:=Scrabble.GameState-[gsDemo];
      Scrabble.Reset;
      GameCourse.Clear;
      if FileExistsUTF8(Config.Path+'demo.ssg') then
      begin
        LoadFrom(UTF8ToSys(Config.Path+'demo.ssg'));
        DeleteFileUTF8(UTF8ToSys(Config.Path+'demo.ssg'));
      end;
    end;
  end;//gsDemo in State
end;

procedure TfmMain.acBestMoveExecute(Sender: TObject);
var
  aValue,aBest,i: word;
  aWord: string;
  aPosition,s: string;
  aDimension: TActiveDimension;
  ex: boolean;
begin
  if not (gsComputing in Scrabble.GameState) then
  begin
    tbBestMove.Down:=true;
    Scrabble.RestoreBoard;
    aBest:=BruteForce.ComputeMove(GetBruteForceSettings(true));

    if not BruteForce.Aborted and (BruteForce.BestMovesCount>0) then
      GameCourse.BestValue[Scrabble.ActualMove]:=aBest;

    seBestMove.Enabled:=false;
    udBestMove.Max:=BruteForce.BestMovesCount;
    udBestMove.Position:=0;
    tbBestMove.Down:=false;
    if BruteForce.BestMovesCount>0 then
    begin
      BruteForce.Report(0,aValue,aWord,aPosition,aDimension);
      s:=Language.Plural(rMain_CalcResult,BruteForce.BestMovesCount)+' '+Language.Plural(rMain_Points,aBest);

      if aValue<aBest then
        s:=s+', '+rMain_CalcComp+' '+Language.Plural(rMain_Points,aValue);
      s:=s+'.';
      OnMessage(smInformation,s);

      seBestMove.Enabled:=true;
      seBestMove.Text:='';
      if Scrabble.CurrentMove<>Scrabble.ActualMove then
      with GameCourse.History[Scrabble.ActualMove] do
      begin
        for i:=0 to Bruteforce.BestMovesCount-1 do
        begin
          BruteForce.Report(i,aValue,aWord,aPosition,aDimension);
          if i=0 then GameCourse.BestValue[Scrabble.ActualMove]:=aValue;
          if (aValue=Value) and (aWord=PlacedWord) then
          begin
            seBestMove.Text:=inttostr(i+1);
            break;
          end;
        end;
        //clear listpos in bruteforce when move is not found
        if seBestMove.Text='' then
          BruteForce.Report(MaxInt,aValue,aWord,aPosition,aDimension);
      end;
    end else
    begin
      seBestMove.Enabled:=false;
      if (Scrabble.CurrentMove=Scrabble.ActualMove) and
         (Scrabble.Player[Scrabble.CurrentPlayer].CambioSecco) then
        OnMessage(smInformation,rMain_RecommendCambioSecco) else
      begin
        ex:=(Scrabble.Player[Scrabble.CurrentPlayer].Name='Computer') and
            (fmGameOptions.tbCompPerformance.Position=100) and
            (Scrabble.NumberOfLettersLeft>0) and
            (Scrabble.NumberOfPlayers*Scrabble.NumberOfPasses{%H-}-Scrabble.NoLettersPlaced>1);
        BruteForce.PlaceBestMove(0,ex);//exchange  -> in bf anpassen
      end;
    end;
  end else
  begin
    BruteForce.Aborted:=true;
    tbBestMove.Down:=false;
  end;
end;

procedure TfmMain.DoAbortCalculation(Sender: TObject);
begin
  BruteForce.Aborted:=true;
end;

function TfmMain.GetBruteForceSettings(const MaxPerformance:boolean): TBruteForceSettings;
var
  i:integer;
begin
  if MaxPerformance then
    Result.Performance:=100 else
    Result.Performance:=fmGameOptions.tbCompPerformance.Position;
  Result.ExchangeLetters:=fmGameOptions.tbCompExchange.Position;
  Result.ExchangeValue:=fmGameOptions.tbCompChangeValue.Position;
  Result.ExchangeJoker:=fmGameOptions.tbCompChangeLetters.Position;
  Result.CompleteExchange:=fmGameOptions.cbCompleteExchange.Checked;
  Result.Deterministic:=fmGameOptions.cbCompDeterministic.Checked;
  Result.RunThreaded:=fmGameOptions.cbParallel.Checked;
  Result.UseJoker:=fmGameOptions.tbCompUseJoker.Position;
  Result.LeftToRight:=fmGameOptions.rbLeftToRight.Checked;
  Result.RomanLabels:=fmGameOptions.cbRoman.Checked;
  //http://17085.homepagemodules.de/t2100f175-behoben-Patch-Rollstuhlbutton-Spielanalyse-findet-kein-Wort.html#msg9875107
  if Scrabble.ActualMove=Scrabble.CurrentMove then
  begin
    Result.IsFirstMove:=true;
    for i:=0 to GameCourse.HistoryCount-1 do
     if TGameCourseMove(GameCourse.History[i]).Value>0 then
     begin
       Result.IsFirstMove:=false;
       exit;
     end;
  end else
    Result.IsFirstMove:=(Scrabble.ActualMove=0);
end;

procedure TfmMain.acGameAnalysisExecute(Sender: TObject);

 procedure StoreBestValues;
 begin
   if not Bruteforce.Aborted and
      not (gsKibitz in Scrabble.GameState) and
      IsGameServer then
   with TStringList.Create do
   try
     Delimiter:=nwDelimiter;
     StrictDelimiter:=true;
     Values['GameID']:=inttostr(TCPClient.PlayerData.GameID);
     Values['BestValues']:=GameCourse.AllBestMovesValues;
     OnMessage(smDebug,'Sending results to game server...');
     TCPClient.OnSend('nwBestValues','group',DelimitedText);
   finally
     Free;
   end;
 end;

type TResult=record
        Placed : word;
        Best   : word;
     end;
var i,j,aBest:integer;
    s:string;
    aStart:TDateTime;
    Result:array of TResult;
    aForm : TForm;
    aButton : TButton;
    aProgress : TProgressbar;
    aLabel : TLabel;
    {%H-}IsNew:boolean;
    OldClientState:TClientState;
begin
  aForm:=TForm.Create(self);
  aButton:=TButton.Create(self);
  aLabel:=TLabel.Create(self);
  aProgress:=TProgressBar.Create(self);
  setlength(Result,Scrabble.NumberOfPlayers);
  Scrabble.GameState:=Scrabble.GameState+[gsDemo];//avoid timer abort
  try
    with aForm do
    begin
      Parent:=nil;
      Width:=250; Height:=150;
      FormStyle:=fsStayOnTop;
      Position:=poMainFormCenter;
      BorderStyle:=bsToolWindow;
      BorderIcons:=[];
      Caption:=rMain_GameAnalysis;
    end;
    with aButton do
    begin
      Parent:=aForm;
      Caption:=rMain_Abort;
      Width:=100;Left:=75;Height:=25;Top:=120;
      OnClick:=@DoAbortCalculation;
    end;
    with aProgress do
    begin
      Parent:=aForm;
      Left:=10; Width:=230; Top:=95;
      Max:=GameCourse.HistoryCount-1;
    end;
    with aLabel do
    begin
      Parent:=aForm;
      Left:=10;Top:=10;
    end;
    aForm.Show;
    for i:=0 to Scrabble.NumberOfPlayers-1 do
    begin
      Result[i].Placed:=0;
      Result[i].Best:=0;
    end;
    IsNew:=true;
    BruteForce.Aborted:=false;// only set automatically by computemove if Scrabble.History[i].Best>-1
    if IsGameServer then
    begin
      OldClientState:=TCPClient.PlayerData.ClientState;
      TCPClient.OnSend('nwRefresh','all','SetState=csBusy');
    end;
    try
      aStart:=Now;
      for i:=0 to GameCourse.HistoryCount-1 do //last history after game end
      begin
        Scrabble.ActualMove:=i;  //executes BruteForce.ClearMove;
        aLabel.Caption:='';
        for j:=0 to Scrabble.NumberOfPlayers-1 do
          aLabel.Caption:=aLabel.Caption+
            Language.Format('%s: %.2f%% (%d/%d)',[Scrabble.Player[j].Name,
                                        (Result[j].Placed/Result[j].Best)*100,
                                         Result[j].Placed,Result[j].Best])+LineBreak;
        aProgress.Position:=i;
        Application.ProcessMessages;

        if GameCourse.History[i].Best=-1 then
          aBest:=BruteForce.ComputeMove(GetBruteForceSettings(true)) else
          IsNew:=false;
        if not BruteForce.Aborted and (BruteForce.BestMovesCount>0) then
          GameCourse.BestValue[i]:=aBest;

        if Bruteforce.Aborted then exit;

        inc(Result[i mod Scrabble.NumberOfPlayers].Placed,GameCourse.History[i].Value);
        if GameCourse.History[i].Best>-1 then
          inc(Result[i mod Scrabble.NumberOfPlayers].Best,GameCourse.History[i].Best);

        //save best values every 10min
        if SecondsBetween(Now,aStart)>600 then
        begin
          StoreBestValues;
          aStart:=Now;
        end;
      end;
      s:='';
      for i:=0 to Scrabble.NumberOfPlayers-1 do
      begin
        s:=s+Language.Format('%s: %.2f%% (%d/%d), δ=%.2f',
                  [Scrabble.Player[i].Name,
                   (Result[i].Placed/Result[i].Best)*100,
                   Result[i].Placed,Result[i].Best,
                   Result[i].Placed/(Scrabble.AllLettersValue/Scrabble.NumberOfPlayers)]);
        if i<Scrabble.NumberOfPlayers-1 then s:=s+LineBreak;
      end;
      StoreBestValues;
      OnMessage(smGameResult,s);
    finally
      if IsGameServer then
        TCPClient.OnSend('nwRefresh','all','SetState='+ClientStateToString(OldClientState));
    end;
  finally
    Scrabble.GameState:=Scrabble.GameState-[gsDemo];//avoid timer abort
    Scrabble.ActualMove:=Scrabble.CurrentMove;
    setlength(result,0);
    aLabel.Free;
    aProgress.Free;
    aButton.Free;
    aForm.Free;
  end;
end;

procedure TfmMain.udBestMoveChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
{$ifdef Darwin}
var
  z: word;
{$endif}
begin
  {$ifdef Darwin}
  {$Warning UpDown fires changing before text is applied}
  z:=VK_RETURN;
  seBestMoveKeyUp(self,z,[]);
  {$endif}
end;

procedure TfmMain.seBestMoveKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not (key in [VK_UNKNOWN..VK_SPACE-1,VK_SPACE+1..VK_HELP,
                  VK_0..VK_9,VK_NUMPAD0..VK_NUMPAD9]) then
    key:=0;
end;

procedure TfmMain.seBestMoveKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i:integer;
begin
  i:=StrToIntDef(seBestMove.Text,1);
  if key=VK_UP then inc(i);
  if key=VK_DOWN then dec(i);
  if i>BruteForce.BestMovesCount then
    i:=BruteForce.BestMovesCount;
  if i<1 then i:=1;
  seBestMove.Text:=IntToStr(i);
  {$ifdef Darwin}
  seBestMoveChange(self);
  {$endif}
end;

procedure TfmMain.seBestMoveChange(Sender: TObject);
var
  aValue : word;
  aWord  : string;
  aDimension : TActiveDimension;
  aPosition  : string;
begin
  if seBestMove.Enabled and
     (seBestMove.Text<>'') and
     (BruteForce.BestMovesCount>0) then
  begin
    BruteForce.Report(StrToInt(seBestMove.Text)-1,aValue,aWord,aPosition,aDimension);
    Scrabble.ActiveDimension:=aDimension;
    if Scrabble.ActualMove=Scrabble.CurrentMove then
      BruteForce.PlaceBestMove(StrToInt(seBestMove.Text)-1,false) else
      ScrabbleGrid.Paint(self);
    if assigned(OnUpdatePoints) then
      OnUpdatePoints(Scrabble.ActualMove mod Scrabble.NumberOfPlayers,aValue);
  end;
end;

procedure TfmMain.DoMoveNumberChange(Sender: TObject);
begin
  BruteForce.ClearMove;
  DoRepaint(seUnknown);
end;

procedure TfmMain.ComputeMove;
var
  b:boolean;
  i:byte;
  aBest : word;
  aWord  : string;
  aDimension : TActiveDimension;
  aPosition  : string;
begin
  tbBestMove.Down:=true;
  aBest:=BruteForce.ComputeMove(GetBruteForceSettings(false));
  //utilize cambio secco?
  if not Bruteforce.Aborted and
     (aBest<fmGameOptions.tbCompCambioSecco.Position) and
     (fmGameOptions.tbCompPerformance.Position=100) and
     (Scrabble.Player[Scrabble.CurrentPlayer].CambioSecco) and
     (Scrabble.NumberOfLettersLeft>0) then
  begin
    //joker?
    b:=false;
    for i:=0 to Scrabble.RackSize-1 do
     if Scrabble.RackLetter[Scrabble.CurrentPlayer,i].IsJoker then
      b:=true;
    //execute
    if not b and Scrabble.CambioSecco then
    begin
      BruteForce.ClearMove;
      OnMessage(smInformation,Language.Format(rMain_CambioSeccoUsed,['Computer']));
      aBest:=BruteForce.ComputeMove(GetBruteForceSettings(false));
    end;
  end;
  tbBestMove.Down:=false;

  if not Bruteforce.Aborted then
    GameCourse.BestValue[Scrabble.ActualMove]:=aBest;

  BruteForce.Report(0,aBest,aWord,aPosition,aDimension);
  Scrabble.ActiveDimension:=aDimension;

  b:=(Scrabble.Player[Scrabble.CurrentPlayer].Name='Computer') and
     (fmGameOptions.tbCompPerformance.Position=100) and
     (Scrabble.NumberOfLettersLeft>0) and
     (Scrabble.NumberOfPlayers*Scrabble.NumberOfPasses{%H-}-Scrabble.NoLettersPlaced>1);
  BruteForce.PlaceBestMove(0,b);

  if assigned(OnUpdatePoints) then
    OnUpdatePoints(Scrabble.ActualMove mod Scrabble.NumberOfPlayers,aBest);

  acNextPlayerExecute(self);//computer
end;


