procedure TGame.SaveTo(const aValue: string);
var s    : string;
    x,y,z : integer;
begin
  RestoreBoard; //lsBoard should not be saved
  with TIniFile.Create(aValue) do
  try
    WriteString('General','Version','3.1-24');

    EraseSection('Game');
    EraseSection('Letter');

    WriteInteger('Game','PlayerCount',length(FPlayers));
    for x:=0 to length(FPlayers)-1 do
    begin
      WriteString('Player'+inttostr(x),'Name',FPlayers[x].Name);
      WriteInteger('Player'+inttostr(x),'Value',FPlayers[x].Points);//value=points prior v3.0.2-10
      WriteInteger('Player'+inttostr(x),'TimeElapsed',FPlayers[x].mTimeElapsed);
      WriteBool('Player'+inttostr(x),'CambioSecco',FPlayers[x].CambioSecco);
      WriteInteger('Player'+inttostr(x),'TimePenaltyLeft',FPlayers[x].TimePenaltyLeft);
    end;

    WriteInteger('Game','Dimensions',integer(FDimension));
    WriteInteger('Game','BankSize',RackSize);//"bank" for legacy reason
    WriteInteger('Game','CurrentMove',FCurrentMove);
    WriteInteger('Game','CurrentPlayer',FCurrentPlayer);
    WriteInteger('Game','RandSeed',FRandSeed);
    WriteInteger('Game','BoardSize',BoardSize);
    WriteInteger('Game','AllLetterBonus',FGameEndBonus);
    WriteInteger('Game','NoLettersPlaced',FNoLettersPlaced);
    WriteInteger('Game','LastNoLettersPlaced',FLastNoLettersPlaced);
    WriteInteger('Game','PreviousMoveState',integer(FPreviousMoveState));

    WriteInteger('Game','WordCheckMode',integer(FWordCheckMode));
    WriteInteger('Game','WordCheckPeriod',FWordCheckPeriod);
    WriteInteger('Game','WordCheckPenalty',FWordCheckPenalty);
    WriteInteger('Game','WordCheckBonus',FWordCheckBonus);

    WriteBool('Game','CanJokerExchange',gsJokerExchange in FGameState);
    WriteBool('Game','Jokerization',gsJokerization in FGameState);
    WriteInteger('Game','NumberOfPasses',FNumberOfPasses);
    WriteInteger('Game','TimeControlSetting',integer(FTimeControlSetting));
    WriteInteger('Game','TimeControlValue',integer(FTimeControlValue));
    WriteInteger('Game','TimePenaltyValue',FTimePenaltyValue);
    WriteInteger('Game','TimePenaltyCount',FTimePenaltyCount);
    WriteBool('Game','TimeGameLost',FTimeGameLost);

    WriteInteger('Game','LettersLeft',FLettersLeft);
    WriteInteger('Game','JokerPenalty',FJokerPenalty);
    WriteInteger('Game','LimitedExchange',FLimitedExchange);
    WriteBool('Game','ChangeIsPass',gsChangeIsPass in FGameState);
    WriteBool('Game','AddLettersLeft',gsAddLettersLeft in FGameState);
    WriteBool('Game','SubstractLettersLeft',gsSubstractLettersLeft in FGameState);
    WriteBool('Game','CambioSecco',gsCambioSecco in FGameState);
    WriteBool('Game','CLABBERS',gsCLABBERS in FGameState);
    WriteInteger('Game','ScrabbleBonus',ScrabbleBonus);

    WriteBool('Game','GameEnd',gsGameEnd in GameState);
    for z:=0 to integer(FDimension=D3)*(BoardSize-1) do
     for x:=0 to BoardSize-1 do
     begin
       s:='';
       for y:=0 to BoardSize-1 do
        if integer(FieldType[x,y,z])<10 then
          s:=s+inttostr(integer(FieldType[x,y,z])) else
          s:=s+chr(integer(FieldType[x,y,z])+55);
       WriteString('Game','Board_'+inttostr(z*BoardSize+x),s);
      end; //for z,x

    WriteInteger('Game','MoveTime',GetMoveTime_ms);

    for x:=0 to LetterList.Count-1 do
     with TLetter(LetterList[x]) do
      begin
        s:=UTF8Encode(widestring(What))+','+
           IntToStr(Value)+','+
           IntToStr(Where[dx])+','+
           IntToStr(Where[dy])+','+
           IntToStr(Where[dz])+','+
           IntToStr(Who)+','+
           IntToStr(When)+','+
           IntToStr(RackPos)+','+
           BoolToStr(IsJoker)+','+
           BoolToStr(IsRandom)+','+
           IntToStr(integer(State))+
           ';';
        WriteString('Letter',IntToStr(x),s);
      end;
  finally
    Free;
  end;
end;

procedure TGame.LoadFrom(const aValue: string);
var
  ws       : WideString;
  x,y,z,i,e   : integer;
  sl      : TStringList;
  aLetter : TLetter;
begin
  with TIniFile.Create(aValue) do
  try
    ws:=ReadString('General','Version','0');
    if ws<>'3.1-24' then
      raise EInOutError.Create('File version '+UTF8Encode(ws)+'<3.1-24 Loading aborted!');

    ClearLetterList;

    BoardSize:=ReadInteger('Game','BoardSize',0);
    FDimension:=TScrabbleDimension(ReadInteger('Game','Dimensions',0));
    for z:=0 to integer(FDimension=D3)*(BoardSize-1) do
     for x:=0 to BoardSize-1 do
     begin
       ws:=UTF8Decode(ReadString('Game','Board_'+inttostr(z*BoardSize+x),''));
       for y:=0 to length(ws)-1 do
       begin
         Val(ws[y+1],i,e);
         if e=0 then
           FieldType[x,y,z]:=TFieldType(i) else //StrToIntDef(s[y+1],0)
         if (ord(ws[y+1])-55>=10) and (ord(ws[y+1])-55<=integer(high(TFieldType))) then
           FieldType[x,y,z]:=TFieldType(ord(ws[y+1])-55);
       end;
     end;

    RackSize:=ReadInteger('Game','BankSize',0);
    FGameEndBonus:=ReadInteger('Game','AllLetterBonus',0);
    FCurrentPlayer:=ReadInteger('Game','CurrentPlayer',0);
    FLocalPlayer:=FCurrentPlayer;
    FActiveDimension.Axis:=dx;
    FActiveDimension.Position:=(BoardSize div 2)*integer(FDimension=D3);
    FRandSeed:=ReadInteger('Game','RandSeed',FRandSeed);
    RandSeed:=FRandSeed;
    FCurrentMove:=ReadInteger('Game','CurrentMove',0);
    FActualMove:=FCurrentMove;
    FNumberOfPasses:=ReadInteger('Game','NumberOfPasses',2);
    FJokerPenalty:=ReadInteger('Game','JokerPenalty',0);
    FTimeControlSetting:=TTimeControlSetting(ReadInteger('Game','TimeControlSetting',0));
    FTimeControlValue:=ReadInteger('Game','TimeControlValue',MaxInt);
    FTimePenaltyValue:=ReadInteger('Game','TimePenaltyValue',0);
    FTimePenaltyCount:=ReadInteger('Game','TimePenaltyCount',0);
    FTimeGameLost:=ReadBool('Game','TimeGameLost',false);

    FLettersLeft:=ReadInteger('Game','LettersLeft',0);

    FGameState:=[];
    if ReadBool('Game','GameEnd',true) then include(FGameState,gsGameEnd);
    if not (gsGameEnd in Scrabble.GameState) then
      FGameState:=FGameState+[gsRunning];
    if ReadBool('Game','ChangeIsPass',true) then
      include(FGameState,gsChangeIsPass) else
      exclude(FGameState,gsChangeIsPass);
    if ReadBool('Game','AddLettersLeft',true) then
      include(FGameState,gsAddLettersLeft) else
      exclude(FGameState,gsAddLettersLeft);
    if ReadBool('Game','SubstractLettersLeft',true) then
      include(FGameState,gsSubstractLettersLeft) else
      exclude(FGameState,gsSubstractLettersLeft);
    if ReadBool('Game','CanJokerExchange',true) then
      include(FGameState,gsJokerExchange) else
      exclude(FGameState,gsJokerExchange);
    if ReadBool('Game','Jokerization',true) then
      include(FGameState,gsJokerization) else
      exclude(FGameState,gsJokerization);
    if ReadBool('Game','CambioSecco',true) then
      include(FGameState,gsCambioSecco) else
      exclude(FGameState,gsCambioSecco);
    if ReadBool('Game','CLABBERS',true) then
      include(FGameState,gsCLABBERS) else
      exclude(FGameState,gsCLABBERS);
    FLimitedExchange:=ReadInteger('Game','LimitedExchange',RackSize);

    FWordCheckMode:=TWordCheckMode(ReadInteger('Game','WordCheckMode',2));
    FWordCheckPeriod:=ReadInteger('Game','WordCheckPeriod',30);
    FWordCheckPenalty:=ReadInteger('Game','WordCheckPenalty',10);
    FWordCheckBonus:=ReadInteger('Game','WordCheckBonus',0);

    FNoLettersPlaced:=ReadInteger('Game','NoLettersPlaced',0);
    FLastNoLettersPlaced:=ReadInteger('Game','LastNoLettersPlaced',0);
    FPreviousMoveState:=TMoveState(ReadInteger('Game','PreviousMoveState',integer(msNone)));
    ScrabbleBonus:=ReadInteger('Game','ScrabbleBonus',50);

    setlength(FPlayers,ReadInteger('Game','PlayerCount',0));
    for x:=0 to length(FPlayers)-1 do
    begin
      FPlayers[x].Name:=ReadString('Player'+inttostr(x),'Name','Player'+inttostr(x));
      FPlayers[x].Points:=ReadInteger('Player'+inttostr(x),'Value',0);
      FPlayers[x].mTimeElapsed:=ReadInteger('Player'+inttostr(x),'TimeElapsed',0);
      FPlayers[x].CambioSecco:=ReadBool('Player'+inttostr(x),'CambioSecco',false);
      FPlayers[x].TimePenaltyLeft:=ReadInteger('Player'+inttostr(x),'TimePenaltyLeft',0);
    end;
    FMoveTime:=IncMilliSecond(Now,-ReadInteger('Game','MoveTime',0));
    FPauseTime:=0;
    sl:=TStringList.Create;
    try
      ReadSection('Letter',sl);
      for x:=0 to sl.Count-1 do
      begin
        ws:=UTF8Decode(ReadString('Letter',sl[x],''));
        aLetter:=TLetter.Create;
        with aLetter do
        begin
          y:=pos(',',ws); What:=Copy(ws,1,y-1)[1]; System.Delete(ws,1,y);
          y:=pos(',',ws); Value:=StrToInt(Copy(ws,1,y-1)); System.Delete(ws,1,y);
          if (Value=255) and assigned(FOnGetLetterValue) then Value:=FOnGetLetterValue(What);
          y:=pos(',',ws); Where[dx]:=StrToInt(Copy(ws,1,y-1)); System.Delete(ws,1,y);
          y:=pos(',',ws); Where[dy]:=StrToInt(Copy(ws,1,y-1)); System.Delete(ws,1,y);
          y:=pos(',',ws); Where[dz]:=StrToInt(Copy(ws,1,y-1)); System.Delete(ws,1,y);
          y:=pos(',',ws); Who:=StrToInt(Copy(ws,1,y-1));       System.Delete(ws,1,y);
          y:=pos(',',ws); When:=StrToInt(Copy(ws,1,y-1));      System.Delete(ws,1,y);
          y:=pos(',',ws); RackPos:=StrToInt(Copy(ws,1,y-1));   System.Delete(ws,1,y);
          y:=pos(',',ws); IsJoker:=StrToBool(Copy(ws,1,y-1));   System.Delete(ws,1,y);
          y:=pos(',',ws); if y>0 then //hack for older ssg
                          begin
                            IsRandom:=StrToBool(Copy(ws,1,y-1));
                            System.Delete(ws,1,y);
                          end else
                            IsRandom:=false;
          y:=pos(';',ws); State:=TLetterState(StrToInt(Copy(ws,1,y-1))); System.Delete(ws,1,y);
        end;
        LetterList.Add(aLetter);
      end;//sl.count
      finally
        sl.Free;
      end;//stringlist
  finally
    Free;
  end;//ini
  include(FGameState,gsActive);
  UpdateMoveState;
  if assigned(FOnRepaint) then
    FOnRepaint(seLoadGame);
end;
