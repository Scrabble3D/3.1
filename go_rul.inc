// ---------- rules ------------

procedure TfmGameOptions.RulesChanged(Sender: TObject);
const
  cInfo:array[0..7] of TConfigOptions=(coInfoLetterset,coInfoPieces,coInfoJokers,
                                       coInfoRandoms,coInfoBoard,coInfoTime,
                                       coInfoTimePanalty,coCambioSecco);
var
  aRow:integer;
begin
  {$ifdef darwin}cbRulesSet.OnChange:=nil;{$endif}
  cbRulesSet.ItemIndex:=cbRulesSet.Items.IndexOf(RulesSet);
  {$ifdef darwin}cbRulesSet.OnChange:=@cbRulesSetChange;{$endif}

  for aRow:=0 to high(cInfo) do
  begin
    with ConfigString(cInfo[aRow]) do
    begin
      sgRules.Cells[0,aRow]:=Key;
      case aRow of
       0..3 : sgRules.Cells[1,aRow]:=Value+cTS+'tsLetter';
       4    : sgRules.Cells[1,aRow]:=Value+cTS+'tsBoard';
       5..6 : sgRules.Cells[1,aRow]:=Value+cTS+'tsTime';
       7    : sgRules.Cells[1,aRow]:=Value+cTS+'tsRulesOptions';
      end;//case
    end;
  end;
  sgResize(sgRules);

  UpdatePresetInfo;
  FSettingsChanged:=true;
end;

function TfmGameOptions.GetRulesSet: string;
 function Compare(lang:TDefaultsLanguage):boolean;
 begin
   Result:=((rbNoLimit.Checked and (Defaults.Value[lang].TimeControl=0)) or
            (rbPerMove.Checked and (Defaults.Value[lang].TimeControl=1)) or
            (rbPerGame.Checked and (Defaults.Value[lang].TimeControl=2))
           ) and
      (rbTimePenaltyEndGame.Checked=Defaults.Value[lang].TimeControlEnd) and
      (rbTimePenaltyBuyTime.Checked=Defaults.Value[lang].TimeControlBuy) and
      (meTimeLimitPerGame.Text=Defaults.Value[lang].TimePerGame) and
      (seTimePenaltyValue.Value=Defaults.Value[lang].PenaltyValue) and
      (seTimePenaltyCount.Value=Defaults.Value[lang].PenaltyCount) and
      (cbLostByTime.Checked=Defaults.Value[lang].GameLostByTime) and
      (cbCanJokerExchange.Checked=Defaults.Value[lang].JokerExchange) and
      (cbChangeIsPass.Checked=Defaults.Value[lang].ChangeIsPass) and
      (cbCambioSecco.Checked=Defaults.Value[lang].CambioSecco) and
      (cbJokerization.Checked=false) and
      (cbSubstractLettersLeft.Checked=Defaults.Value[lang].SubstractLetters) and
      (cbAddLettersLeft.Checked=Defaults.Value[lang].AddLetters) and
      (seJokerPenalty.Value=Defaults.Value[lang].JokerPenalty) and
      (seNumberOfPasses.Value=Defaults.Value[lang].NumberOfPasses) and
      (seLimitExchange.Value=Defaults.Value[lang].LimitExchange) and
      (seScrabbleBonus.Value=Defaults.Value[lang].ScrabbleBonus) and
      (seGameEndBonus.Value=Defaults.Value[lang].EndBonus);
 end;
var
  lang:TDefaultsLanguage;
begin
  if (cbLetterSet.ItemIndex>-1) and //known letterset
     (cbBoard.ItemIndex=0) then  //classic scrabble
  begin
    //correct set
    if cbRulesSet.ItemIndex>-1 then
     for lang:=low(TDefaultsLanguage) to high(TDefaultsLanguage) do
      with Defaults.Value[lang] do
       if (English+' ('+Native+')'=cbRulesSet.Items[cbRulesSet.ItemIndex]) and Compare(lang) then
        exit(English+' ('+Native+')');
    //update
    for lang:=low(TDefaultsLanguage) to high(TDefaultsLanguage) do
     with Defaults.Value[lang] do
      if RulesValid and
         (cbLetterSet.Text=English+' ('+Native+')') and
         Compare(lang) then
       exit(English+' ('+Native+')');
  end;
end;

procedure TfmGameOptions.SetRulesSet(const aValue: string);
var
  tmp:TByteSet;
  i,j:integer;
begin
  case Defaults[aValue].TimeControl of
   1:rbPerMove.Checked:=true;
   2:rbPerGame.Checked:=true;
   else rbNoLimit.Checked:=true;
  end;
  rbTimePenaltyEndGame.Checked:=Defaults[aValue].TimeControlEnd;
  rbTimePenaltyBuyTime.Checked:=Defaults[aValue].TimeControlBuy;
  meTimeLimitPerGame.Text:=Defaults[aValue].TimePerGame;
  seTimePenaltyValue.Value:=Defaults[aValue].PenaltyValue;
  seTimePenaltyCount.Value:=Defaults[aValue].PenaltyCount;
  rbTimePenaltyChange(self);
  rbTimeControlChange(self);
  cbLostByTime.Checked:=Defaults[aValue].GameLostByTime;

  cbCanJokerExchange.Checked:=Defaults[aValue].JokerExchange;
  cbChangeIsPass.Checked:=Defaults[aValue].ChangeIsPass;
  cbCambioSecco.Checked:=Defaults[aValue].CambioSecco;
  cbJokerization.Checked:=false;
  cbSubstractLettersLeft.Checked:=Defaults[aValue].SubstractLetters;
  cbAddLettersLeft.Checked:=Defaults[aValue].AddLetters;
  seJokerPenalty.Value:=Defaults[aValue].JokerPenalty;
  seNumberOfPasses.Value:=Defaults[aValue].NumberOfPasses;
  seLimitExchange.Value:=Defaults[aValue].LimitExchange;
  seGameEndBonus.Value:=Defaults[aValue].EndBonus;
  seScrabbleBonus.Value:=Defaults[aValue].ScrabbleBonus;
  {$ifdef darwin}cbRulesSet.OnChange:=nil;{$endif}
  cbRulesSet.ItemIndex:=cbRulesSet.Items.IndexOf(aValue);
  {$ifdef darwin}cbRulesSet.OnChange:=@cbRulesSetChange;{$endif}

  case Defaults[aValue].WordCheckMode of
   0:rbWCMTakeBack.Checked:=true;
   1:rbWCMPoll.Checked:=true;
   2:rbWCMChallenge.Checked:=true;
  end;

  with TStringList.Create do
  try
    DelimitedText:=Defaults[aValue].ExcludedCat;
    tmp:=[];
    for i:=0 to Count-1 do
    begin
      j:=StrToIntDef(Strings[i],0);
      if j<>0 then include(tmp,j);
    end;
    Dictionary.Excluded:=tmp;
    for i:=0 to gbCategories.Items.Count-1 do
     gbCategories.Checked[i]:=not (i in Dictionary.Excluded);

  finally
    Free;
  end;

  cbCLABBERS.Checked:=false; //CLABBERS is not default in any tournament rule
end;

procedure TfmGameOptions.rbTimeControlChange(Sender: TObject);
begin
  meTimeLimitPerMove.Enabled:=false;
  meTimeLimitPerGame.Enabled:=false;
  rbTimePenaltyEndGame.Enabled:=false;
  rbTimePenaltyBuyTime.Enabled:=false;
  cbLostByTime.Enabled:=false;
  if rbPerMove.Checked then
    meTimeLimitPerMove.Enabled:=true;
  if rbPerGame.Checked then
  begin
    meTimeLimitPerGame.Enabled:=true;
    rbTimePenaltyEndGame.Enabled:=true;
    rbTimePenaltyBuyTime.Enabled:=true;
    cbLostByTime.Enabled:=true;
  end;
  rbTimePenaltyChange(self);
end;

procedure TfmGameOptions.rbTimePenaltyChange(Sender: TObject);
begin
  seTimePenaltyValue.Enabled:=rbTimePenaltyBuyTime.Checked and rbPerGame.Checked;
  lbTimePenaltyValue.Enabled:=rbTimePenaltyBuyTime.Checked and rbPerGame.Checked;
  seTimePenaltyCount.Enabled:=rbTimePenaltyBuyTime.Checked and rbPerGame.Checked;
  lbTimePenaltyCount.Enabled:=rbTimePenaltyBuyTime.Checked and rbPerGame.Checked;
  lbTimePenaltyCountLabel.Enabled:=rbTimePenaltyBuyTime.Checked and rbPerGame.Checked;
  RulesChanged(nil);
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.cbCanJokerExchangeChange(Sender: TObject);
begin
  tbCompExchange.Enabled:=cbCanJokerExchange.Checked and (tbCompPerformance.Position=100);
  lbCompExchangeValue.Enabled:=cbCanJokerExchange.Checked and (tbCompPerformance.Position=100);
  RulesChanged(nil);
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.cbCLABBERSChange(Sender: TObject);
begin
  RulesChanged(nil);
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.cbCambioSeccoChange(Sender: TObject);
begin
  tbCompCambioSecco.Enabled:=cbCambioSecco.Checked and (tbCompPerformance.Position=100);
  lbCompCambioSeccoValue.Enabled:=cbCambioSecco.Checked and (tbCompPerformance.Position=100);
  RulesChanged(nil);
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.cbAddSubstractChange(Sender: TObject);
begin
  cbAddLettersLeft.Enabled:=cbSubstractLettersLeft.Checked;
  if not cbAddLettersLeft.Enabled then cbAddLettersLeft.Checked:=false;
  lbJokerPenalty.Enabled:=cbSubstractLettersLeft.Checked;
  seJokerPenalty.Enabled:=cbSubstractLettersLeft.Checked;
  RulesChanged(nil);
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.seNumberOfPassesEditingDone(Sender: TObject);
begin
  if seNumberOfPasses.Value>10 then
    seNumberOfPasses.Value:=10;
  if seNumberOfPasses.Value<1 then
    seNumberOfPasses.Value:=1;
end;

