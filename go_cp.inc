// ---------- computer ----------
procedure TfmGameOptions.tbCompExchangeChange(Sender: TObject);
var
  aLabel:TLabel;
begin
  {$IFDEF LCLGtk2}
  lbCompCambioSeccoValue.Caption:=' '+rOptions_Points;
  lbCompExchangeValue.Caption:=' '+rOptions_Points;
  lbCompChangeValue.Caption:=' '+rOptions_Points;
  lbCompChangeLetter.Caption:=' '+rOptions_Letters;
  lbCompUseJokerValue.Caption:=' '+rOptions_Points;
  stCompPerformanceValue.Caption:=' %';
  stCompTrustValue.Caption:=' %';
  {$ELSE}
  lbCompCambioSeccoValue.Caption:=inttostr(tbCompCambiosecco.Position)+' '+rOptions_Points;
  lbCompExchangeValue.Caption:=inttostr(tbCompExchange.Position)+' '+rOptions_Points;
  lbCompChangeValue.Caption:=inttostr(tbCompChangeValue.Position)+' '+rOptions_Points;
  lbCompChangeLetter.Caption:=inttostr(tbCompChangeLetters.Position)+' '+rOptions_Letters;
  lbCompUseJokerValue.Caption:=inttostr(tbCompUseJoker.Position)+' '+rOptions_Points;
  stCompPerformanceValue.Caption:=inttostr(tbCompPerformance.Position)+' %';
  stCompTrustValue.Caption:=inttostr(tbCompTrust.Position)+' %';
  {$ENDIF}
  cbCompDeterministic.Enabled:=tbCompPerformance.Position<100;

  //anchoring
  aLabel:=lbCompCambioSeccoValue;
  if Canvas.TextWidth(lbCompExchangeValue.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbCompExchangeValue;
  if Canvas.TextWidth(lbCompChangeValue.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbCompChangeValue;
  if Canvas.TextWidth(lbCompChangeLetter.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbCompChangeLetter;
  if Canvas.TextWidth(lbCompUseJokerValue.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbCompUseJokerValue;
  tbCompCambioSecco.AnchorSide[akRight].Control:=aLabel;
  tbCompExchange.AnchorSide[akRight].Control:=aLabel;
  tbCompChangeValue.AnchorSide[akRight].Control:=aLabel;
  tbCompChangeLetters.AnchorSide[akRight].Control:=aLabel;
  tbCompUseJoker.AnchorSide[akRight].Control:=aLabel;

  //enable|disable
  lbCompUseJoker.Enabled:=tbCompPerformance.Position=100;
  lbCompUseJokerValue.Enabled:=lbCompUseJoker.Enabled;
  tbCompUseJoker.Enabled:=lbCompUseJoker.Enabled;
  lbCompChange.Enabled:=lbCompUseJoker.Enabled;
  lbChangeValue.Enabled:=lbCompUseJoker.Enabled;
  lbChangeLength.Enabled:=lbCompUseJoker.Enabled;
  tbCompChangeValue.Enabled:=lbCompUseJoker.Enabled;
  tbCompChangeLetters.Enabled:=lbCompUseJoker.Enabled;
  lbCompChangeValue.Enabled:=lbCompUseJoker.Enabled;
  lbCompChangeLetter.Enabled:=lbCompUseJoker.Enabled;

  lbCompExchange.Enabled:=tbCompPerformance.Position=100;//link should be enabled to change option but disabled when perf<100
  lbCompExchangeValue.Enabled:=lbCompUseJoker.Enabled and cbCanJokerExchange.Checked;
  tbCompExchange.Enabled:=lbCompUseJoker.Enabled and cbCanJokerExchange.Checked;

  lbCompCambioSecco.Enabled:=tbCompPerformance.Position=100;
  tbCompCambioSecco.Enabled:=cbCambioSecco.Checked;
  lbCompCambioSeccoValue.Enabled:=cbCambioSecco.Checked;
end;

procedure TfmGameOptions.lbCompLinkClick(Sender: TObject);
begin
  tvOptionsNavigator.Selected:=tvOptionsNavigator.Items[pcOptions.IndexOf(tsRulesOptions)];
end;


