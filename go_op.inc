// ---------- network -----------
procedure TfmGameOptions.ImageClick(Sender: TObject);
begin
  with TOpenPictureDialog.Create(self) do
  try
    if Execute then
      (Sender as TImage).Picture.LoadFromFile(FileName);
    VisualChange(self);
  finally
    Free;
  end;
end;

//event is not fired on Qt
procedure TfmGameOptions.lvConfigurationCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  aRect:TRect;
begin
  if Item.Index=Sender.Items.Count-integer(high(TDefaultsLanguage))-1 then
  begin
    aRect:=Item.DisplayRect(drBounds);
    DrawEdge(Sender.Canvas.Handle, aRect, EDGE_ETCHED, BF_TOP);
  end;
  DefaultDraw:=true;
end;

procedure TfmGameOptions.lvConfigurationSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  acLoadPreset.Execute;
end;

procedure TfmGameOptions.lvDictionariesDblClick(Sender: TObject);
begin
  acLoadDictionary.Execute;
end;

procedure TfmGameOptions.btnOpenConfigDirClick(Sender: TObject);
begin
  OpenDocument(edConfigDir.Text);
end;

procedure TfmGameOptions.btnUpdateClick(Sender: TObject);
begin
  Updater.CheckForUpdates;
end;

// ---------- popups/sounds ----------

procedure TfmGameOptions.sbSndClick(Sender: TObject);
begin
  with (Sender as TSpeedButton) do
   if Down then ilGlyphs.GetBitmap(1,Glyph)
           else ilGlyphs.GetBitmap(0,Glyph);
  VisualChange(self);
end;

procedure TfmGameOptions.UpdateLanguage;
var
  i:integer;
  aLabel:TLabel;
begin
  //sgLocalization
  sgLocalization.RowCount:=4;
  sgLocalization.Cells[0,0]:=rOptions_Filename; sgDesign.Cells[0,0]:=rOptions_Filename;
  sgLocalization.Cells[1,0]:=Language.Info[liFileName];
  sgLocalization.Cells[0,1]:=rOptions_Author;   sgDesign.Cells[0,1]:=rOptions_Author;
  sgLocalization.Cells[1,1]:=Language.Info[liAuthor];
  sgLocalization.Cells[0,2]:=rOptions_Date;     sgDesign.Cells[0,2]:=rOptions_Date;
  sgLocalization.Cells[1,2]:=Language.Info[liDate];
  sgLocalization.Cells[0,3]:=rOptions_Comment;  sgDesign.Cells[0,3]:=rOptions_Comment;
  sgLocalization.Cells[1,3]:=Language.Info[liComment];
  sgResize(sgLocalization);

  //update sgRules, sgDictionary
  RulesChanged(self);
  UpdateDictionaryInfo;

  //BoardConfig
  aLabel:=lbBoardSize;
  if Canvas.TextWidth(lbDimension.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbDimension;
  if Canvas.TextWidth(lbPlane.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbPlane;
  seBoardSize.AnchorSide[akLeft].Control:=aLabel;
  rbDim2D.AnchorSide[akLeft].Control:=aLabel;
  tbActivePlane.AnchorSide[akLeft].Control:=aLabel;

  //RulesOptions
  aLabel:=lbJokerPenalty;
  if Canvas.TextWidth(lbPass.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbPass;
  if Canvas.TextWidth(lbGameEndBonus.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbGameEndBonus;
  if Canvas.TextWidth(lbLimitExchange.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbLimitExchange;
  if Canvas.TextWidth(lbScrabbleBonus.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbScrabbleBonus;
  seJokerPenalty.AnchorSide[akLeft].Control:=aLabel;
  seNumberOfPasses.AnchorSide[akLeft].Control:=aLabel;
  seGameEndBonus.AnchorSide[akLeft].Control:=aLabel;
  seLimitExchange.AnchorSide[akLeft].Control:=aLabel;
  seScrabbleBonus.AnchorSide[akLeft].Control:=aLabel;

  //Network
  aLabel:=lbProxyIP;
  if Canvas.TextWidth(lbProxyPort.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbProxyPort;
  if Canvas.TextWidth(lbProxyUser.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbProxyUser;
  if Canvas.TextWidth(lbProxyPass.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbProxyPass;
  edProxyIP.AnchorSide[akLeft].Control:=aLabel;
  edProxyPort.AnchorSide[akLeft].Control:=aLabel;
  edProxyUser.AnchorSide[akLeft].Control:=aLabel;
  edProxyPass.AnchorSide[akLeft].Control:=aLabel;

  //WordCheckMode
  aLabel:=lbChallengePeriod;
  if Canvas.TextWidth(lbChallengePenalty.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbChallengePenalty;
  if Canvas.TextWidth(lbChallengeBonus.Caption)>Canvas.TextWidth(aLabel.Caption) then
    aLabel:=lbChallengeBonus;
  seChallengePeriod.AnchorSide[akLeft].Control:=aLabel;
  seChallengePenalty.AnchorSide[akLeft].Control:=aLabel;
  seChallengeBonus.AnchorSide[akLeft].Control:=aLabel;

  //field names
  DefaultFieldName[ftNormal]:=rFieldNormal;
  DefaultFieldName[ftDoubleLetter]:=rFieldDoubleLetter;
  DefaultFieldName[ftTripleLetter]:=rFieldTripleLetter;
  DefaultFieldName[ftQuadLetter]:=rFieldQuadLetter;
  DefaultFieldName[ftDoubleWord]:=rFieldDoubleWord;
  DefaultFieldName[ftTripleWord]:=rFieldTripleWord;
  DefaultFieldName[ftQuadWord]:=rFieldQuadWord;
  DefaultFieldName[ftStart]:=rFieldStart;
  DefaultFieldName[ftLetter]:=rFieldPlacedLetter;
  DefaultFieldName[ftNewLetter]:=rFieldNewLetter;
  DefaultFieldName[ftMalusSingleLetter]:=rFieldSingleMalus;
  DefaultFieldName[ftMalusDoubleLetter]:=rFieldDoubleMalus;
  DefaultFieldName[ftMalusTripleLetter]:=rFieldTripleMalus;
  DefaultFieldName[ftMalusQuadLetter]:=rFieldQuadMalus;
  clBoardColorsGetColors(clBoardColors,clBoardColors.Items);

  //computer
  tbCompExchangeChange(self);

  //options
  i:=cbUpdates.ItemIndex;
  cbUpdates.Items.Clear;
  cbUpdates.Items.Add(rOptions_Update0);
  cbUpdates.Items.Add(rOptions_Update1);
  cbUpdates.Items.Add(rOptions_Update2);
  cbUpdates.Items.Add(rOptions_Update3);
  cbUpdates.Items.Add(rOptions_Update4);
  cbUpdates.ItemIndex:=i;
  cbUpdates.Text:=cbUpdates.Items[i];
  Application.ProcessMessages;
  if gsLoading in Scrabble.GameState then
    FSettingsChanged:=false;
end;

