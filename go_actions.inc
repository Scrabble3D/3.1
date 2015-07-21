procedure TfmGameOptions.acLoadPresetExecute(Sender: TObject);
begin
  LoadSettings(cbPresets.Text);
  FConfigName:=cbPresets.Text;
  UpdatePresetInfo;
  Config.Write('General/LastPreset',cbPresets.Text);
  FSettingsChanged:=false;
end;

procedure TfmGameOptions.acSavePresetExecute(Sender: TObject);
begin
  if cbPresets.Text='' then
    cbPresets.Text:='Standard';
  //config section must not start with number
  if cbPresets.Text[1] in ['0'..'9'] then
    cbPresets.Text:='_'+cbPresets.Text;
  if cbPresets.Items.IndexOf(cbPresets.Text)=-1 then
    cbPresets.ItemIndex:=cbPresets.Items.Add(cbPresets.Text);
  SaveSettings(cbPresets.Text);
  FConfigName:=cbPresets.Text;
  Config.Write('General/LastPreset',cbPresets.Text);
end;

procedure TfmGameOptions.acLoadRulesExecute(Sender: TObject);
begin
  if cbRulesSet.ItemIndex>-1 then
  begin
    RulesSet:=cbRulesSet.Items[cbRulesSet.ItemIndex];
    LetterSet:=cbRulesSet.Items[cbRulesSet.ItemIndex];
    BoardSet:='Classic Scrabble';
  end;
end;

procedure TfmGameOptions.alConfigurationUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  acDeletePreset.Enabled:=cbPresets.ItemIndex>-1;
  acSavePreset.Enabled:=FSettingsChanged;
  acDeleteDictionary.Enabled:=(lvDictionaries.Selected<>nil) and
                        ((lvDictionaries.Selected.SubItems[1]<>'') or (lvDictionaries.Selected.SubItems[2]=''));//local<>'' or remote='' resp. non-repo file
  acDeleteLang.Enabled:=(lvLocalizations.Selected<>nil) and
                        ((lvLocalizations.Selected.SubItems[1]<>'') or (lvLocalizations.Selected.SubItems[2]=''));//local<>'' or remote='' resp. non-repo file
  acDeleteDesign.Enabled:=(lvDesigns.Selected<>nil) and
                        ((lvDesigns.Selected.SubItems[1]<>'') or (lvDesigns.Selected.SubItems[2]=''));//local<>'' or remote='' resp. non-repo file
  acChangeBoardcolor.Enabled:=clBoardColors.ItemIndex>-1;
end;

procedure TfmGameOptions.btnDesignLoadClick(Sender: TObject);
var
  s:string;
begin
  if lvDesigns.Selected<>nil then
  begin
    s:=lvDesigns.Selected.SubItems[3];
    if FileExistsUTF8(Config.Path+s) then
      LoadDesign(s) else
    if MessageDlg(Language.Format(rOptions_DownloadProceed,[lvDesigns.Selected.Caption]),mtConfirmation,[mbYes,mbNo],0)=mrYes then
      Updater.DownloadFile(Updater.ItemByFileName(s));
  end;
end;

procedure TfmGameOptions.acDeletePresetExecute(Sender: TObject);
begin
  if MessageDlg(Language.Format(rOptions_DeletePreset,[cbPresets.Text]),mtConfirmation,[mbYes,mbNo],0)=mrYes then
  begin
    Config.Delete('Game/'+cbPresets.Text);
    cbPresets.Items.Delete(cbPresets.ItemIndex);
    Config.Write('Game/Presets',cbPresets.Items.DelimitedText);
    Config.Flush;
    if cbPresets.Items.Count>0 then
    begin
      cbPresets.ItemIndex:=0;
      acLoadPreset.Execute;
    end;
  end;
end;

procedure TfmGameOptions.acDeleteLangExecute(Sender: TObject);
begin
  if MessageDlg(Language.Format(rOptions_DeleteLang,[lvLocalizations.Selected.Caption]),mtConfirmation,[mbYes,mbNo],0)=mrYes then
  begin
    DeleteFileUTF8(Config.Path+lvLocalizations.Selected.SubItems[3]);
    ReloadRemoteFiles(lvLocalizations);
  end;
end;

procedure TfmGameOptions.acDeleteDesignExecute(Sender: TObject);
begin
  if MessageDlg(Language.Format(rOptions_DeleteDesign,[lvDesigns.Selected.Caption]),mtConfirmation,[mbYes,mbNo],0)=mrYes then
  begin
    DeleteFileUTF8(Config.Path+lvDesigns.Selected.SubItems[3]);
    ReloadRemoteFiles(lvDesigns);
  end;
end;

procedure TfmGameOptions.acDeleteDictionaryExecute(Sender: TObject);
begin
  if MessageDlg(Language.Format(rOptions_DeleteDictionary,[lvDictionaries.Selected.Caption]),mtConfirmation,[mbYes,mbNo],0)=mrYes then
  begin
    DeleteFileUTF8(Config.Path+lvDictionaries.Selected.SubItems[3]);
    ReloadRemoteFiles(lvDictionaries);
  end;
end;

procedure TfmGameOptions.acChangeBoardcolorExecute(Sender: TObject);
const
  cColor:array[0..1] of TColor=(clWhite,clBlack);
var
  i:integer;
begin
  with TColorDialog.Create(nil) do
  try
    for i:=0 to integer(high(TFieldType)) do
      CustomColors[i]:='Color'+chr(ord('A')+i)+'='+IntToHex(DefaultFieldColor[TFieldType(i)],6);
    for i:=integer(high(TFieldType)) to StandardColorsCount+ExtendedColorCount-1 do
      CustomColors[i]:='Color'+chr(ord('A')+i)+'='+IntToHex(cColor[i mod 2],6);
    Color:=clBoardColors.Selected;
    Title:=rSelectColor+' '+clBoardColors.Items[clBoardColors.ItemIndex];
    if Execute then
    begin
      clBoardColors.Items.Objects[clBoardColors.ItemIndex]:=TObject(PtrInt(Color));
      clBoardColors.Invalidate;   //repaint @W7
      ColorChanged(nil);          //updates ilFieldType
      VisualChange(self);
    end;
  finally
    Free;
  end;
end;

procedure TfmGameOptions.acLoadDictionaryExecute(Sender: TObject);
var
  s:string;
begin
  if (lvDictionaries.Selected=nil) or (lvDictionaries.Selected.SubItems[3]='') then
    exit;
  s:=lvDictionaries.Selected.SubItems[3];
  //(down-)load dic file
  if FileExistsUTF8(Config.Path+s) then
    Dictionary.LoadFrom(Config.Path+s) else
  if MessageDlg(Language.Format(rOptions_DownloadProceed,[lvDictionaries.Selected.Caption]),mtConfirmation,[mbYes,mbNo],0)=mrYes then
    Updater.DownloadFile(Updater.ItemByFileName(s));
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.cbPresetsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FSettingsChanged:=true;
  if Key=13 then
    acSavePreset.Execute;
end;

procedure TfmGameOptions.cbPresetsSelect(Sender: TObject);
begin
  if FSettingsChanged and
     (cbPresets.Text<>FConfigName) and
     (MessageDlg(rOptions_ChangedWarn,mtConfirmation,mbYesNo,0)=mrYes) then
    cbPresets.Text:=FConfigName else
    acLoadPreset.Execute;
end;

procedure TfmGameOptions.cbRulesSetChange(Sender: TObject);
begin
  acLoadRules.Execute;
end;

