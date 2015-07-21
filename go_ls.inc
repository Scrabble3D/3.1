// -------- load / save options --------
procedure SaveItems(Section:string;aParent:TControl);
var
  i,j,z: integer;
  aSection:string;
begin
  aSection:=Section+'/'+aParent.Name+'/Value';
  if (aParent is TCheckBox) then
    Config.Write(aSection,(aParent as TCheckBox).Checked);
  if (aParent is TSpinEdit) then
    Config.Write(aSection,(aParent as TSpinEdit).Value);
  if (aParent is TEdit) then
    Config.Write(aSection,(aParent as TEdit).Text);
  if (aParent is TMaskEdit) then
    Config.Write(aSection,(aParent as TMaskEdit).Text);
  if (aParent is TTrackBar) then
    Config.Write(aSection,(aParent as TTrackBar).Position);
  if (aParent is TComboBox) then
    Config.Write(aSection,(aParent as TComboBox).ItemIndex);
  if (aParent is TListBox) then
    Config.Write(aSection,(aParent as TListBox).ItemIndex);
  if (aParent is TListView) and
     ((aParent as TListView).Selected<>nil) then
    Config.Write(aSection,(aParent as TListView).Selected.Caption);
  if (aParent is TRadioButton) and not (aParent.Parent is TRadioGroup) then
    Config.Write(aSection,(aParent as TRadioButton).Checked);
  if (aParent is TRadioGroup) then
    Config.Write(aSection,(aParent as TRadioGroup).ItemIndex);
  if (aParent is TColorButton) then
    Config.Write(aSection,(aParent as TColorButton).ButtonColor);
  if (aParent is TSpeedButton) then
    Config.Write(aSection,(aParent as TSpeedButton).Down);

  aSection:=Section+'/'+aParent.Name+'/Item:';

  if (aParent is TColorListBox) then
   with (aParent as TColorListBox) do
    for i:=0 to Items.Count-1 do
     Config.Write(aSection+inttostr(i)+'/Value',PtrInt(Items.Objects[i]));
//     Config.Write(aSection+inttostr(i)+'/Value',Colors[i]);

  if (aParent is TCheckGroup) then
   with (aParent as TCheckGroup) do
    for i:=0 to Items.Count-1 do
     Config.Write(aSection+inttostr(i)+'/Value',Checked[i]);

  if (aParent is TStringGrid) and
     not (aParent.Name='sgPreset') and    //read only information
     not (aParent.Name='sgDictionary') and
     not (aParent.Name='sgRules') and
     not (aParent.Name='sgDesign') and
     not (aParent.Name='sgLocalization') then
   with (aParent as TStringGrid),TStringList.Create do
   try
     for i:=0 to ColCount-1 do
     begin
       Clear;
       for j:=0 to RowCount-1 do
         Add(Cells[i,j]);
       Config.Write(aSection+inttostr(i)+'/Value',DelimitedText);
     end;
   finally
     Free;
   end;

  if aParent is TWinControl then
   with TWinControl(aParent) do
    for z:=0 to ControlCount-1 do
     SaveItems(Section,Controls[z]);
end;

procedure TfmGameOptions.SaveSettings(const aPreset:string);
var
  i: integer;
  aSection: string;
begin
  //Rules
  if aPreset<>'' then //don't save rules without names
  begin
    Config.Write('Game/Presets',cbPresets.Items.DelimitedText);

    aSection:='Game/'+aPreset; //must not start with numbers or contain blanks
    Config.Delete(aSection);

    SaveItems(aSection+'/Dictionary/FileName',tsDictionary);
    SaveItems(aSection+'/Dictionary/Categories',tsCategories);
    SaveItems(aSection+'/Dictionary/WordCheckMode',tsWordCheckMode);
    //tsRules: updated from rules values
    SaveItems(aSection+'/Rules/Time',tsTime);
    SaveItems(aSection+'/Rules/Letter',tsLetter);
    SaveItems(aSection+'/Rules/Options',tsRulesOptions);
    //tsBoard: updated from board values
    SaveItems(aSection+'/Board/Config',tsBoardConfig);
    with TStringList.Create do
    try
      DelimitedText:=GetBoardString;
      for i:=0 to Count-1 do
        Config.Write(aSection+'/Board/FieldType/Item:'+inttostr(i)+'/Value',Strings[i]);
    finally
      Free; //TStringList
    end;
  end;

  //Design
  aSection:='Design'; //must not start with numbers or contain blanks
  SaveItems(aSection,tsDesign);
  SaveItems(aSection+'/Font',tsDesignGeneral);
  SaveItems(aSection+'/BoardColor',tsBoardColor);
  SaveItems(aSection+'/PlayerColor',tsPlayerColor);
  SaveItems(aSection+'/View2D',ts2DView);
  SaveItems(aSection+'/Illumination',tsExtra);

  //Computer
  aSection:='Computer';
  SaveItems(aSection+'/Performance',tsComputer);
  SaveItems(aSection+'/Options',tsCompOpt);

  //Options
  aSection:='Options';
  SaveItems(aSection+'/Localization',tsLocalization);
  SaveItems(aSection+'/Messages',tsMessages);
  SaveItems(aSection+'/Network',tsNetwork);
  SaveItems(aSection+'/Modus',rgOptionsNavigator);

  Config.Flush;
  FSettingsChanged:=false;
end;

procedure TfmGameOptions.LoadItems(const Section:string;aParent:TControl;aConfig:TConfig=nil);
var
  i,j,z : integer;
  aSection:string;
  s: string;
  {$ifdef darwin}
  aNotification: TNotifyEvent;
  {$endif}
begin
  aSection:=Section+'/'+aParent.Name+'/Value';
  if aConfig=nil then
    aConfig:=Config;
  if (aParent is TCheckBox) then
    (aParent as TCheckBox).Checked:=aConfig.Read(aSection,(aParent as TCheckBox).Checked);
  if (aParent is TSpinEdit) then
    (aParent as TSpinEdit).Value:=aConfig.Read(aSection,(aParent as TSpinEdit).Value);
  if (aParent is TEdit) then
    (aParent as TEdit).Text:=aConfig.Read(aSection,(aParent as TEdit).Text);
  if (aParent is TMaskEdit) then
    (aParent as TMaskEdit).Text:=aConfig.Read(aSection,(aParent as TMaskEdit).Text);
  if (aParent is TTrackBar) then
    (aParent as TTrackBar).Position:=aConfig.Read(aSection,(aParent as TTrackBar).Position);
  if (aParent is TComboBox) then
  {$ifdef darwin}
  begin
    aNotification:=(aParent as TComboBox).OnChange;
    (aParent as TComboBox).OnChange:=nil;
  {$endif}
    (aParent as TComboBox).ItemIndex:=aConfig.Read(aSection,(aParent as TComboBox).ItemIndex);
  {$ifdef darwin}
    (aParent as TComboBox).OnChange:=aNotification;
  end;
  {$endif}
  if (aParent is TListBox) then
    (aParent as TListBox).ItemIndex:=aConfig.Read(aSection,(aParent as TListBox).ItemIndex);
  if (aParent is TListView) then
  begin
    if (aParent as TListView).Selected<>nil then
      s:=(aParent as TListView).Selected.Caption;
    s:=aConfig.Read(aSection,s);
    (aParent as TListView).Selected:=(aParent as TListView).FindCaption(0,s,true,true,true);
  end;
  if (aParent is TRadioButton) and not (aParent.Parent is TRadioGroup) then
    (aParent as TRadioButton).Checked:=aConfig.Read(aSection,(aParent as TRadioButton).Checked);
  if (aParent is TRadioGroup) then
    (aParent as TRadioGroup).ItemIndex:=aConfig.Read(aSection,(aParent as TRadioGroup).ItemIndex);
  if (aParent is TColorButton) then
    (aParent as TColorButton).ButtonColor:=aConfig.Read(aSection,(aParent as TColorButton).ButtonColor);
  if (aParent is TSpeedButton) then
    (aParent as TSpeedButton).Down:=aConfig.Read(aSection,(aParent as TSpeedButton).Down);

  aSection:=Section+'/'+aParent.Name+'/Item:';

  if (aParent is TCheckGroup) then
   with (aParent as TCheckGroup) do
    for i:=0 to Items.Count-1 do
     Checked[i]:=aConfig.Read(aSection+inttostr(i)+'/Value',Checked[i]);

  if (aParent is TStringGrid) then
   with (aParent as TStringGrid) do
    for i:=0 to ColCount-1 do
     for j:=0 to RowCount-1 do
      Cells[i,j]:=aConfig.Read(aSection+inttostr(i)+':'+inttostr(j)+'/Value',Cells[i,j]);

  if (aParent is TStringGrid) then
   with (aParent as TStringGrid),TStringList.Create do
   try
     for i:=0 to ColCount-1 do
     begin
       Clear;
       DelimitedText:=aConfig.Read(aSection+inttostr(i)+'/Value','');
       if (Count>0) then
       begin
         RowCount:=Count;
         for j:=0 to RowCount-1 do
           Cells[i,j]:=Strings[j];
       end;
     end;
   finally
     Free;
   end;

  if aParent is TWinControl then
   with TWinControl(aParent) do
    for z:=0 to ControlCount-1 do
     LoadItems(Section,Controls[z],aConfig);
end;

procedure TfmGameOptions.LoadSettings(const aPreset:string);
var
  aSection:string;
  i:integer;
  s:string;
begin
  //Rules
  if aPreset<>'' then //don't try to load rules without names
  begin
    aSection:='Game/'+aPreset; //must not start with numbers or contain blanks

    //tsRules
    //  cbRulesSet.ItemIndex:=cbRulesSet.Items.IndexOf(RulesSet); //tsRules -> updated from values
    LoadItems(aSection+'/Rules/Time',tsTime);
    rbTimeControlChange(self);  //enable controls
    LoadItems(aSection+'/Rules/Letter',tsLetter);
    LoadItems(aSection+'/Rules/Options',tsRulesOptions);

    //dictionary -> load after letters to avoid to get asked if letters should be overriden from dic
    LoadItems(aSection+'/Dictionary/FileName',tsDictionary);
    if lvDictionaries.ItemIndex>-1 then
      Dictionary.LoadFrom(Config.Path+lvDictionaries.Items[lvDictionaries.ItemIndex].SubItems[3]);
    LoadItems(aSection+'/Dictionary/Categories',tsCategories);
    LoadItems(aSection+'/Dictionary/WordCheckMode',tsWordCheckMode);
    WCMUpdate(self);  //enable wcm edit fields if mode is challenge

    //tsBoard
    LoadItems(aSection+'/Board/Config',tsBoardConfig);
    with TStringList.Create do
    try
      i:=0;
      repeat
        s:=Config.Read(aSection+'/Board/FieldType/Item:'+inttostr(i)+'/Value','');
        if s<>'' then
          Add(s);
        inc(i);
      until s='';
      BoardString:=DelimitedText;
    finally
      Free; //TStringList
    end;
    cbBoard.ItemIndex:=cbBoard.Items.IndexOf(BoardSet); //update label

    RulesChanged(nil);
  end;

  //Design
  aSection:='Design';
  LoadItems(aSection,tsDesign);
  //load design from file
  if (lvDesigns.ItemIndex>-1) and FileExistsUTF8(Config.Path+lvDesigns.Items[lvDesigns.ItemIndex].SubItems[3]) then
    LoadDesign(lvDesigns.Items[lvDesigns.ItemIndex].SubItems[3]);
  //override file settings
  LoadItems(aSection+'/Font',tsDesignGeneral);
  LoadItems(aSection+'/BoardColor',tsBoardColor);     //item's color is read from config in clBoardColorsGetColors
  LoadItems(aSection+'/PlayerColor',tsPlayerColor);
  {$ifndef LCLGtk2}
  stLuminanceValue.Caption:=inttostr(tbLuminance.Position)+'%'; //trackbar does not have units except with gtk2
  {$endif}
  ColorChanged(nil);                                  //updates ilFieldType
  LoadItems(aSection+'/View2D',ts2DView);
  LoadItems(aSection+'/Illumination',tsExtra);

  //Computer
  aSection:='Computer';
  LoadItems(aSection+'/Performance',tsComputer);
  LoadItems(aSection+'/Options',tsCompOpt);
  cbCanJokerExchangeChange(self);//enable controls
  cbCambioSeccoChange(self);
  cbAddSubstractChange(self);
  cbParallel.Enabled:=GetSystemThreadCount>1;
  if not cbParallel.Enabled then cbParallel.Checked:=false;

  //Options
  aSection:='Options';
  LoadItems(aSection+'/Localization',tsLocalization);
  if lvLocalizations.ItemIndex>-1 then
    Language.LangFile:=Config.Path+lvLocalizations.Items[lvLocalizations.ItemIndex].SubItems[3];
  LoadItems(aSection+'/Messages',tsMessages);
  sbSndClick(sbScrabble);//set icon
  sbSndClick(sbChat);
  sbSndClick(sbOwnMove);
  sbSndClick(sbGameResult);
  LoadItems(aSection+'/Network',tsNetwork);
  cbProxyChange(self);//enable controls
  edConfigDir.Text:=Config.Path; //override saved and maybe changed value
  LoadItems(aSection+'/Modus',rgOptionsNavigator);

  RulesChanged(self); //update key/value info
  pcOptions.ActivePage:=tsGame;
  FSettingsChanged:=false;
end;


