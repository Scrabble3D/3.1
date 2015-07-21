// ---------- Dictionary ------------
procedure TfmGameOptions.DoAfterDownload(const aItem: TUpdateItem);
var
  s:string;
  i:integer;
begin
  if aItem<>nil then
  case aItem.ItemType of
   itDictionary:
    begin
      if lvDictionaries.Selected<>nil then
        s:=lvDictionaries.Selected.Caption else //store selection
        s:='';
      ReloadRemoteFiles(lvDictionaries); //rebuild list
      if s<>'' then
        i:=lvDictionaries.FindCaption(0,s,true,true,true).Index else //find selection
        i:=-1;
      if (i>-1) and (i<lvDictionaries.Items.Count) then
      begin
        lvDictionaries.ItemIndex:=i;   //restore selection
        Dictionary.LoadFrom(Config.Path+lvDictionaries.Selected.SubItems[3]); //load
      end;
    end;
   itLocalization:
    begin
      if lvLocalizations.Selected<>nil then
        s:=lvLocalizations.Selected.Caption else
        s:='';
      ReloadRemoteFiles(lvLocalizations);
      if s<>'' then
        i:=lvLocalizations.FindCaption(0,s,true,true,true).Index else
        i:=-1;
      if (i>-1) and (i<lvLocalizations.Items.Count) then
      begin
        lvLocalizations.ItemIndex:=i;
        LoadLocalization(lvLocalizations.Selected.SubItems[3]);
      end;
    end;
   itDesign:
    begin
      if lvDesigns.Selected<>nil then
        s:=lvDesigns.Selected.Caption else
        s:='';
      ReloadRemoteFiles(lvDesigns);
      if s<>'' then
        i:=lvDesigns.FindCaption(0,s,true,true,true).Index else
        i:=-1;
      if (i>-1) and (i<lvDesigns.Items.Count) then
      begin
        lvDesigns.ItemIndex:=i;
        LoadDesign(lvDesigns.Selected.SubItems[3]);
      end;
    end;
  end //case
  else //nil = version.conf
  begin
    ReloadRemoteFiles(lvDictionaries);
    ReloadRemoteFiles(lvLocalizations);
    ReloadRemoteFiles(lvDesigns);
  end;
end;

procedure TfmGameOptions.LoadDictionary(const aFileName: string);
var
  i:integer;
begin
  if FileExistsUTF8(Config.Path+aFileName) then
  begin
    Dictionary.LoadFrom(Config.Path+aFileName);

    //selection
    for i:=0 to lvDictionaries.Items.Count-1 do
     if lvDictionaries.Items[i].SubItems[3]=aFileName then
      lvDictionaries.ItemIndex:=i;
  end;
end;

procedure TfmGameOptions.DoAfterDictionaryLoaded(Sender: TObject);
var
  i,j:integer;
  tmp:TByteSet;
  s:string;
begin
  //adjust letter set?
  if (Dictionary.Info[itLetterSetLetter]<>'') and
     (Dictionary.Info[itLetterSetCount]<>'') and
     (Dictionary.Info[itLetterSetValue]<>'') and
    ((LettersetString[lsLetter]<>Dictionary.Info[itLetterSetLetter]) or
      (LettersetString[lsCount]<>Dictionary.Info[itLetterSetCount]) or
      (LettersetString[lsValue]<>Dictionary.Info[itLetterSetValue])) and
     (MessageDlg(rMain_UpdateLetterset,mtConfirmation,[mbYes,mbNo],0)=mrYes) then
  begin
    LettersetString[lsLetter]:=Dictionary.Info[itLetterSetLetter];
    LettersetString[lsCount]:=Dictionary.Info[itLetterSetCount];
    LettersetString[lsValue]:=Dictionary.Info[itLetterSetValue];
    FSettingsChanged:=true;
  end;

  //default categories
  if lvDictionaries.Selected<>nil then
  begin
    with TStringList.Create do
    try
      s:=lvDictionaries.Selected.Caption+' ('+lvDictionaries.Selected.SubItems[0]+')';
      DelimitedText:=Defaults[s].ExcludedCat;
      tmp:=[];
      for i:=0 to Count-1 do
      begin
        j:=StrToIntDef(Strings[i],0);
        if j<>0 then include(tmp,j);
      end;
      Dictionary.Excluded:=tmp;
    finally
      Free;
    end;
    for i:=0 to gbCategories.Items.Count-1 do
     gbCategories.Checked[i]:=not (i in Dictionary.Excluded);
  end;

  //apply stored cat
  if cbPresets.Text<>'' then
  begin
    s:='Game/'+cbPresets.Text+'/Dictionary/Categories/gbCategories/Item:';
    tmp:=[];
    with gbCategories do
     for i:=1 to Items.Count-1 do
     begin
       Checked[i]:=Config.Read(s+inttostr(i)+'/Value',Checked[i]);
       if not Checked[i] then include(tmp,i);
     end;
    Dictionary.Excluded:=tmp;
    UpdatePresetInfo;
  end;

  //adjust row height
  sgResize(sgDictionary);
end;

procedure TfmGameOptions.gbCategoriesItemClick(Sender: TObject; Index: integer);
var
  tmp:TByteSet;
begin
  tmp:=Dictionary.Excluded;
  if not gbCategories.Checked[index] then
    include(tmp,index) else
    exclude(tmp,index);
  Dictionary.Excluded:=tmp;
  UpdateDictionaryInfo;
  UpdatePresetInfo;
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.UpdateDictionaryInfo;
var
  i:integer;
  s:string;
  UpdateCategories:boolean;
begin
  //cat info
  if assigned(Dictionary.Categories) then
    UpdateCategories:=(gbCategories.Items.Count<>Dictionary.Categories.Count) else //different number of categories
    exit;
  if not UpdateCategories then
   for i:=0 to gbCategories.Items.Count-1 do
    if (gbCategories.Items[i]<>Dictionary.Categories[i]) then //different category names
     UpdateCategories:=true;

  if UpdateCategories then
  begin
    gbCategories.Items.Clear;
    gbCategories.Items.AddStrings(Dictionary.Categories);
    gbCategories.ChildSizing.EnlargeVertical:=crsAnchorAligning;
    if Dictionary.Categories.Count>0 then gbCategories.CheckEnabled[0]:=false;
    for i:=0 to gbCategories.Items.Count-1 do
      gbCategories.Checked[i]:=not (i in Dictionary.Excluded);
    //preset info
    UpdatePresetInfo;
  end;

  //dic info
  for i:=0 to 4 do
  begin
    case i of
     0:sgDictionary.Cells[0,i]:=rOptions_DictionaryID0;
     1:sgDictionary.Cells[0,i]:=rOptions_DictionaryID1;
     2:sgDictionary.Cells[0,i]:=rOptions_DictionaryID2;
     3:sgDictionary.Cells[0,i]:=rOptions_DictionaryID3;
     4:sgDictionary.Cells[0,i]:=rOptions_DictionaryID4;
    end;
    sgDictionary.Cells[1,i]:=Dictionary.Info[TDicInfoType(i)];
  end;
  sgDictionary.Cells[0,5]:=rOptions_DictionaryID9;
  s:=Dictionary.Info[itCategoriesDisabled];
  if s<>'' then
    sgDictionary.Cells[1,5]:=Dictionary.Info[itCategoriesEnabled]+' -('+s+')'+cTS+'tsCategories' else
    sgDictionary.Cells[1,5]:=Dictionary.Info[itCategoriesEnabled]+cTS+'tsCategories';

  for i:=6 to 9 do
  begin
    case i of
     6:sgDictionary.Cells[0,i]:=rOptions_DictionaryID5;
     7:sgDictionary.Cells[0,i]:=rOptions_DictionaryID6;
     8:sgDictionary.Cells[0,i]:=rOptions_DictionaryID7;
     9:sgDictionary.Cells[0,i]:=rOptions_DictionaryID8;
    end;
    sgDictionary.Cells[1,i]:=Dictionary.Info[TDicInfoType(i-1)];
  end;

  sgDictionary.Cells[0,10]:=gbWordCheckMode.Caption+':';
  if rbWCMChallenge.Checked then
    sgDictionary.Cells[1,10]:=rbWCMChallenge.Caption+cTS+'tsWordCheckMode' else
  if rbWCMTakeback.Checked then
    sgDictionary.Cells[1,10]:=rbWCMTakeback.Caption+cTS+'tsWordCheckMode' else
  if rbWCMPoll.Checked then
    sgDictionary.Cells[1,10]:=rbWCMPoll.Caption+cTS+'tsWordCheckMode';

  //adjust row height
  if UpdateCategories then
    sgResize(sgDictionary);
end;

procedure TfmGameOptions.UpdatePresetInfo;
var
  aRow:integer;
begin
  sgPreset.RowCount:=high(StandardOptions)+1;
  for aRow:=0 to high(StandardOptions) do
  with ConfigString(StandardOptions[aRow]) do
  begin
    sgPreset.Cells[0,aRow]:=Key;
    sgPreset.Cells[1,aRow]:=Value;
  end;
  sgPreset.AutoSizeColumn(0);
  sgPreset.ColWidths[1]:=sgPreset.ClientWidth-sgPreset.ColWidths[0];
end;


