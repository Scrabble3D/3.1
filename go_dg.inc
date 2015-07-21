// ---------- Design ------------

procedure TfmGameOptions.rg2DModeSelectionChanged(Sender: TObject);
var
  i:integer;
begin
  {$ifndef UseOpenGl}
  rg2DMode.ItemIndex:=1;
  rg2DMode.Enabled:=false;
  {$endif}
  for i:=0 to gb2DLight.ControlCount-1 do
    gb2DLight.Controls[i].Enabled:=rg2DMode.ItemIndex=0;
  for i:=0 to gb2DPerspective.ControlCount-1 do
    gb2DPerspective.Controls[i].Enabled:=rg2DMode.ItemIndex=0;
  VisualChange(self);
end;

procedure TfmGameOptions.btnFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Font.Name:=edFont.Text;
    if Execute then
    begin
      edFont.Text:=Font.Name;
      VisualChange(self);
    end;
  finally
    Free;
  end;
end;

procedure TfmGameOptions.ReloadRemoteFiles(Sender:TListView);
begin
  Sender.Clear;
  Updater.ItemsByIdentifier(TUpdateItemType(Sender.Tag), Sender.Items);
end;

procedure TfmGameOptions.LoadLocalization(const aFileName: string);
begin
  Language.LangFile:=Config.Path+aFileName;
  if assigned(FOnLanguageChanged) then
     FOnLanguageChanged(self);
end;

procedure TfmGameOptions.LoadDesign(const aFileName: string);
var
  sl:TStringList;
  uz:TUnzipper;
  cf:TConfig;
  ft:TFieldType;
begin
  sl:=TStringList.Create;
  try
    sl.Add('content.xml');
    uz:=TUnzipper.Create;
    with uz do
    try
      uz.FileName:=Config.Path+aFileName;
      uz.OutputPath:=GetTempDir;
      uz.UnzipFiles(sl);
      if FileExistsUTF8(Config.Path+aFileName) then
      begin
        cf:=TConfig.Create(nil);
        try
          cf.Filename:=GetTempDir+'/content.xml';
          sgDesign.Cells[0,0]:=rOptions_Filename; sgDesign.Cells[1,0]:=aFileName;
          sgDesign.Cells[0,1]:=rOptions_Author;   sgDesign.Cells[1,1]:=cf.GetValue('General/Author/Value','');
          sgDesign.Cells[0,2]:=rOptions_Date;     sgDesign.Cells[1,2]:=cf.GetValue('General/Date/Value','');
          sgDesign.Cells[0,3]:=rOptions_Comment;  sgDesign.Cells[1,3]:=cf.GetValue('General/Comment/Value','');
          sgResize(sgDesign);

          LoadItems('Design/Font',tsDesignGeneral,cf);
          //LoadItems('Design/BoardColor',tsBoardColor,cf);
          clBoardColors.Clear;
          for ft:=low(TFieldType) to high(TFieldType) do
           clBoardColors.Items.AddObject(DefaultFieldName[ft],TObject(PtrInt(cf.GetValue('Design/BoardColor/clBoardColors/Item:'+inttostr(integer(ft))+'/Value',DefaultFieldColor[ft]))));

          LoadItems('Design/PlayerColor',tsPlayerColor,cf);
          {$ifndef LCLGtk2}
          stLuminanceValue.Caption:=inttostr(tbLuminance.Position)+'%'; //trackbar does not have units except with gtk2
          {$endif}
          ColorChanged(nil);
          LoadItems('Design/View2D',ts2DView,cf);
          LoadItems('Design/Illumination',tsExtra,cf);

          VisualChange(self);
        finally
          cf.Free;
        end;
      end;//if FileExists
    finally
      DeleteFileUTF8(GetTempDir+'/content.xml');
      uz.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure TfmGameOptions.btnLocalizationClick(Sender: TObject);
var
  s:string;
begin
  if lvLocalizations.Selected<>nil then
  begin
    s:=lvLocalizations.Selected.SubItems[3];
    if FileExistsUTF8(Config.Path+s) then
      LoadLocalization(s) else
    if MessageDlg(Language.Format(rOptions_DownloadProceed,[lvLocalizations.Selected.Caption]),mtConfirmation,[mbYes,mbNo],0)=mrYes then
      Updater.DownloadFile(Updater.ItemByFileName(s));
  end;
end;

procedure TfmGameOptions.btnMovieProgramClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    {$ifdef Windows}
    Filter:='ffmpeg.exe';
    {$else}
    Filter:='ffmpeg';
    {$endif}
    FileName:=edMovieProgram.Text;
    if Execute then
      edMovieProgram.Text:=FileName;
  finally
    Free;
  end;
end;

procedure TfmGameOptions.clBoardColorsDblClick(Sender: TObject);
begin
  acChangeBoardcolor.Execute;
end;

procedure TfmGameOptions.clBoardColorsGetColors(Sender: TCustomColorListBox; Items: TStrings);
var
  ft:TFieldType;
  aColor:TColor;
  aSection:string;
begin
  clBoardColors.Clear;
  for ft:=low(TFieldType) to high(TFieldType) do
  begin
    aSection:='Design/BoardColor/'+Sender.Name+'/Item:'+inttostr(integer(ft))+'/Value';
    aColor:=Config.Read(aSection,DefaultFieldColor[ft]);
    clBoardColors.Items.AddObject(DefaultFieldName[ft],TObject(PtrInt(aColor)));
  end;
end;

procedure TfmGameOptions.cbPlayerColoredChange(Sender: TObject);
begin
  cbShaded.Enabled:=cbPlayerColored.Checked;
  if not cbShaded.Enabled then
    cbShaded.Checked:=false;
  VisualChange(self);
end;

procedure TfmGameOptions.ColorChanged(Sender: TObject);
var
  ft  : TFieldType;
  bmp : Graphics.TBitmap;
begin
  pmFieldType.Images:=ilFieldType;
  ilFieldType.Clear;
  bmp:=Graphics.TBitmap.Create;
  with bmp do
  try
    Width:=ilFieldType.Width;
    Height:=ilFieldType.Height;
    for ft:=low(TFieldType) to high(TFieldType) do
    begin
      Canvas.Brush.Color:=FieldColor[ft];
      Canvas.FillRect(Bounds(0,0,ilFieldType.Width,ilFieldType.Height));
      ilFieldType.Add(bmp,nil);
    end;
  finally
    Free;
  end;
end;

procedure TfmGameOptions.edFontButtonClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Font.Name:=edFont.Text;
    if Execute then
      edFont.Text:=Font.Name;
  finally
    Free;
  end;
end;

function TfmGameOptions.GetFieldColor(index: TFieldType): TColor;
begin
  Result:=clBoardColors.Colors[integer(index)];
end;

function TfmGameOptions.GetPlayerColor(index: byte): TColor;
begin
  case index of
   0 : Result:=cbPlayer1.ButtonColor;
   1 : Result:=cbPlayer2.ButtonColor;
   2 : Result:=cbPlayer3.ButtonColor;
   3 : Result:=cbPlayer4.ButtonColor;
   else Result:=clBlack;
  end;
end;

procedure TfmGameOptions.tbLuminanceChange(Sender: TObject);
begin
  {$ifndef LCLGtk2}
  stLuminanceValue.Caption:=inttostr(tbLuminance.Position)+'%';
  {$endif}
  VisualChange(self);
end;

