// ---------- Board ------------
procedure TfmGameOptions.seBoardSizeChange(Sender: TObject);
begin
  if (seBoardSize.Value>1) then
  begin
    setlength(FFieldType,seBoardSize.Value,seBoardSize.Value,seBoardSize.Value);
    tbActivePlane.Min:=1;
    tbActivePlane.Max:=seBoardSize.Value;
    {$ifdef Darwin}cbBoard.OnChange:=nil;{$endif}
    cbBoard.ItemIndex:=cbBoard.Items.IndexOf(BoardSet);
    {$ifdef Darwin}cbBoard.OnChange:=@cbBoardChange;{$endif}
    RulesChanged(self);
    ResizeFieldTypeGrid;
  end;
  FSettingsChanged:=true;
end;

function TfmGameOptions.GetBoardSet: string;
var
  i,x,y,z : byte;
begin
  z:=seBoardSize.Value;
  y:=integer(rbDim2D.Checked);
  if (seBoardSize.Value=15) and (rbDim2D.Checked) then
    i:=1 else
  if (seBoardSize.Value=21) and (rbDim2D.Checked) then
    i:=2 else
  if (seBoardSize.Value=15) and (rbDim3D.Checked) then
    i:=3 else
  if (seBoardSize.Value=21) and (rbDim3D.Checked) then
    i:=4 else
    i:=0;
  for x:=0 to (seBoardSize.Value-1) do
   for y:=0 to (seBoardSize.Value-1) do
    for z:=0 to integer(rbDim3D.Checked)*(seBoardSize.Value-1) do
  begin
    if (seBoardSize.Value=15) and (rbDim2D.Checked) then
       i:=i*integer(FFieldType[x,y,z]=TFieldType(DefaultFieldTypeClassic[x,y])) else
    if (seBoardSize.Value=21) and (rbDim2D.Checked) then
       i:=i*integer(FFieldType[x,y,z]=TFieldType(DefaultFieldTypeSuperscrabble[x,y])) else
    if (seBoardSize.Value=15) and (rbDim3D.Checked) then
       i:=i*integer(FFieldType[x,y,z]=TFieldType(DefaultFieldType3D[x,y,z])) else
    if (seBoardSize.Value=21) and (rbDim3D.Checked) then
       i:=i*integer(FFieldType[x,y,z]=TFieldType(DefaultFieldType3DSS[x,y,z])) else
    i:=0;
  end;
  case i of
   0:Result:='special';
   1:Result:='Classic Scrabble';
   2:Result:='Superscrabble';
   3:Result:='3D Classic';
   4:Result:='3D Superscrabble';
  end;
end;

procedure TfmGameOptions.SetBoardSet(const aValue: string);
var
  x,y,z:integer;
begin
  case aValue of
   'Classic Scrabble':
     begin
       seBoardSize.Value:=15;
       rbDim2D.Checked:=true;
       {$ifdef Darwin}cbBoard.OnChange:=nil;{$endif}
       cbBoard.ItemIndex:=0;
       {$ifdef Darwin}cbBoard.OnChange:=@cbBoardChange;{$endif}
       setlength(FFieldType,seBoardSize.Value,seBoardSize.Value,seBoardSize.Value);
       for x:=0 to 14 do
        for y:=0 to 14 do
         FFieldType[x,y,0]:=TFieldType(DefaultFieldTypeClassic[x,y]);
     end;
   'Superscrabble':
     begin
       seBoardSize.Value:=21;
       rbDim2D.Checked:=true;
       {$ifdef Darwin}cbBoard.OnChange:=nil;{$endif}
       cbBoard.ItemIndex:=1;
       {$ifdef Darwin}cbBoard.OnChange:=@cbBoardChange;{$endif}
       setlength(FFieldType,seBoardSize.Value,seBoardSize.Value,seBoardSize.Value);
       for x:=0 to 20 do
        for y:=0 to 20 do
         FFieldType[x,y,0]:=TFieldType(DefaultFieldTypeSuperscrabble[x,y]);
     end;
   '3D Classic':
     begin
       seBoardSize.Value:=15;
       rbDim3D.Checked:=true;
       {$ifdef Darwin}cbBoard.OnChange:=nil;{$endif}
       cbBoard.ItemIndex:=2;
       {$ifdef Darwin}cbBoard.OnChange:=@cbBoardChange;{$endif}
       setlength(FFieldType,seBoardSize.Value,seBoardSize.Value,seBoardSize.Value);
       for x:=0 to 14 do
        for y:=0 to 14 do
         for z:=0 to 14 do
          FFieldType[x,y,z]:=TFieldType(DefaultFieldType3D[x,y,z]);
     end;
   '3D Superscrabble':
     begin
       seBoardSize.Value:=21;
       rbDim3D.Checked:=true;
       {$ifdef Darwin}cbBoard.OnChange:=nil;{$endif}
       cbBoard.ItemIndex:=3;
       {$ifdef Darwin}cbBoard.OnChange:=@cbBoardChange;{$endif}
       setlength(FFieldType,seBoardSize.Value,seBoardSize.Value,seBoardSize.Value);
       for x:=0 to 20 do
        for y:=0 to 20 do
         for z:=0 to 20 do
          FFieldType[x,y,z]:=TFieldType(DefaultFieldType3DSS[x,y,z]);
     end;
  end;
  imBoardPaint(self);
  RulesChanged(self); //update key/value label
end;

procedure TfmGameOptions.cbBoardChange(Sender: TObject);
begin
  BoardSet:=cbBoard.Text;
  dgFieldType.Repaint;
  RulesChanged(self); //update key/value info
end;

procedure TfmGameOptions.tbActivePlaneChange(Sender: TObject);
begin
  {$ifndef LCLGtk2}
  stActivePlaneValue.Caption:=inttostr(tbActivePlane.Position);
  {$endif}
  dgFieldType.Repaint;
end;

procedure TfmGameOptions.rbDimClick(Sender: TObject);
begin
  tbActivePlane.Visible:=rbDim3D.Checked;
  lbPlane.Visible:=rbDim3D.Checked;
  cbFixate.Enabled:=rbDim3D.Checked;
  ResizeFieldTypeGrid;

  cbRoman.Enabled:=rbDim3D.Checked;
  {$ifndef LCLGtk2}
  stActivePlaneValue.Visible:=rbDim3D.Checked;
  {$endif}
  if not cbFixate.Enabled then
    cbFixate.Checked:=false;
  if not tbActivePlane.Visible then
    tbActivePlane.Position:=1;
  if (Scrabble<>nil) and not (gsLoading in Scrabble.GameState) then
  begin
    seBoardSizeChange(self);            //setlength(FFieldType,,,)
    {$ifdef Darwin}cbBoard.OnChange:=nil;{$endif}
    cbBoard.ItemIndex:=cbBoard.Items.IndexOf(BoardSet);
    {$ifdef Darwin}cbBoard.OnChange:=@cbBoardChange;{$endif}
    RulesChanged(self);
  end;
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.dgFieldTypeDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (length(FFieldType)>aCol) and (length(FFieldType)>aRow) then
  with dgFieldType.Canvas do
  begin
    Brush.Color:=FieldColor[FFieldType[aCol,aRow,tbActivePlane.Position-1]];
    FillRect(aRect);
  end;
end;

procedure TfmGameOptions.tsBoardShow(Sender: TObject);
begin
  imBoardPaint(self);
end;

procedure TfmGameOptions.imBoardPaint(Sender: TObject);
const
  cSize=96;
var
  i,j,z:integer;
begin
  if (cbBoard.ItemIndex in [2,3]) then
  begin
    if imBoard.Tag<>cbBoard.ItemIndex then  //flicker under windows
    begin
      imBoard.Width:=cSize;
      imBoard.Height:=cSize;
      il96.GetBitmap(cbBoard.ItemIndex-2,imBoard.Picture.Bitmap);
    end;
  end else
  begin
    imBoard.Canvas.LockCanvas;
    try
      z:=round(cSize/seBoardSize.Value);
      imBoard.Width:=z*seBoardSize.Value;
      imBoard.Height:=z*seBoardSize.Value;
      for i:=0 to seBoardSize.Value-1 do
       for j:=0 to seBoardSize.Value-1 do
       if length(FFieldType)=seBoardSize.Value then
       begin
         imBoard.Canvas.Brush.Color:=FieldColor[FFieldType[i,j,tbActivePlane.Position-1]];
         imBoard.Canvas.FillRect(Bounds(i*z,j*z,(i+1)*z,(j+1)*z));
       end;
    finally
      imBoard.Canvas.UnLockCanvas;
    end;
  end;
  imBoard.Tag:=cbBoard.ItemIndex;
end;

procedure TfmGameOptions.kvDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aFlags:Cardinal;
  aObject:TValueListEditor;
begin
  aObject:=TValueListEditor(Sender);
  aObject.Canvas.Brush.Style:=bsSolid;
  {$ifdef WindowsXP}
  aObject.Canvas.Brush.Color:=clForm;
  {$endif}
  aObject.Canvas.FillRect(aRect);
  aObject.Canvas.Brush.Style:=bsClear;
  if aCol=0 then
  begin
    aObject.Canvas.Font.Style:=[];
    aObject.Canvas.Font.Color:=clDefault;
    aFlags:=DT_RIGHT;
    InflateRect(aRect,-2,0);
  end else
  begin
{    if (Sender=kvRules) or
       ((Sender=kvDictionary) and (aRow in [5,10])) then
    begin
      aObject.Canvas.Font.Style:=[fsUnderline];
      aObject.Canvas.Font.Color:=clBlue;
    end;
}    aFlags:=DT_LEFT or DT_MODIFYSTRING or DT_END_ELLIPSIS;
  end;
  DrawText(aObject.Canvas.Handle,PChar(aObject.Cells[aCol,aRow]),-1,aRect,aFlags);
end;

procedure TfmGameOptions.dgFieldTypeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dgFieldType.MouseToCell(x,y,FPoint.X,FPoint.Y);
  if Button=mbLeft then
    miFieldTypeClick(nil) else
    pmFieldType.Popup;
end;

procedure TfmGameOptions.dgOptionsNavigatorDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  x,y:integer;
  s:string;
begin
  with dgOptionsNavigator do
  begin
    Canvas.Brush.Color:=clDefault;
    Canvas.Brush.Style:=bsSolid;
    Canvas.FillRect(aRect);
    InflateRect(aRect,-2,-2);
    if (Row=aRow) and (dgOptionsNavigator.Tag=pcOptions.IndexOf(pcOptions.ActivePage)) then
    begin
      Canvas.Brush.Color:=clMenuHighlight;
      Canvas.FillRect(aRect);
    end;
    Canvas.Brush.Style:=bsClear;
    case aRow of
     0:s:=rOptions_NavigatorGame;
     1:s:=rOptions_NavigatorDic;
     2:s:=rOptions_NavigatorRules;
     3:s:=rOptions_NavigatorDesign;
     4:s:=rOptions_NavigatorLang;
    end;
    x:=round(DefaultColWidth/2-il48.Width/2);
    y:=aRect.Top+1;
    il48.Draw(Canvas,x,y,aRow);
    x:=round(DefaultColWidth/2-Canvas.TextWidth(s)/2);
    y:=aRect.Bottom-Canvas.TextHeight(s);
    Canvas.TextOut(x,y,s);
  end;
end;

procedure TfmGameOptions.dgOptionsNavigatorSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  case aRow of
   0:pcOptions.ActivePage:=pcOptions.Pages[pcOptions.IndexOf(tsGame)];
   1:pcOptions.ActivePage:=pcOptions.Pages[pcOptions.IndexOf(tsDictionary)];
   2:pcOptions.ActivePage:=pcOptions.Pages[pcOptions.IndexOf(tsRules)];
   3:pcOptions.ActivePage:=pcOptions.Pages[pcOptions.IndexOf(tsDesign)];
   4:pcOptions.ActivePage:=pcOptions.Pages[pcOptions.IndexOf(tsLocalization)];
  end;
  dgOptionsNavigator.Tag:=pcOptions.IndexOf(pcOptions.ActivePage);
end;

procedure TfmGameOptions.dgFieldTypeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dgFieldType.LeftCol:=0;
  dgFieldType.TopRow:=0;
end;

procedure TfmGameOptions.miFieldTypeClick(Sender: TObject);
var
  i:integer;
begin
  if Sender=nil then
    i:=(integer(FFieldType[FPoint.X,FPoint.Y,tbActivePlane.Position-1])+1) mod length(DefaultFieldColor) else
    i:=(Sender as TMenuItem).Tag;
  if TFieldType(i) in [ftLetter,ftNewLetter] then
    i:=integer(ftMalusSingleLetter);//do not predefine boards with letter and newletter
  FFieldType[FPoint.X,FPoint.Y,tbActivePlane.Position-1]:=TFieldType(i);
  cbBoard.ItemIndex:=cbBoard.Items.IndexOf(BoardSet);
  dgFieldType.Repaint;
  RulesChanged(self);
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.ResizeFieldTypeGrid;
var
  z,h,w:integer;
begin
  z:=seBoardSize.Value;
  if (z>1) and not (csLoading in ComponentState) then
  begin
    h:=gbBoardConfig.Height-dgFieldType.Top-8;
    w:=gbBoardConfig.Width-16;
    if w<h then
      w:=w-16 else
      w:=h-16;
    dgFieldType.ColCount:=z;
    dgFieldType.RowCount:=z;
    dgFieldType.DefaultColWidth:=round((w-z*dgFieldType.GridLineWidth)/z);
    dgFieldType.DefaultRowHeight:=dgFieldType.DefaultColWidth;
    dgFieldType.Width:=dgFieldType.DefaultColWidth*z+dgFieldType.GridLineWidth+1;
    {$if defined (Windows) or defined(LCLQt)}
    dgFieldType.Width:=dgFieldType.Width+4;
    {$endif};
    dgFieldType.Height:=dgFieldType.Width;
  end;
end;

procedure TfmGameOptions.tsBoardLayoutResize(Sender: TObject);
begin
  ResizeFieldTypeGrid;
end;

function TfmGameOptions.GetFieldTypeArray: TFieldTypeArray;
var
  x,y,z : integer;
begin
  setlength(Result,seBoardSize.Value,seBoardSize.Value,seBoardSize.Value);
  if length(FFieldType)=seBoardSize.Value then
  for x:=0 to seBoardSize.Value-1 do
   for y:=0 to seBoardSize.Value-1 do
    for z:=0 to seBoardSize.Value-1 do
     Result[x,y,z]:=FFieldType[x,y,z];
end;

procedure TfmGameOptions.SetFieldTypeArray(const AValue: TFieldTypeArray);
var
  x,y,z : integer;
begin
  setlength(FFieldType,seBoardSize.Value,seBoardSize.Value,seBoardSize.Value);
  for x:=0 to seBoardSize.Value-1 do
   for y:=0 to seBoardSize.Value-1 do
    for z:=0 to seBoardSize.Value-1 do
     FFieldType[x,y,z]:=aValue[x,y,z];
  cbBoard.ItemIndex:=cbBoard.Items.IndexOf(BoardSet);
end;

function TfmGameOptions.GetBoardString: string;
var
  x,y,z:integer;
begin
  Result:='';
  if (length(FFieldType)=seBoardSize.Value) then
  begin
    for z:=0 to integer(rbDim3D.Checked)*(seBoardSize.Value-1) do
     for x:=0 to (seBoardSize.Value-1) do
     begin
       for y:=0 to (seBoardSize.Value-1) do
         if integer(FFieldType[x,y,z])<10 then
           Result:=Result+inttostr(integer(FFieldType[x,y,z])) else
           Result:=Result+chr(integer(FFieldType[x,y,z])+55);

       Result:=Result+',';
     end; //for z,x
    delete(Result,length(Result),1);
  end;
end;

procedure TfmGameOptions.SetBoardString(const aValue: string);
var
  x,y,z,e,{%H-}i : integer;
begin
  seBoardSizeChange(nil); //setlength(FFieldType,,,)
  with TStringList.Create do
  try
    CommaText:=aValue;
    if (seBoardSize.Value-1)+seBoardSize.Value*integer(rbDim3D.Checked)*(seBoardSize.Value-1)=Count-1 then
    for z:=0 to integer(rbDim3D.Checked)*(seBoardSize.Value-1) do
     for x:=0 to (seBoardSize.Value-1) do
     begin
       if length(Strings[z*seBoardSize.Value+x])=seBoardSize.Value then
       for y:=0 to (seBoardSize.Value-1) do
       begin
         Val(Strings[z*seBoardSize.Value+x][y+1],i,e);  //legacy function
         if e=0 then
           FFieldType[x,y,z]:=TFieldType(i) else
         begin
           i:=ord(Strings[z*seBoardSize.Value+x][y+1]);
           if (i-55>=10) and (i-55<=integer(high(TFieldType))) then
             FFieldType[x,y,z]:=TFieldType(i-55);//A=10
         end;
       end;
     end;
  finally
    Free;
  end;
end;


