procedure TfmGameOptions.sgResize(Sender: TObject);
const
  cDist=2;
var
  aObject: TStringGrid;
  aRow,i,z: integer;
  aRect: TRect;
  s: string;
begin
  aObject:=TStringGrid(Sender);
  //col width
  aObject.AutoSizeColumn(0);
  if aObject.ClientWidth-aObject.ColWidths[0]>50 then
    aObject.ColWidths[1]:=aObject.ClientWidth-aObject.ColWidths[0] else
    aObject.ColWidths[1]:=50;//min width
  //individual row height
  z:=0;
  for aRow:=0 to aObject.RowCount-1 do
  begin
    aRect:=Bounds(0,0,aObject.ColWidths[1],24);
    s:=aObject.Cells[1,aRow];
    i:=Pos(cTS,s);
    if i>0 then
      s:=Copy(s,1,i-1);
    if s='' then s:=' ';//word wrap bug with empty lines
    DrawText(aObject.Canvas.Handle, PChar(s),-1,aRect, DT_LEFT or DT_TOP or DT_WORDBREAK or DT_CALCRECT);
    aObject.RowHeights[aRow]:=aRect.Bottom-aRect.Top+cDist;
    if aRow<aObject.RowCount-1 then
      inc(z,aRect.Bottom-aRect.Top+cDist);
  end;
  if z+aRect.Bottom-aRect.Top+cDist<aObject.ClientHeight then
    aObject.RowHeights[aObject.RowCount-1]:=aObject.ClientHeight-z;
end;

procedure TfmGameOptions.sgDrawCells(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  aFlags:Cardinal;
  aObject:TStringGrid;
  s: string;
  i: integer;
  p: TPoint;
begin
  aObject:=TStringGrid(Sender);
  aObject.Canvas.Brush.Style:=bsClear;
  {$ifdef WindowsXP}
  aObject.Canvas.Brush.Color:=clForm;
  {$endif}
  aObject.Canvas.FillRect(aRect);
//  aObject.Canvas.Brush.Style:=bsClear;
  aObject.Canvas.Font.Style:=[];
  aObject.Canvas.Font.Color:=clDefault;
  if aCol=0 then
  begin
    s:=aObject.Cells[0,aRow];
    aFlags:=DT_RIGHT or DT_TOP;
    InflateRect(aRect,-2,0);
  end else
  begin
    s:=aObject.Cells[1,aRow];
    //remove accelerators
    i:=Pos('&',s);
    if i>0 then
      Delete(s,i,1);//utf8delete
    //remove links
    i:=Pos(cTS,s);
    if i>0 then
    begin
      s:=Copy(s,1,i-1);
      aObject.Canvas.Font.Color:=clBlue;
      //underline on mouse over only
      GetCursorPos(p);
      p:=aObject.ScreenToClient(p);
      if PtInRect(aObject.CellRect(aCol,aRow),p) then
        aObject.Canvas.Font.Style:=[fsUnderline];
      InflateRect(aRect,0,-1); //horizontal aligned with non-underlined text left hand
    end;
    aFlags:=DT_LEFT or DT_TOP or DT_WORDBREAK;
  end;
  DrawText(aObject.Canvas.Handle,PChar(s),-1,aRect,aFlags);
end;

procedure TfmGameOptions.sgMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  aCol,aRow:integer;
  aObject: TStringGrid;
begin
  aObject:=TStringGrid(Sender);
  aObject.MouseToCell(x,y,aCol,aRow);
  if Pos(cTS,aObject.Cells[aCol,aRow])>0 then
    aObject.Cursor:=crHandPoint else
    aObject.Cursor:=crDefault;
  aObject.Invalidate;
end;

procedure TfmGameOptions.sgSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  aObject: TStringGrid;
  i,j: integer;
  s: string;
begin
  aObject:=TStringGrid(Sender);
  i:=Pos(cTS,aObject.Cells[aCol,aRow]);
  if i>0 then
  begin
    s:=aObject.Cells[aCol,aRow];
    s:=copy(s,i+1,length(s));
    for j:=0 to pcOptions.PageCount-1 do
    if pcOptions.Pages[j].Name=s then
    begin
      pcOptions.ActivePageIndex:=j;
      if tvOptionsNavigator.Items.Count>j then
        tvOptionsNavigator.Selected:=tvOptionsNavigator.Items[j]; //1:1 relation, unsafe!
      dgOptionsNavigator.Tag:=-1;
      exit;
    end;
  end;
end;


