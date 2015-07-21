procedure TfmMain.tbDimensionClick(Sender: TObject);
begin
  with Scrabble.ActiveDimension do
    Axis:=TDimension((Sender as TMenuItem).Tag);
end;

procedure TfmMain.pmDimension3DPopup(Sender: TObject);
begin
  case Scrabble.ActiveDimension.Axis of
   dx:pmDimension3D.Items[2].Checked:=true;
   dy:pmDimension3D.Items[1].Checked:=true;
   dz:pmDimension3D.Items[0].Checked:=true;
  end;
end;

procedure TfmMain.tbCubeSettingClick(Sender: TObject);
begin
  if pnPosition.Visible then
  begin
    tbPosition.Down:=false;
    pnPosition.Visible:=false;
  end else
  if (Sender is TToolButton) then
  begin
   trPosition.Tag:=(Sender as TToolButton).Tag;
   case (Sender as TToolButton).Tag of
    0:begin
        pnPosition.Left:=tbPosition.Left+2;
        trPosition.Max:=Scrabble.BoardSize-1;
        trPosition.Position:=Scrabble.ActiveDimension.Position;
      end;
    1:begin
      end;
    2:begin
        pnPosition.Left:=tbBrightness.Left+2;
        trPosition.Max:=50;
        trPosition.Position:=ScrabbleCube.Transparency;
      end;
   end;
   pnPosition.Top:=41;
   pnPosition.Visible:=true;
   pnPosition.BringToFront;
  end;
end;

procedure TfmMain.trPositionChange(Sender: TObject);
var aDim:TActiveDimension;
begin
  case trPosition.Tag of
   0: begin
       aDim:=Scrabble.ActiveDimension;
       aDim.Position:=(Sender as TTrackBar).Position;
       Scrabble.ActiveDimension:=aDim;
      end;
   2:ScrabbleCube.Transparency:=(Sender as TTrackBar).Position;
  end;//case
end;

function TfmMain.DoGetFieldLetter(x, y, z: byte): TLetter;
begin
  Result:=nil;
  if (Scrabble.CurrentMove<>Scrabble.ActualMove) then
    Result:=Bruteforce.BoardLetter[x,y,z] else
    Result:=Scrabble.BoardLetter[x,y,z];

  if (gsPaused in Scrabble.GameState) and
     not (gsPoll in Scrabble.GameState) and
     not (gsDemo in Scrabble.GameState) and
     not (gsNextPlayer in Scrabble.GameState) then //do not hide during nextplayer
    Result:=nil;
  if (Result<>nil) and
     (Result.State=lsPlaced) and
     (Result.When>Scrabble.ActualMove) then
     Result:=nil;
end;

function TfmMain.DoGetFieldColor(x, y, z: byte; aLetter:TLetter; var a: single; var FieldType:byte): TColor;
var
  ft:TFieldType;
begin
  if aLetter<>nil then
  begin
    if (aLetter.State=lsPlaced) and
       (fmGameOptions.cbLatest.Checked) and
       (aLetter.When>=Scrabble.CurrentMove-Scrabble.NumberOfPlayers) and
       ((Scrabble.CurrentMove=Scrabble.ActualMove) or (aLetter.When<Scrabble.ActualMove)) then
    case aLetter.Who of
     0 : Result:=fmGameOptions.cbPlayer5.ButtonColor;
     1 : Result:=fmGameOptions.cbPlayer6.ButtonColor;
     2 : Result:=fmGameOptions.cbPlayer7.ButtonColor;
     3 : Result:=fmGameOptions.cbPlayer8.ButtonColor;
    end else
    if (aLetter.When=Scrabble.ActualMove) then
       Result:=fmGameOptions.FieldColor[ftNewLetter] else
       Result:=fmGameOptions.FieldColor[ftLetter];
    a:=1.0;
  end else
  begin
    ft:=Scrabble.FieldType[x,y,z];
    Result:=fmGameOptions.FieldColor[ft];
    if ft=ftNormal then a:=0.03 else a:=0.5;
    FieldType:=byte(ft);
  end;
end;

function TfmMain.DoGetHint(const x, y, z: byte): string;
var
  aLetter:TLetter;
  i,j: integer;
  s: string;
  sl: TStringList;
begin
  if Scrabble.CurrentMove<>Scrabble.ActualMove then
    aLetter:=Bruteforce.BoardLetter[x,y,z] else
    aLetter:=Scrabble.BoardLetter[x,y,z];
  if (aLetter<>nil) and (aLetter.State=lsPlaced) and (aLetter.When>Scrabble.ActualMove) then
    aLetter:=nil;
  if (aLetter<>nil) and
     ((aLetter.State=lsPlaced) or (Scrabble.CurrentMove<>Scrabble.ActualMove) or (Scrabble.WordCheckMode=wcAsk)) and
     ((Scrabble.WordCheckMode<>wcChallenge) or
      ((gsGameEnd in Scrabble.GameState) and not (gsRunning in Scrabble.GameState))
     ) then
  begin
    sl:=TStringList.Create;
    try
      if Scrabble.CurrentMove=Scrabble.ActualMove then
        sl.CommaText:=Scrabble.WordAtPos[x,y,z,(Scrabble.WordCheckMode=wcPoll)] else
        sl.CommaText:=BruteForce.WordAtPos[x,y,z];

      if (gsCLABBERS in Scrabble.GameState) then
      begin
        with TStringList.Create do
        try
          for i:=sl.Count-1 downto 0 do
          begin
            CommaText:=Dictionary.WordsByLetters(sl[i]);
            for j:=0 to Count-1 do
             if (UTF8length(Strings[j])=UTF8Length(sl[i])) then
              sl.Insert(i+1,'+ '+Strings[j]);
          end;
        finally
          Free;
        end;
      end;

      Result:='';
      for i:=0 to sl.Count-1 do
      if sl[i]<>'' then
      begin
        Result:=Result+Dictionary.ReplaceDigraphs(sl[i]);
        if sl[i][1]='+' then
          s:=Copy(sl[i],3,length(sl[i])) else
          s:=sl[i];
        if Dictionary.IsWordInDictionary(s,j)<>frNone then
        begin
          s:=Dictionary.Meaning[j];
          if (s<>'') then
            Result:=Result+' ('+s+')';
          s:=Dictionary.Category[j];
          if (s<>'') and (s<>Dictionary.Categories[0]) then
            Result:=Result+' ['+s+']';
        end else
          if (Scrabble.WordCheckMode=wcAsk) or
             (gsGameEnd in Scrabble.GameState) then
            Result:=Result+' ('+rMain_NotFound+')';
        Result:=Result+LineBreak;
      end;//sl[i]<>''
      Result:=TrimRight(Result);
    finally
      sl.Free;//TStringList
    end;
  end;//showing hint is allowed
end;

procedure TfmMain.DoDrawLetter(aLetter: TLetter; aCanvas: TCanvas);
const
  LeftShift=3;
var
  s:string;
begin
  if (aLetter<>nil) and
     (not (gsPaused in Scrabble.GameState) or
      (gsDemo in Scrabble.GameState) or
      (gsNextPlayer in Scrabble.GameState)) then //do not hide during nextplayer
  with aCanvas do
  begin
    if SquareSize<>aCanvas.Width then
      CalcFontWidth(aCanvas,aCanvas.ClipRect,LetterSize,NumberSize,SquareSize); //global func definied in upieces

   if aLetter.What<>ltJoker then
   begin
     //setup font and string
     s:=Dictionary.ReplaceDigraphs(UTF8Encode(widestring(aLetter.What)));
     aCanvas.Font.Name:=fmGameOptions.edFont.Text;
     aCanvas.Font.Size:=LetterSize;
     aCanvas.Brush.Style:=bsClear;
     aCanvas.Font.Color:=clBlack;
     //shaded letter
     if fmGameOptions.cbShaded.Checked or not fmGameOptions.cbPlayerColored.Checked then
        TextOut(ClipRect.Left+((ClipRect.Right-ClipRect.Left) div 2-TextWidth(s) div 2)-LeftShift+1,
                ClipRect.Top+((ClipRect.Bottom-ClipRect.Top) div 2-TextHeight(s) div 2)+1, s);
     //colored letter
     if fmGameOptions.cbPlayerColored.Checked then
     begin
       Font.Color:=fmGameOptions.PlayerColor[aLetter.Who];
       TextOut(ClipRect.Left+((ClipRect.Right-ClipRect.Left) div 2-TextWidth(s) div 2)-LeftShift,
               ClipRect.Top+((ClipRect.Bottom-ClipRect.Top) div 2-TextHeight(s) div 2), s);
     end;
   end;
   //random letter
   if aLetter.IsRandom then
      StretchDraw(ClipRect,fmGameOptions.imRandom.Picture.Bitmap);
   //letter value or joker icon
   if aLetter.IsJoker then
      StretchDraw(ClipRect,fmGameOptions.imJoker.Picture.Bitmap)
   else if aLetter.Value>0 then
   begin
     Font.Color:=clBlack;
     Font.Name:=fmGameOptions.edFont.Text;
     Font.Size:=NumberSize;
     s:=inttostr(aLetter.Value);
     aCanvas.TextOut(ClipRect.Right-TextWidth(s)-3, ClipRect.Bottom-TextHeight(s)-1, s);
   end;
  end;
end;


