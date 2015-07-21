// ---------- letter set ------------
function TfmGameOptions.GetLetterValue(aChar: widechar): byte;
var
  i:integer;
begin
  if aChar='?' then
    Result:=0 else
  for i:=1 to sgLetters.RowCount-1 do
   if UTF8Decode(sgLetters.Cells[0,i])=aChar then
   begin
     Result:=StrToIntDef(sgLetters.Cells[2,i],0);
     exit;
   end;
end;

procedure TfmGameOptions.sgLettersKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  UpdateLettersetInfo(self);
end;

procedure TfmGameOptions.UpdateLettersetInfo(Sender: TObject);
begin
  {$ifdef darwin}cbLetterSet.OnChange:=nil;{$endif}
  cbLetterSet.ItemIndex:=cbLetterSet.Items.IndexOf(LetterSet);
  {$ifdef darwin}cbLetterSet.OnChange:=@cbLetterSetChange;{$endif}
  stTotalLetterCount.Caption:=inttostr(NumberOfLetters);
  RulesChanged(self); //update key/value info
end;

procedure TfmGameOptions.miCopyClick(Sender: TObject);
var
  i:integer;
  s:string;
begin
  s:='';
  for i:=1 to sgLetters.RowCount-1 do
    s:=s+sgLetters.Cells[0,i]+#9+sgLetters.Cells[1,i]+#9+sgLetters.Cells[2,i]+#10;
  ClipBoard.AsText:=s;
end;

procedure TfmGameOptions.miPasteClick(Sender: TObject);
var
  i:integer;
begin
  with TStringList.Create do
  try
    Delimiter:=#9;
    DelimitedText:=ClipBoard.AsText;
    sgLetters.RowCount:=1;
    for i:=0 to Count-1 do
    begin
      if sgLetters.RowCount<2+i div 3 then sgLetters.RowCount:=sgLetters.RowCount+1;
      sgLetters.Cells[i mod 3,1+i div 3]:=Strings[i];
    end;
    cbLetterSet.ItemIndex:=cbLetterSet.Items.IndexOf(LetterSet);
    FSettingsChanged:=true;
  finally
    Free;
  end;
end;

procedure TfmGameOptions.seLettersetMultiplierChange(Sender: TObject);
var
  i,z:integer;
begin
  for i:=1 to sgLetters.RowCount-1 do
  begin
    z:=StrToIntDef(sgLetters.Cells[1,i],0);
    z:=z div seLettersetMultiplier.Tag;
    sgLetters.Cells[1,i]:=IntToStr(z*seLettersetMultiplier.Value);
  end;
  seNumberOfJokers.Value:=(seNumberOfJokers.Value div seLettersetMultiplier.Tag)*seLettersetMultiplier.Value;
  seLettersetMultiplier.Tag:=seLettersetMultiplier.Value;
  cbLetterSet.ItemIndex:=cbLetterSet.Items.IndexOf(LetterSet);
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.cbLetterSetChange(Sender: TObject);
begin
  LetterSet:=cbLetterSet.Text;
  stTotalLetterCount.Caption:=inttostr(NumberOfLetters);
  FSettingsChanged:=true;
end;

procedure TfmGameOptions.sgLettersResize(Sender: TObject);
var
  i:integer;
begin
  i:=sgLetters.ClientWidth div 3;
  sgLetters.Columns[0].Width:=i;
  sgLetters.Columns[1].Width:=i;
  sgLetters.Columns[2].Width:=sgLetters.ClientWidth-2*i;
end;

function TfmGameOptions.GetLetterSet: string;
var
  i:integer;
  lang:TDefaultsLanguage;
  s1,s2,s3:string;
begin
  //delete empty rows
  for i:=sgLetters.RowCount-1 downto 1 do
   if (sgLetters.Cells[0,i]='') and not sgLetters.IsCellSelected[0,i] and
      (sgLetters.Cells[1,i]='') and not sgLetters.IsCellSelected[1,i] and
      (sgLetters.Cells[2,i]='') and not sgLetters.IsCellSelected[2,i] then
    sgLetters.DeleteRow(i);

  //collect letterset to string for comparison
   s1:='';s2:='';s3:='';
   for i:=1 to sgLetters.RowCount-1 do
   begin
     s1:=s1+sgLetters.Cells[0,i]+',';
     s2:=s2+IntToStr(StrToIntDef(sgLetters.Cells[1,i],0) div seLettersetMultiplier.Value)+',';
     s3:=s3+IntToStr(StrToIntDef(sgLetters.Cells[2,i],0))+',';
   end;
   delete(s1,length(s1),1); delete(s2,length(s2),1); delete(s3,length(s3),1);

   //default is unknown/special
   Result:='';      //special

   //compare with default
   for lang:=low(TDefaultsLanguage) to high(TDefaultsLanguage) do
    with Defaults.Value[lang] do
     if (s1=Letters) and
        (s2=LetterCount) and
        (s3=LetterValue) and
        (NumberOfJokers=(seNumberOfJokers.Value div seLettersetMultiplier.Value)) and
        (((ReadingDirection=rdLeftToRight) and rbLeftToRight.Checked) or
         ((ReadingDirection=rdRightToLeft) and rbRightToLeft.Checked)) and
        (seNumberOfLetters.Value=NumberOfLetters) and
        (seNumberOfRandomLetters.Value=NumberOfRandoms) then
       Result:=English+' ('+Native+')';
end;

procedure TfmGameOptions.SetLetterSet(const aValue: string);
var
  i,j:integer;
begin
  with TStringList.Create do
  try
    for i:=0 to 2 do
    begin
      case i of
       0 : CommaText:=Defaults[aValue].Letters;
       1 : CommaText:=Defaults[aValue].LetterCount;
       2 : CommaText:=Defaults[aValue].LetterValue;
      end;
      sgLetters.RowCount:=Count+1;
      for j:=0 to Count-1 do
        sgLetters.Cells[i,j+1]:=Strings[j];
    end;
    seNumberOfJokers.Value:=Defaults[aValue].NumberOfJokers;
    case Defaults[aValue].ReadingDirection of
     rdLeftToRight:rbLeftToRight.Checked:=true;
     rdRightToLeft:rbRightToLeft.Checked:=true;
    end;
    seLettersetMultiplier.Value:=1;
    seLettersetMultiplier.Tag:=1;
    seNumberOfLetters.Value:=Defaults[aValue].NumberOfLetters;
    seNumberOfRandomLetters.Value:=Defaults[aValue].NumberOfRandoms;

    cbLetterSet.ItemIndex:=cbLetterSet.Items.IndexOf(aValue);
  finally
    Free;
  end;//Value<'special' (6)
end;

procedure TfmGameOptions.SetLettersetString(aIndex: TLettersetType; const aValue: string);
var
  i:integer;
begin
  with TStringList.Create do
  try
    StrictDelimiter:=true;
    DelimitedText:=aValue;
    sgLetters.RowCount:=Count+1;
    for i:=0 to Count-1 do
      sgLetters.Cells[integer(aIndex),i+1]:=Strings[i];
  finally
    Free;
  end;
  UpdateLettersetInfo(self);
end;

function TfmGameOptions.GetLettersetString(aIndex: TLettersetType): string;
var
  i:integer;
begin
  Result:='';
  for i:=1 to sgLetters.RowCount-1 do
    Result:=Result+sgLetters.Cells[integer(aIndex),i]+',';
  delete(Result,length(Result),1);
end;

