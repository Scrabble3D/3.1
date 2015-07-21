{ Dictionary

  v3.1.3; 2015-Mar-01
  Copyleft (c) GPLv3: 1996-2015 Heiko Tietze heiko_tietze@web.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit udictionary;

{.$define UseDAWG}

//dawg speeds up word generation by ~3x
//but word generation takes only 10% of total time to find valid moves
//dawg needs work for jokers

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls,
  FileUtil,        //FileExistsUTF8
  Forms,           //Screen.Cursors
  LazUTF8,         //UTF8
  Synacode,        //Base64Encode
  Dateutils,       //Millisecondsbetween
  ulanguage,       //l10n
  uversion,        //version2string
  utypes,          //TOnProgress, OnMessage
  lclintf          //CharLowerBuff
  {$ifdef UseDAWG}
  ,udawg
  {$endif}
  ;

type

  TItem=class
    Category : byte;
    Meaning  : string;
  end;

  TByteSet=set of byte;

  TDicInfoType=(itFileName,itAuthor,itVersion,itFileSize,itFileSizeString,
                itFileDate,itWordCount,itFileIncludeWordCount,itLicence,
                itComment,itCategoriesEnabled,itCategoriesDisabled,
                itLetterSetLetter,itLetterSetCount,itLetterSetValue);

  TDicInfo=record
    Version    : integer;       //information in file header
    Author     : string;
    Licence    : string;
    Comment    : string;
    Letters    : string;
    LetterCount: string;
    LetterValue: string;
  end;

  TFindResult=(frInclude,frExclude,frNone);

  TOnGetAvailableLetters=function:string of object;

  { TDictionary }

  TDictionary=class(TStringList)
    private
      FFileName   : string;   //store for saving file on change
      FDicInfo    : TDicInfo; //dictionary header

      FDebug      : boolean;  //collect all letter from dictionary and check for duplicates
      FChanged    : boolean;  //save on close
      FIsLoading  : boolean;
      FExcluded   : TByteSet; //category to exclude items from valid words
      FReplaceChar: TStringList; //list of char-digraph combinations like 1=LL, 2=RR
      FCategories : TStringList; //list of category names
      {$ifdef UseDAWG}
      FDawg       : TDawg;
      {$endif}
      FOnChange,FOnAfterDictionaryLoaded   : TNotifyEvent;
      FOnGetAvailableLetters: TOnGetAvailableLetters; //callback to get letters list from settings

      FLastSearch,FLastSearchResult: string; //remember last WordsByLetters() to speed up

      procedure SetCategory(const aIndex: longword; const aValue: string);
      function GetCategory(const aIndex: longword): string;
      procedure SetMeaning(const aIndex: longword; const aValue: string);
      function GetMeaning(const aIndex: longword): string;

      function GetInfo(const aIndex: TDicInfoType): variant;

      function XORCrypt(aData, aKey: string): string;
    private
      procedure {%H-}Clear; override;
      procedure Save;
      procedure LoadFromIni;
    protected
      function Performance:longword;
    public
      constructor Create;
      destructor Destroy;override;
      procedure LoadFrom(const aFileName:string);

      function Add(const aValue: string):integer; reintroduce; //does InsertObject(); returns index in list
      procedure Delete(const aValue: string);
      procedure Delete(const aIndex: longword); overload;

      function IsWordInDictionary(const aValue:string; var aIndex:integer; const cCaseSensitive:boolean=false): TFindResult;
      function WordsByLetters(const aChars: string): string;
      function ReplaceDigraphs(const aValue:string):string;

      property OnGetAvailableLetters: TOnGetAvailableLetters read FOnGetAvailableLetters write FOnGetAvailableLetters;
      property Excluded:TByteSet read FExcluded write FExcluded;
      property Categories: TStringList read FCategories;
      property Info[index:TDicInfoType]:variant read GetInfo;
      property Meaning[index:longword]:string read GetMeaning write SetMeaning;
      property Category[index:longword]:string read GetCategory write SetCategory;
      property IsDebug:boolean read FDebug write FDebug;
      property OnChange:TNotifyEvent read FOnChange write FOnChange;
      property OnAfterDictionaryLoaded:TNotifyEvent read FOnAfterDictionaryLoaded write FOnAfterDictionaryLoaded;
    end; //TDictionary

var
  Dictionary:TDictionary;

implementation

function TDictionary.GetInfo(const aIndex: TDicInfoType): variant;
 function FileSize2String(fs:extended):string;
 const
   SizeNames : array[0..4] of string = (' Byte',' kB',' MB',' GB',' TB');
 var
   i:byte;
 begin
   i:=0;
   while fs>1000.0 do
   begin
     fs:=fs/1024.0;
     inc(i);
   end;
   Result:=FloatToStrF(fs,ffGeneral,4,2)+SizeNames[i];
 end;
var
 i,j:integer;
 tmp:TItem;
 s:string;
begin
  Result:='';
  case aIndex of
    itFileSize:
      if FileExistsUTF8(FFileName) then
        Result:=FileSize(FFileName) else
        Result:=0;
    itFileSizeString:
      if FileExistsUTF8(FFileName) then
        Result:=FileSize2String(FileSize(FFileName));
    itFileName:
      if FileExistsUTF8(FFileName) then
        Result:=Trim(ExtractFileNameOnly(FFileName));
    itFileDate:
      if FileExistsUTF8(FFileName) then
        Result:=DateTimeToStr(FileDateToDateTime(FileAgeUTF8(FFileName)));
    itAuthor:
      Result:=FDicinfo.Author;
    itWordCount:
      Result:=self.Count;
    itVersion:
      Result:=VersionToString(FDicinfo.Version);
    itFileIncludeWordCount:
      begin
        j:=0;
        for i:=0 to self.Count-1 do
        begin
          if (self.Count>i) and (self.Objects[i] is TItem) then
          try
            tmp:=TItem(self.Objects[i]);
          except
            tmp:=nil
          end else
            tmp:=nil;
          if (tmp=nil) or not (tmp.Category in FExcluded) then
            inc(j);
        end;
        Result:=inttostr(j);
      end; //itFileIncludeWordCount
    itLicence:
      Result:=FDicInfo.Licence;
    itComment:
      Result:=FDicInfo.Comment;
    itCategoriesEnabled:
      begin
        s:='';
        for i:=0 to FCategories.Count-1 do
         if not (i in FExcluded) then
          s:=s+FCategories[i]+',';
        if length(s)>0 then
          System.Delete(s,length(s),1);
        Result:=s;
      end;//itCategoriesEnabled
    itCategoriesDisabled:
      begin
        s:='';
        for i:=0 to FCategories.Count-1 do
         if (i in FExcluded) then
          s:=s+FCategories[i]+',';
        if length(s)>0 then
          System.Delete(s,length(s),1);
        Result:=s;
      end;//itCategoriesDisabled
    itLetterSetLetter:
      Result:=FDicInfo.Letters;
    itLetterSetCount:
      Result:=FDicInfo.LetterCount;
    itLetterSetValue:
      Result:=FDicInfo.LetterValue;
  end; //case
end;

function TDictionary.GetMeaning(const aIndex: longword): string;
var
  tmp : TItem;
begin
  Result:='';
  if (aIndex<self.Count) then
  begin
    tmp:=TItem(self.Objects[aIndex]);
    if tmp<>nil then
      Result:=tmp.Meaning;
  end;
end;

procedure TDictionary.SetMeaning(const aIndex: longword; const aValue: string);
var
  tmp: TItem;
begin
  if (aIndex<self.Count) then
  begin
    tmp:=TItem(self.Objects[aIndex]);
    if tmp=nil then
      tmp:=TItem.Create;
    tmp.Meaning:=aValue;
    self.Objects[aIndex]:=tmp;
    FChanged:=true;
    if assigned(FOnChange) then
      FOnChange(self);
  end;
end;

function TDictionary.GetCategory(const aIndex: longword): string;
var
  tmp: TItem;
begin
  Result:='';
  if (aIndex<self.Count) then
  begin
    tmp:=TItem(self.Objects[aIndex]);
    if (tmp<>nil) then
    begin
      if FCategories.Count>tmp.Category then
        Result:=FCategories[tmp.Category]
    end else
      if FCategories.Count>0 then
        Result:=FCategories[0]
  end;
end;

procedure TDictionary.SetCategory(const aIndex: longword; const aValue: string);
var
  z : integer;
  tmp : TItem;
begin
  if (aIndex<self.Count) then
  begin
    tmp:=TItem(self.Objects[aIndex]);
    if tmp=nil then
      tmp:=TItem.Create;

    z:=FCategories.IndexOf(aValue);
    if z=-1 then
      z:=FCategories.Add(aValue);
    tmp.Category:=z;

    self.Objects[aIndex]:=tmp;
    FChanged:=true;
    if assigned(FOnChange) then
      FOnChange(self);
  end;
end;

function TDictionary.XORCrypt(aData, aKey: string): string;
var
  KeyLen,i: integer;
begin
  if aKey='' then
    Result:=aData else
  begin
    KeyLen:=length(aKey);
    for i:=1 to length(aData) do
      aData[i]:=Chr(Ord(aData[i]) xor Ord(aKey[(i mod KeyLen) + 1]));
    Result:=aData;
  end;
end;

function TDictionary.Performance: longword;
const
  aCount=100000;
var
  i,j,k,l:integer;
  z:double;
  s,aLetters:widestring;
begin
  aLetters:=UTF8Decode(FOnGetAvailableLetters());
  with TStringList.Create do
  try
    for i:=0 to aCount-1 do
    begin
      s:='';
      k:=random(15)+1;
      for j:=1 to k do
      begin
        l:=random(length(aLetters))+1;
        s:=s+aLetters[l];
      end;
      Add(s);
    end;
    z:=now;
    for i:=0 to Count-1 do
      if IsWordInDictionary(Strings[i],j)=frInclude then;
    Result:=MilliSecondsBetween(Now,z)
  finally
    Free;
  end;
end;

function TDictionary.ReplaceDigraphs(const aValue: string): string;
var
  i:integer;
  s:string;//widestring;
begin
  s:=aValue;//UTF8Decode(aValue);
  for i:=0 to FReplaceChar.Count-1 do
   if pos(FReplaceChar.Names[i],s)>0 then
    s:=StringReplace(s,FReplaceChar.Names[i],FReplaceChar.Values[FReplaceChar.Names[i]],[rfReplaceAll]);
  Result:=s;//UTF8Encode(s);
end;

constructor TDictionary.Create;
begin
  inherited Create;
  self.Sorted:=false;
  self.Duplicates:=dupIgnore;
  {$ifdef UseDAWG}
  FDawg:=TDawg.Create;
  {$endif}
  FCategories:=TStringList.Create;
  FReplaceChar:=TStringList.Create;

  FExcluded:=[];

  FDicInfo.Version:=0;
  FDicInfo.Author:='';
  FDicInfo.Comment:='';
  FDicInfo.Licence:='';
  FDicInfo.LetterCount:='';
  FDicInfo.Letters:='';
  FDicInfo.LetterValue:='';
end;

destructor TDictionary.Destroy;
begin
  if FChanged and (MessageDlg(rDictionary_Save,mtWarning,[mbYes,mbNo],0)=mrYes) then
     Save;
  self.Clear;//does freeandnil(Flist^[i].FObject)
  {$ifdef UseDAWG}
  FDawg.Free;
  {$endif}
  FCategories.Free;
  FReplaceChar.Free;
  inherited Destroy;
end;

procedure TDictionary.Clear;
var
  i:integer;
begin
  FCategories.Clear;
  FReplaceChar.Clear;
  FExcluded:=[];
  FDicInfo.Version:=0;
  FDicInfo.Author:='';
  FDicInfo.Comment:='';
  FDicInfo.Licence:='';
  FDicInfo.LetterCount:='';
  FDicInfo.Letters:='';
  FDicInfo.LetterValue:='';

  for i:=0 to Count-1 do
    TItem(self.Objects[i]).Free;
  inherited Clear;
end;

procedure TDictionary.LoadFrom(const aFileName: string);
var
  s:string;
begin
  if FileExistsUTF8(aFileName) and (FileSize(aFileName)>8) then
  begin
    FIsLoading:=true;

    with TFileStream.Create(aFileName,fmOpenRead) do
    try
      SetLength(s,8);
      Read(s[1],8)
    finally
      Free;
    end;

    if (s='[Header]') then
    begin
      if FChanged then
       case MessageDlg(rDictionary_Save,mtWarning,[mbYes,mbNo,mbCancel],0) of
        mrNo:;
        mrYes: Save;
        mrCancel: exit;
       end;//case
      Clear;
      FFileName:=aFileName;//used later
      {$ifdef UseDAWG}
      FDawg.Clear;
      {$endif}
      LoadFromIni;
    end else
      OnMessage(smError,'Dictionary''s file format not recognized');

    FIsLoading:=false;
    SetLength(s,0);
  end;//file exists
end;

procedure TDictionary.Save;
const
  Chars='0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  i: integer;
  tmp: TItem;
  aKey, s: string;
begin
  Randomize;
  aKey:='';
  {$define Encrypted}

  {$ifdef Encrypted}
  {$Note Dictionary encrypted}
  for i:=1 to 8 do
    aKey:=aKey+Chars[random(length(Chars))+1];
  {$else}
  {$Warning Dictionary not encrypted}
  aKey:='';
  {$endif}
  with TStringList.Create do
   try
     StrictDelimiter:=true;
     Add('[Header]');
     Add('Version='+inttostr(FDicInfo.Version));
     Add('Author='+FDicInfo.Author);
     Add('StandardCategory='+FCategories[0]);
     Add('Licence='+FDicInfo.Licence);
     Add('Comment='+FDicInfo.Comment);
     Add('Letters='+FDicInfo.Letters);
     Add('Values='+FDicInfo.LetterValue);
     Add('Counts='+FDicInfo.LetterCount);
     Add('Key='+aKey);
     Add('[Replace]');
     for i:=0 to FReplaceChar.Count-1 do
      Add(FReplaceChar.Names[i]+'='+FReplaceChar.ValueFromIndex[i]);

     Add('[Categories]');
     for i:=1 to FCategories.Count-1 do
       if (FCategories[i]<>'') then
        Add(inttostr(i)+'='+FCategories[i]);
     Add('[Words]');
     for i:=0 to self.Count-1 do
     begin
       if assigned(OnProgress) then
         OnProgress(self, round((i/self.Count)*100));
       tmp:=TItem(self.Objects[i]);
       if tmp=nil then s:=self.Strings[i] else
       begin
          s:=self.Strings[i]+'='+tmp.Meaning;
          if (tmp.Category>0) then s:=s+';'+IntToStr(tmp.Category);
       end;
       if (aKey<>'') then
         s:=EncodeBase64(XorCrypt(s,aKey));
       Add(s);
     end;
     SaveToFile(FFileName);
     FChanged:=false;
   finally
     Free;
     if assigned(OnProgress) then
       OnProgress(self,101);//progress>100 clears statusbar
   end;
end;

procedure TDictionary.LoadFromIni;
var
  aKey:string;
  aChars: string;

 procedure DoHeader(aLine:string);
 begin
   case copy(aLine,1,3) of
    'Ver':FDicInfo.Version:=strtoint(copy(aLine,length('Version')+2,length(aLine)));
    'Aut':FDicInfo.Author:=copy(aLine,length('Author')+2,length(aLine));
    'Lic':FDicInfo.Licence:=copy(aLine,length('Licence')+2,length(aLine));
    'Com':FDicInfo.Comment:=copy(aLine,length('Comment')+2,length(aLine));
    'Let':FDicInfo.Letters:=copy(aLine,length('Letters')+2,length(aLine));
    'Cou':FDicInfo.LetterCount:=copy(aLine,length('Counts')+2,length(aLine));
    'Val':FDicInfo.LetterValue:=copy(aLine,length('Values')+2,length(aLine));
    'Key':aKey:=copy(aLine,length('Key')+2,length(aLine));
    'Sta':FCategories.Add(copy(aLine,length('StandardCategory')+2,length(aLine)));
   end;
 end;
 procedure DoWords(aLine:string);
 var
   i,j,z: integer;
   aWord:string;
   tmp: TItem;
 begin
   //read meaning/category of particular string
   if (aKey<>'') then
     aLine:=XorCrypt(DecodeBase64(aLine), aKey);

   tmp:=nil;
   i:=pos('=',aLine);//utf8pos
   if i>0 then
   begin
     tmp:=TItem.Create;
     tmp.Meaning:='';
     tmp.Category:=0;
     aWord:=copy(aLine,1,i-1);//UTF8Copy
     j:=pos(';',aLine);//UTF8Pos
     if j>0 then
     begin
       tmp.Meaning:=copy(aLine,i+1,j-i-1);
       tmp.Category:=StrToIntDef(copy(aLine,j+1,length(aLine)),0);//UTF8Copy
     end else
       tmp.Meaning:=copy(aLine,i+1,length(aLine));
   end else
     aWord:=aLine;

   {$ifdef UseDAWG}
   FDawg.InsertString(aWord);
   {$endif}
   //add all letters to test (dupignore)
   if FDebug and assigned(OnMessage) then
    for z:=1 to UTF8Length(aWord) do
     if pos(UTF8Copy(aWord,z,1),aChars)=0 then
      aChars:=aChars+UTF8Copy(aWord,z,1);

   if IsWordInDictionary(aWord, z)=frNone then //list position of new word after z
     self.InsertObject(z,aWord,tmp) else
     if FDebug and assigned(OnMessage) then
       OnMessage(smDebug,'duplicate entry: '+aWord);//frNone
 end;

type
  TSection=(scNone,scHeader,scCategories,scReplace,scWords);
const
  cBufSize=65355;//The maximum size of the newly assigned buffer is 65355 bytes.
var
  z_all,z_pos : int64;
  aLine: string;
  f: textfile;
  aBuffer: array[0..cBufSize-1] of byte;
  aSection: TSection;
  {$ifdef UseDAWG}
  c: TDawgMapping;
  i: TDawgChar;
  {$endif}
begin
  if assigned(OnMessage) then
    OnMessage(smInformation,Language.Format(rDictionary_Loading,[ExtractFileName(FFileName)]));

  Screen.Cursor:=crHourglass;

  {$ifdef UseDAWG}
  if assigned(FDawg) then //not needed?
  begin
    for i:=low(c) to High(c) do
      c[i]:=i;
    CharUpperBuff(@c, sizeof(c));
    FDawg.SetMapping(c);
  end;
  {$endif}

  z_all:=FileSize(FFileName);
  z_pos:=0;

  AssignFile(f, UTF8ToSys(FFileName));
  SetTextBuf(f, aBuffer, cBufSize);
  Reset(f);

  aSection:=scNone;
  while not eof(f) do
  begin
    readln(f,aLine);

    //Progress
    inc(z_pos,length(aLine));
    if assigned(OnProgress) then
      OnProgress(self,round((z_pos/z_all)*100));

    if aLine='' then
      Continue;

    if aLine[1]='[' then
     case aLine of
      '[Header]':aSection:=scHeader;
      '[Categories]':aSection:=scCategories;
      '[Replace]':aSection:=scReplace;
      '[Words]':aSection:=scWords;
     end else
     case aSection of
      scHeader: DoHeader(aLine);
      scCategories: FCategories.Add(copy(aLine,pos('=',aLine)+1,length(aLine)));
      scReplace: FCategories.Add(aLine);
      scWords: DoWords(aLine);
     end;
  end;
  CloseFile(f);

  if FDebug and assigned(OnMessage) then
    OnMessage(smDebug,UTF8Encode(aChars));
  FChanged:=false;

  Screen.Cursor:=crDefault;

  if assigned(OnProgress) then
    OnProgress(self,101);
  if assigned(FOnChange) then
    FOnChange(self);
  if assigned(FOnAfterDictionaryLoaded) then
    FOnAfterDictionaryLoaded(self);
end;

function TDictionary.Add(const aValue: string): integer;
begin
  if (IsWordInDictionary(aValue,Result{%H-})=frNone) then
  begin
    self.InsertObject(Result,aValue,nil);
    FChanged:=true;
    if assigned(FOnChange) then
      FOnChange(self);
  end;
end;

procedure TDictionary.Delete(const aValue: string);
var
  i: integer;
begin
  if (IsWordInDictionary(aValue,i)<>frNone) then
  begin
    self.Delete(i);
    FChanged:=true;
    if assigned(FOnChange) then
      FOnChange(self);
  end;
end;

procedure TDictionary.Delete(const aIndex: longword);
begin
  if (aIndex<self.Count) then
    self.Delete(aIndex);
end;

function TDictionary.IsWordInDictionary(const aValue: string; var aIndex: integer;
  const cCaseSensitive: boolean): TFindResult;
var
  li, hi, p, z: integer;
  tmp: TItem;
begin
  li:=0;
  hi:=self.Count-1;
  Result:=frNone;
  while li<=hi do
  begin
    z:=(li+hi) shr 1;
    if cCaseSensitive then
      p:=UTF8CompareText(self.Strings[z],aValue) else
      p:=UTF8CompareStr(self.Strings[z],aValue);
    if p<0 then
      li:=z+1 else
    begin
      hi:=z-1;
      if p=0 then
      begin
        tmp:=TItem(self.Objects[z]);
        if (tmp<>nil) and (tmp.Category in FExcluded)
            then Result:=frExclude
            else Result:=frInclude;
        li:=z;
      end;
    end;
  end;
  aIndex:=li;
end;

{$ifdef UseDAWG}
function AddToResults(UserData: Pointer; Word: PChar; WordLength: Integer): Boolean; register;
begin
  TStringList(UserData).Add(Word);
end;
{$endif}

function TDictionary.WordsByLetters(const aChars: string): string;
{ Scrabble-like variation of letters
  ABC:
  A    B    C
  AB   BA   CA
  AC   BC   CB
  ABC  BAC  CAB
  ACB  BCA  CBA
  Result as comma delimited string
}
var
  {$ifdef UseDAWG}
  Found: TDawg;
  sl: TStringList;
  {$endif}
  i,j,z      : integer;        //i=index of letter,j=flag,z=index of available letters,index=word in dictionary
  index      : integer;
  LetterSet  : widestring;     //defined by game options
  Chars      : widestring;
  s          : widestring;     //word to be composed
  us         : string;
begin
  {$ifdef UseDAWG}
  if assigned(FDawg) then
  begin
    sl:=TStringList.Create;
    sl.Sorted:=true;
    sl.Duplicates:=dupIgnore;
    Found:=TDawg.Create;
    try
      Chars:=UTF8Decode(aChars);
      z:=0;
      i:=pos('?',Chars);
      while i>0 do
      begin
        system.delete(Chars,i,1);
        inc(z);
        i:=pos('?',Chars);
      end;

      if FDawg.SearchCombinatoric(UTF8Encode(Chars), Found) then
        Found.Enum(@AddToResults,sl);

      if assigned(FOnGetAvailableLetters) then
        LetterSet:=UTF8Decode(FOnGetAvailableLetters());
      { TODO 5 : DAWG with (many) jokers }
      if z>0 then
      for i:=1 to length(LetterSet) do
      begin
        s:=Chars+Letterset[i];
        if FDawg.SearchCombinatoric(UTF8Encode(s), Found) then
          Found.Enum(@AddToResults,sl);
      end;
      Result:=sl.CommaText;
    finally
      FreeAndNil(Found);
      sl.Free;
    end;
  end else
  {$endif}
  begin
    if (FLastSearch=aChars) then
      exit(FLastSearchResult);

    i:=1; s:=''; Result:='';

    Chars:=UTF8Decode(aChars);
    if assigned(FOnGetAvailableLetters) then
      LetterSet:=UTF8Decode(FOnGetAvailableLetters());
    if (Chars<>'') and (LetterSet<>'') then
    repeat
      repeat
        z:=Pos(WideChar(LetterSet[i]),Chars);
        if (z=0) then z:=Pos(WideChar('?'),Chars);                 // try to insert joker
        if (z>0) then
        begin
           s:=s+LetterSet[i];
           us:=UTF8Encode(s);
           if (IsWordInDictionary(us,index)=frInclude) then
             if Result='' then
               Result:=us else
               Result:=Result+','+us;
           if (index<self.Count) and (Pos(us,self.Strings[index])=1) then    //incomplete or complete word
           begin
              System.Delete(Chars,z,1);                            //exclude letter from next search
              i:=1;                                                //start from first letter
              j:=0;
           end else
           begin
              System.Delete(s,length(s),1);                       //clear the last letter in temp string
              j:=-1;
           end;
        end else j:=-1;
      until j=-1;
      if (i=Length(LetterSet)) and (Length(s)>0) then             //if last letter in the set and something was found
      begin
        repeat
          i:=Pos(WideChar(s[Length(s)]),LetterSet)+1;             //start next search with then successor in available
          System.Delete(s,Length(s),1);                           //clear the last letter in temp string
        until (i<=Length(LetterSet)) or (s='');
        Chars:=UTF8Decode(aChars);                                //reset the available letters and delete found chars
        for j:=1 to Length(s) do
        begin
           z:=Pos(WideChar(s[j]),Chars);
           if z=0 then z:=pos(WideChar('?'),Chars);
           System.Delete(Chars,z,1);
        end;
      end else inc(i);                                            //next char in set
    until (i>Length(LetterSet)) and (s='');

    FLastSearch:=aChars;
    FLastSearchResult:=Result;
  end;
end;

end.

