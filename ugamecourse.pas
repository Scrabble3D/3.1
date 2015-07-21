{ Game course

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

unit ugamecourse;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Controls, Grids, Themes, Graphics, Menus, Inifiles, Dialogs, Forms,
  LCLIntf, //cursorpos
  LCLType, //DT_*
  LazUTF8, //UTF8*
  LResources, //loadfrom
  Math, DateUtils, //Compare*
  ulanguage,
  utypes;

type

  TGameCourseMove=class
    PlacedWord       : string;
    ConnectedWords   : string;
    ExchangedLetters : string;
    CambioSecco      : string;//letters before cs was used
    RackLetters      : THistoryArray;
    Dimension        : byte;//0=dx...
    Position         : byte;//0..(14)
    MoveNumber       : word;
    PlayerNumber     : byte;
    Value            : word;
    Penalty          : array[0..3] of integer;
    Best             : integer;  //-1 = not set
    TimeUsed         : longword;
    IsScrabble       : boolean;
  end;

  TOnSetCurrentMove=function(aIndex:word):word of object;
  TOnReplaceDigraphs=function(const aValue:string):string of object;
  TOnIsKnownWord=function(const aValue: string; var aNotFound: string; DoAsk:boolean): Boolean of object;

  { TGameCourse }

  TGameCourse=class
    private
      FBiDiMode      : TBiDiMode;
      FHistory       : TList;
      FMaskExchanged : boolean;
      FStrikeout     : boolean;
      FStringGrid    : TStringGrid;
      FOnIsKnownWord : TOnIsKnownWord;
      FPlayerColor   : array[0..3] of TColor;
      FShaded, FColored : boolean;
      FIsScrabble    : TBitmap;
      FActualMove    : integer;
      FPopupMenu     : TPopupMenu;
      FOnReplaceDigraphs : TOnReplaceDigraphs;
      FOnSetCurrentMove: TOnSetCurrentMove;
      procedure DoChangeFont(Sender: TObject);
      procedure DoDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
      procedure DoHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
      procedure DoResize(Sender: TObject);
      procedure DoSelection(Sender: TObject; aCol, aRow: Integer);
      procedure DoClearBest(Sender: TObject);
      function GetAllBestmoveValues: string;
      function GetHistoryCount: integer;
      function GetHistory(aMoveNumber: integer): TGameCourseMove;
      function GetPlayerColor(aPlayerNumber: byte): TColor;
      procedure SetAllBestmoveValues(AValue: string);
      procedure SetBest(aMoveNumber: word; const aValue: integer);
      procedure SetColored(const aValue: boolean);
      procedure SetMaskExchanged(const aValue: boolean);
      procedure SetPlayerColor(aPlayerNumber: byte; const aValue: TColor);
      procedure SetShaded(const AValue: boolean);
    public
      procedure DoAddMove(const aPlacedWord,aConnectedWords,aExchangedLetters,aCambioSecco:string;
                          const aRackLetters:THistoryArray;
                          const aPlayer:byte;
                          const aMoveNumber,aValue:word;
                          const aTimeUsed:Longword;
                          const aIsScrabble:boolean;
                          const aDimension,aPosition:byte);
      function DoGetRackLetterIndex(aIndex:word;aRackLetter:byte):integer;
    public
      constructor Create(AOwner: TComponent);
      destructor Destroy;override;
      procedure UpdateLanguage;
      procedure AddPenalty(aPlayer:byte; aMoveNumber:word; aValue:integer);
      procedure Clear;
      procedure LoadFrom(const aFileName:string);
      procedure SaveTo(const aFileName:string);
      procedure Takeback;
      procedure AdjustColWidth;

      property OnSetCurrentMove:TOnSetCurrentMove read FOnSetCurrentMove write FOnSetCurrentMove;
      property OnReplaceDigraphs:TOnReplaceDigraphs read FOnReplaceDigraphs write FOnReplaceDigraphs;
      property OnIsKnownWord: TOnIsKnownWord read FOnIsKnownWord write FOnIsKnownWord;

      property History[aMoveNumber:integer]:TGameCourseMove read GetHistory;
      property HistoryCount:integer read GetHistoryCount;

      property BestValue[aMoveNumber:word]:integer write SetBest;
      property MaskExchanged:boolean read FMaskExchanged write SetMaskExchanged;
      property Strikeout:boolean read FStrikeout write FStrikeout;
      property PlayerColor[aPlayerNumber:byte]:TColor read GetPlayerColor write SetPlayerColor;
      property Shaded:boolean read FShaded write SetShaded;
      property Colored:boolean read FColored write SetColored;
      property BiDiMode:TBiDiMode read FBiDiMode write FBiDiMode;
      property AllBestMovesValues:string read GetAllBestmoveValues write SetAllBestmoveValues;
    end;//TGameCourse;

implementation

uses
  uconfig;

{ TGameCourse }

const cDelimiter=';';

var SortUpDown  : (udUP, udDown);
    SortBy  : byte;

function CompareList(Item1, Item2: Pointer): Integer;
begin
  case SortBy of
   0 : Result:=CompareValue(TGameCourseMove(Item1).MoveNumber,TGameCourseMove(Item2).MoveNumber);
   1 : Result:=CompareText(TGameCourseMove(Item1).PlacedWord,TGameCourseMove(Item2).PlacedWord);
   2 : Result:=CompareValue(TGameCourseMove(Item1).Value,TGameCourseMove(Item2).Value);
   3 : Result:=CompareValue(TGameCourseMove(Item1).Best,TGameCourseMove(Item2).Best);
   4 : Result:=CompareValue(TGameCourseMove(Item1).TimeUsed,TGameCourseMove(Item2).TimeUsed);
  end;
  if (SortUpdown=udDown) then
    Result:=Result*-1;
end;

procedure TGameCourse.DoClearBest(Sender: TObject);
var
  i:integer;
begin
  for i:=0 to FHistory.Count-1 do
    TGameCourseMove(FHistory[i]).Best:=-1;
  FStringGrid.Repaint;
end;

function TGameCourse.GetAllBestmoveValues: string;
var
  i:integer;
  OldSortBy,OldSortUpDown:byte;
begin
  OldSortBy:=SortBy;
  OldSortUpDown:=integer(SortUpDown);
  SortBy:=0;
  SortUpDown:=udUp;
  FHistory.Sort(@CompareList);

  Result:='';
  for i:=0 to FHistory.Count-1 do
   Result:=Result+inttostr(TGameCourseMove(FHistory[i]).Best)+',';
  if length(Result)>0 then
    delete(Result,length(Result),1);

  SortBy:=OldSortBy;
  if OldSortUpDown=0 then
    SortUpDown:=udUp else SortUpDown:=udDown;
  FHistory.Sort(@CompareList);
end;

function ReplaceChars(s: string; const ReplaceChar: Char): string;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to UTF8Length(s) do
    Result:=Result+ReplaceChar;
end;

procedure TGameCourse.DoChangeFont(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Font:=FStringGrid.Font;
    if Execute then
    begin
      FStringGrid.Font:=Font;
      DoResize(self);
      FStringGrid.Repaint;
    end;
  finally
    Free;
  end;
end;

procedure TGameCourse.DoDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);

 procedure DoPaintWord(aMove:TGameCourseMove;DT_BiDi:integer);
 var
   s,dummy: string;
   z: integer;
 begin
   with aMove do
   begin
    s:='';
    if (ExchangedLetters<>'') then
    begin
      if FMaskExchanged then
        s:='('+ReplaceChars(ExchangedLetters,'*')+') ' else
        s:='('+FOnReplaceDigraphs(ExchangedLetters)+') ';
    end;
    if (CambioSecco<>'') then
    begin
      if FMaskExchanged then
        s:=s+'['+ReplaceChars(CambioSecco,'*')+'] ' else
        s:=s+'['+FOnReplaceDigraphs(CambioSecco)+'] ';
    end;
    s:=s+FOnReplaceDigraphs(PlacedWord);

    if BiDiMode=bdLeftToRight then
      inc(aRect.Left,1) else
      dec(aRect.Right,1);
    inc(aRect.Top,1);
    if FColored then
    begin
      FStringGrid.Canvas.Font.Color:=FPlayerColor[TGameCourseMove(FHistory[aRow-1]).PlayerNumber];
      DrawText(FStringGrid.Canvas.Handle,PChar(s),-1,aRect, DT_TOP or DT_BiDi);
    end;

    if (ExchangedLetters='') and FStrikeout and assigned(FOnIsKnownWord) then
      FStringGrid.Canvas.Font.StrikeThrough:=not FOnIsKnownWord(PlacedWord+','+ConnectedWords,dummy,false);

    if BiDiMode=bdLeftToRight then
      inc(aRect.Left,1) else
      dec(aRect.Right,1);
    if not FColored or FShaded then
    begin
      FStringGrid.Canvas.Font.Color:=LightOrDark(FStringGrid.Canvas.Brush.Color);//clBlack;
      DrawText(FStringGrid.Canvas.Handle,PChar(s),-1,aRect, DT_TOP or DT_BiDi);
    end;
    FStringGrid.Canvas.Font.StrikeThrough:=false;

    //connected words
    inc(aRect.Top,FStringGrid.Canvas.TextHeight('ABC')+1);
    FStringGrid.Canvas.Font.Color:=clGray;
    z:=FStringGrid.Font.Size;
    FStringGrid.Canvas.Font.Size:=z-2;
    s:=FOnReplaceDigraphs(ConnectedWords);
    DrawText(FStringGrid.Canvas.Handle,PChar(s),-1,aRect, DT_WORDBREAK or DT_BOTTOM or DT_BiDi);
    FStringGrid.Canvas.Font.Size:=z;
   end;
 end;
 procedure DoPaintValue(aMove:TGameCourseMove);
 var
   s:string;
   i,z:integer;
 begin
   with aMove do
   begin
    if IsScrabble then
      FStringGrid.Canvas.Draw(aRect.Left+1,aRect.Top+1,FIsScrabble);
    inc(aRect.Top,1);
    dec(aRect.Right,1);
    if FColored then
    begin
      FStringGrid.Canvas.Font.Color:=FPlayerColor[aMove.PlayerNumber];
      DrawText(FStringGrid.Canvas.Handle,PChar(inttostr(Value)),-1,aRect,DT_TOP or DT_RIGHT);
    end;
    if not FColored or FShaded then
    begin
      dec(aRect.Right,1);
      FStringGrid.Canvas.Font.Color:=LightOrDark(FStringGrid.Canvas.Brush.Color);//clBlack;
      DrawText(FStringGrid.Canvas.Handle,PChar(inttostr(Value)),-1,aRect,DT_TOP or DT_RIGHT);
    end;

    inc(aRect.Top,FStringGrid.Canvas.TextHeight('123')+1);
    z:=FStringGrid.Font.Size;
    FStringGrid.Canvas.Font.Size:=z-2;
    s:='';
    for i:=3 downto 0 do
    if Penalty[i]<>0 then
    begin
      if s<>'' then s:=',';
      s:=inttostr(Penalty[i])+s;
      if Penalty[i]>0 then
        s:='+'+s;
      if FColored then
      begin
        FStringGrid.Canvas.Font.Color:=FPlayerColor[i];
        DrawText(FStringGrid.Canvas.Handle,PChar(s),-1,aRect,DT_WORDBREAK or DT_BOTTOM or DT_RIGHT);
      end;
      if not FColored or FShaded then
      begin
        dec(aRect.Right,1);
        FStringGrid.Canvas.Font.Color:=clGray;
        DrawText(FStringGrid.Canvas.Handle,PChar(s),-1,aRect,DT_WORDBREAK or DT_BOTTOM or DT_RIGHT);
      end;
      dec(aRect.Right,FStringGrid.Canvas.TextWidth(s));
    end;
    FStringGrid.Canvas.Font.Size:=z;
   end;
 end;

var
  tde : TThemedElementDetails;
  s : string;
  aFontName : string;
  aFontSize : integer;
  DT_BiDi : integer;
begin
  with FStringGrid do
  begin
   BiDiMode:=FBiDiMode;
   if BiDiMode=bdLeftToRight then
     DT_BiDi:=DT_LEFT else DT_BiDi:=DT_RIGHT or DT_RTLREADING;
   if (aRow=0) then
   begin
     aFontSize:=FStringGrid.Font.Size;
     aFontName:=FStringGrid.Font.Name;
     FStringGrid.Font.Name:=Parent.Font.Name;
     FStringGrid.Font.Size:=Parent.Font.Size;
     if ThemeServices.ThemesEnabled then
     begin
       if aCol=SortBy then
        tde:=ThemeServices.GetElementDetails(thHeaderItemHot) else
        tde:=ThemeServices.GetElementDetails(thHeaderRoot);
       ThemeServices.DrawElement(Canvas.Handle,tde,aRect, nil);
       Canvas.TextRect(aRect, aRect.Left+3, aRect.Top+1, Cells[aCol,aRow]);
     end else
     begin
       Canvas.Brush.Style:=bsSolid;
       Canvas.Brush.Color:=clBtnFace;
       Canvas.FillRect(aRect);
       Canvas.Frame3d(aRect,1,bvRaised);
       Canvas.Font.Color:=clWindowText;
       InflateRect(aRect,1,1);
       Canvas.TextRect(aRect, aRect.Left+2, aRect.Top+1,Cells[aCol,aRow]);
     end;
     FStringGrid.Font.Name:=aFontName;
     FStringGrid.Font.Size:=aFontSize;
   end else
   begin
     if (FHistory.Count>=aRow) then
     begin
       Canvas.Brush.Style:=bsSolid;
       if (TGameCourseMove(FHistory[aRow-1]).MoveNumber=FActualMove) then
         Canvas.Brush.Color:=clScrabble else //clHighlight
         Canvas.Brush.Color:=clWindow;
       Canvas.FillRect(aRect);
       if FColored then
         Canvas.Font.Color:=FPlayerColor[TGameCourseMove(FHistory[aRow-1]).PlayerNumber] else
         Canvas.Font.Color:=LightOrDark(FStringGrid.Canvas.Brush.Color);//clBlack;
       Canvas.Brush.Style:=bsClear;

       case aCol of
        0 : s:=inttostr(TGameCourseMove(FHistory[aRow-1]).MoveNumber+1);
        1 : if assigned(FOnReplaceDigraphs) then
              DoPaintWord(TGameCourseMove(FHistory[aRow-1]),DT_BiDi);
        2 : DoPaintValue(TGameCourseMove(FHistory[aRow-1]));
        3 : if TGameCourseMove(FHistory[aRow-1]).Best>-1 then
              s:=IntToStr(TGameCourseMove(FHistory[aRow-1]).Best) else
              s:='';
        4 : s:=SecondsToString(TGameCourseMove(FHistory[aRow-1]).TimeUsed,false);
       end;//case
       if not (aCol in [1,2]) then
       begin
         dec(aRect.Right,5);
         inc(aRect.Top,1);
         if FColored then
           DrawText(Canvas.Handle,PChar(s),-1,aRect, DT_RIGHT);
         if not FColored or FShaded then
         begin
           inc(aRect.Right,1);
           Canvas.Font.Color:=LightOrDark(FStringGrid.Canvas.Brush.Color);//clBlack;
           DrawText(Canvas.Handle,PChar(s),-1,aRect, DT_RIGHT);
         end;
       end;//aCol<>1
     end;//with FHistory[]
   end;//aRow<>0
  end;//with FStrngGrid
end;

procedure TGameCourse.DoHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  if SortUpDown=udUp then
    SortUpDown:=udDown else
    SortUpDown:=udUp;
  SortBy:=Index;
  FHistory.Sort(@CompareList);
  DoResize(self);
  FStringGrid.Repaint;
end;

procedure TGameCourse.DoResize(Sender: TObject);
var i,j,y,z:integer;
    aRect:TRect;
begin
  with FStringGrid do
  try
    BeginUpdate;

    //row height by content
    for i:=1 to RowCount-1 do
    begin
      aRect:=Bounds(0,0,ColWidths[1],24);
      DrawText(Canvas.Handle,PChar('ABCDE'),-1,aRect, DT_CALCRECT or DT_TOP or DT_LEFT);
      RowHeights[i]:=aRect.Bottom-aRect.Top+4;
      y:=RowHeights[i];
      z:=Font.Size;
      Canvas.Font.Size:=z-2;
      if TGameCourseMove(FHistory[i-1]).ConnectedWords<>'' then
      begin
        aRect:=Bounds(0,0,ColWidths[1],24);
        DrawText(Canvas.Handle,PChar(TGameCourseMove(FHistory[i-1]).ConnectedWords),-1,aRect, DT_CALCRECT or DT_WORDBREAK or DT_BOTTOM or DT_LEFT);
        y:=RowHeights[i]+(aRect.Bottom-aRect.Top)+2;
      end;
      for j:=0 to 3 do
       if TGameCourseMove(FHistory[i-1]).Penalty[j]<>0 then
       begin
         aRect.Right:=ColWidths[2];
         DrawText(Canvas.Handle,PChar('+10,+20'),-1,aRect, DT_CALCRECT or DT_WORDBREAK or DT_BOTTOM or DT_RIGHT);
         if y<RowHeights[i]+(aRect.Bottom-aRect.Top)+2 then
           y:=RowHeights[i]+(aRect.Bottom-aRect.Top)+2;
         break;
       end;
      Canvas.Font.Size:=z;
      RowHeights[i]:=y;
    end;//for to
  finally
    EndUpdate;
  end;//with
end;

procedure TGameCourse.DoSelection(Sender: TObject; aCol, aRow: Integer);
var
  i: integer;
begin
  if assigned(FOnSetCurrentMove) and
    (FHistory.Count>aRow-1) and
    (aRow>0) then
  begin
    i:=FOnSetCurrentMove(TGameCourseMove(FHistory[aRow-1]).MoveNumber);
    if i=FActualMove then
      FActualMove:=-1 else FActualMove:=i;
    FStringGrid.Repaint;
  end;
end;

function TGameCourse.GetHistoryCount: integer;
begin
  Result:=FHistory.Count;
end;

function TGameCourse.GetHistory(aMoveNumber: integer): TGameCourseMove;
var i:integer;
begin
  for i:=0 to FHistory.Count-1 do
   if TGameCourseMove(FHistory[i]).MoveNumber=aMoveNumber then
   begin
     Result:=TGameCourseMove(FHistory[i]);
     exit;
   end;
  Result:=nil;
end;

function TGameCourse.GetPlayerColor(aPlayerNumber: byte): TColor;
begin
  Result:=FPlayerColor[aPlayerNumber];
end;

procedure TGameCourse.SetAllBestmoveValues(aValue: string);
var
  i:integer;
  OldSortBy,OldSortUpDown:byte;
begin
  OldSortBy:=SortBy;
  OldSortUpDown:=integer(SortUpDown);
  SortBy:=0;
  SortUpDown:=udUp;
  FHistory.Sort(@CompareList);

  with TStringList.Create do
  try
    CommaText:=aValue;
    for i:=0 to Count-1 do
     if i<FHistory.Count then
      TGameCourseMove(FHistory[i]).Best:=StrToIntDef(Strings[i],-1);
  finally
    Free;
  end;

  SortBy:=OldSortBy;
  if OldSortUpDown=0 then
    SortUpDown:=udUp else SortUpDown:=udDown;
  FHistory.Sort(@CompareList);
  FStringGrid.Invalidate;
end;

procedure TGameCourse.SetBest(aMoveNumber: word; const aValue: integer);
var i:integer;
    aGameCourseMove:TGameCourseMove;
begin
  for i:=0 to FHistory.Count-1 do
   with TGameCourseMove(FHistory[i]) do
    if (MoveNumber=aMoveNumber) then
    begin
      Best:=aValue;
      FStringGrid.Repaint;
      exit;
    end;
  //not yet set
  aGameCourseMove:=TGameCourseMove.Create;
  aGameCourseMove.MoveNumber:=aMoveNumber;
  aGameCourseMove.Best:=aValue;
  FHistory.Add(aGameCourseMove);
  FStringGrid.Repaint;
end;

procedure TGameCourse.SetColored(const AValue: boolean);
begin
  if FColored<>aValue then
  begin
    FColored:=aValue;
    FStringGrid.Repaint;
  end;
end;

function TGameCourse.DoGetRackLetterIndex(aIndex:word; aRackLetter:byte): integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to FHistory.Count-1 do
   with TGameCourseMove(FHistory[i]) do
    if (MoveNumber=aIndex) then
     if (aRackLetter<length(RackLetters)) then
      exit(RackLetters[aRackLetter]);
end;

procedure TGameCourse.SetMaskExchanged(const aValue: boolean);
begin
  if FMaskExchanged<>aValue then
  begin
    FMaskExchanged:=aValue;
    FStringGrid.Repaint
  end;
end;

procedure TGameCourse.AddPenalty(aPlayer:byte; aMoveNumber:word; aValue:integer);
var
  AddMove: boolean;
  aGameCourseMove:TGameCourseMove;
begin
  aGameCourseMove:=GetHistory(aMoveNumber);
  if aGameCourseMove=nil then
  begin
    aGameCourseMove:=TGameCourseMove.Create;
    aGameCourseMove.MoveNumber:=aMoveNumber;
    aGameCourseMove.Best:=-1;
    aGameCourseMove.Penalty[0]:=0;
    aGameCourseMove.Penalty[1]:=0;
    aGameCourseMove.Penalty[2]:=0;
    aGameCourseMove.Penalty[3]:=0;
    AddMove:=true;
  end else
    AddMove:=false;

  aGameCourseMove.Penalty[aPlayer]:=aGameCourseMove.Penalty[aPlayer]+aValue;
  if AddMove then
    FHistory.Add(aGameCourseMove);
  DoResize(self); //adjust row height
end;

procedure TGameCourse.SetPlayerColor(aPlayerNumber: byte; const aValue: TColor);
begin
  if aValue<>FPlayerColor[aPlayerNumber] then
  begin
    FPlayerColor[aPlayerNumber]:=aValue;
    FStringGrid.Repaint;
  end;
end;

procedure TGameCourse.SetShaded(const AValue: boolean);
begin
  if FShaded<>aValue then
  begin
    FShaded:=aValue;
    FStringGrid.Repaint;
  end;
end;

constructor TGameCourse.Create(AOwner: TComponent);
var
  aMenuItem: TMenuItem;
  aFontData: TFontData;
begin
  inherited Create;
  FHistory:=TList.Create;
  FMaskExchanged:=true;

  FStringGrid:=TStringGrid.Create(aOwner);
  with FStringGrid do
  begin
    Parent:=aOwner as TWinControl;
    Align:=alClient;
    BorderStyle:=bsNone;
    DefaultDrawing:=false;
    FixedCols:=0;
    Options:=Options+[goRowSelect,goColSizing,goThumbTracking,goSmoothScroll];
    Options:=Options-[goRangeSelect];
    RowCount:=1;
    ColCount:=5;
    aFontData:=GetFontData(Application.MainForm.Font.Reference.Handle);
    with Config do
    begin
      Font.Name:=Read('General/GameCourse/FontName',aFontData.Name);
      Font.Size:=Read('General/GameCourse/FontSize',aFontData.Height);
    end;
    ShowHint:=true;
    TitleStyle:=tsNative;
    OnDrawCell:=@DoDrawCell;
    OnHeaderClick:=@DoHeaderClick;
    OnSelection:=@DoSelection;
    OnResize:=@DoResize;
  end;

  FPopupMenu:=TPopupMenu.Create(FStringGrid);
  with FPopupMenu do
  begin
    Parent:=FStringGrid;
    FStringGrid.PopupMenu:=FPopupMenu;
    aMenuItem:=TMenuItem.Create(FPopupMenu);
    with aMenuItem do
    begin
      Caption:=rGameCourse_Menu1;
      OnClick:=@DoClearBest;
    end;
    Items.Add(aMenuItem);
    aMenuItem:=TMenuItem.Create(FPopupMenu);
    with aMenuItem do
    begin
      Caption:=rMessages_ChangeFont;
      OnClick:=@DoChangeFont;
    end;
    Items.Add(aMenuItem);
  end;

  UpdateLanguage;

  FActualMove:=-1;
  FIsScrabble:=TBitmap.Create;
  FIsScrabble.LoadFromLazarusResource('Scrabble3D_16x16');
  SortBy:=0;
  SortUpDown:=udDown;
end;

destructor TGameCourse.Destroy;
begin
  with Config do
  begin
    Write('General/GameCourse/FontName',FStringGrid.Font.Name);
    Write('General/GameCourse/FontSize',FStringGrid.Font.Size);
  end;
  Clear;
  FStringGrid.Free;
  FHistory.Free;
  FIsScrabble.Free;
  inherited Destroy;
end;

procedure TGameCourse.UpdateLanguage;
begin
  with FStringGrid do
  begin
    Cells[0,0]:=rGameCourse_Col0;
    Cells[1,0]:=rGameCourse_Col1;
    Cells[2,0]:=rGameCourse_Col2;
    Cells[3,0]:=rGameCourse_Col3;
    Cells[4,0]:=rGameCourse_Col4;
    Hint:=rGameCourse_Hint;
  end;
  with FPopupMenu do
  begin
    Items[0].Caption:=rGameCourse_Menu1;
    Items[1].Caption:=rMessages_ChangeFont;
  end;
end;

procedure TGameCourse.DoAddMove(const aPlacedWord, aConnectedWords, aExchangedLetters, aCambioSecco:string;
                                const aRackLetters:THistoryArray;
                                const aPlayer:byte;
                                const aMoveNumber, aValue: word;
                                const aTimeUsed: Longword;
                                const aIsScrabble:boolean;
                                const aDimension,aPosition:byte);
var
  aGameCourseMove: TGameCourseMove;
  AddMove: boolean;
begin
  aGameCourseMove:=GetHistory(aMoveNumber);
  if aGameCourseMove=nil then
  begin
    aGameCourseMove:=TGameCourseMove.Create;
    aGameCourseMove.MoveNumber:=aMoveNumber;
    aGameCourseMove.Best:=-1;
    aGameCourseMove.Penalty[0]:=0;
    aGameCourseMove.Penalty[1]:=0;
    aGameCourseMove.Penalty[2]:=0;
    aGameCourseMove.Penalty[3]:=0;
    AddMove:=true;
  end else
    AddMove:=false;
  aGameCourseMove.PlacedWord:=aPlacedWord;
  aGameCourseMove.ConnectedWords:=aConnectedWords;
  aGameCourseMove.ExchangedLetters:=aExchangedLetters;
  aGameCourseMove.CambioSecco:=aCambioSecco;
  aGameCourseMove.RackLetters:=aRackLetters;
  aGameCourseMove.PlayerNumber:=aPlayer;
  aGameCourseMove.Value:=aValue;
  aGameCourseMove.TimeUsed:=aTimeUsed;
  aGameCourseMove.IsScrabble:=aIsScrabble;
  aGameCourseMove.Dimension:=aDimension;
  aGameCourseMove.Position:=aPosition;
  if AddMove then
    FHistory.Add(aGameCourseMove);
  FStringGrid.RowCount:=FHistory.Count+1;
  FActualMove:=-1;
  FHistory.Sort(@CompareList);
  DoResize(self);
end;

procedure TGameCourse.Clear;
var i:integer;
begin
  for i:=0 to FHistory.Count-1 do
  begin
    setlength(TGameCourseMove(FHistory[i]).RackLetters,0);
    TGameCourseMove(FHistory[i]).Free;
  end;
  FActualMove:=-1;
  FHistory.Clear;
  FStringGrid.RowCount:=1;
end;

procedure TGameCourse.LoadFrom(const aFileName: string);
var
  i,j,k : integer;
  s: string;
  aGameCourseMove:TGameCourseMove;
begin
  self.Clear;
  with TIniFile.Create(aFileName) do
  try
    i:=0;
    repeat
      s:=ReadString('History',IntToStr(i),'');
      if s<>'' then
      begin
        aGameCourseMove:=TGameCourseMove.Create;
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.PlacedWord:=UTF8Copy(s,1,j-1); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.ConnectedWords:=UTF8Copy(s,1,j-1); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.ExchangedLetters:=UTF8Copy(s,1,j-1); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.CambioSecco:=UTF8Copy(s,1,j-1); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s);
        with TStringList.Create do
        try
          Delimiter:=',';
          CommaText:=UTF8Copy(s,1,j-1);
          setlength(aGameCourseMove.RackLetters,Count);
          for k:=0 to Count-1 do
            aGameCourseMove.RackLetters[k]:=StrToInt(Strings[k]);
        finally
          Free;
        end;
        UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.MoveNumber:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.PlayerNumber:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.Value:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.Best:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.TimeUsed:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.IsScrabble:=StrToBool(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.Penalty[0]:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.Penalty[1]:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.Penalty[2]:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.Penalty[3]:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
        j:=UTF8Pos(cDelimiter,s); aGameCourseMove.Dimension:=StrToInt(UTF8Copy(s,1,j-1)); UTF8Delete(s,1,j);
                                  aGameCourseMove.Position:=StrToInt(s);
        FHistory.Add(aGameCourseMove);
      end;
      inc(i);
    until s='';
    FStringGrid.RowCount:=FHistory.Count+1;
    FHistory.Sort(@CompareList);
    DoResize(self);
  finally
    Free;
  end;
end;

procedure TGameCourse.SaveTo(const aFileName: string);
var i,j:integer;
    s:string;
    OldSortBy,OldSortUpDown:byte;
begin
  OldSortBy:=SortBy;
  OldSortUpDown:=integer(SortUpDown);
  SortBy:=0;
  SortUpDown:=udUp;
  FHistory.Sort(@CompareList);
  with TIniFile.Create(aFileName) do
  try
    EraseSection('History');
    for i:=0 to FHistory.Count-1 do
     with TGameCourseMove(FHistory[i]) do
     begin
       s:=PlacedWord+cDelimiter+ConnectedWords+cDelimiter+ExchangedLetters+cDelimiter+CambioSecco+cDelimiter;
       with TStringList.Create do
       try
         Delimiter:=',';
         for j:=0 to length(RackLetters)-1 do
           Add(IntToStr(RackLetters[j]));
         s:=s+CommaText;
       finally
         Free;
       end;
       s:=s+cDelimiter+
          IntToStr(MoveNumber)+cDelimiter+
          IntToStr(PlayerNumber)+cDelimiter+
          IntToStr(Value)+cDelimiter+
          IntToStr(Best)+cDelimiter+
          IntToStr(TimeUsed)+cDelimiter+
          BoolToStr(IsScrabble)+cDelimiter+
          IntToStr(Penalty[0])+cDelimiter+
          IntToStr(Penalty[1])+cDelimiter+
          IntToStr(Penalty[2])+cDelimiter+
          IntToStr(Penalty[3])+cDelimiter+
          IntToStr(Dimension)+cDelimiter+
          IntToStr(Position);
       WriteString('History',inttostr(i),s);
     end;
  finally
    Free;
  end;
  SortBy:=OldSortBy;
  if OldSortUpDown=0 then
    SortUpDown:=udUp else SortUpDown:=udDown;
end;

procedure TGameCourse.Takeback;
var
  OldSortBy,OldSortUpDown:byte;
begin
  OldSortBy:=SortBy;
  OldSortUpDown:=integer(SortUpDown);
  SortBy:=0;
  SortUpDown:=udUp;
  FHistory.Sort(@CompareList);

  {.$define full_takeback}
  {$ifdef full_takeback}
  {$Warning history}
  TGameCourseMove(FHistory[FHistory.Count-1]).Free;
  FHistory.Delete(FHistory.Count-1);
  {$else}
  if FHistory.Count>0 then
   with TGameCourseMove(FHistory[FHistory.Count-1]) do
   begin
     PlacedWord:='';
     ConnectedWords:='';
     Value:=0;
     IsScrabble:=false;
   end;
  {$endif}
  FActualMove:=-1;
  SortBy:=OldSortBy;
  if OldSortUpDown=0 then
    SortUpDown:=udUp else SortUpDown:=udDown;
  FHistory.Sort(@CompareList);
  FStringGrid.Invalidate;
end;

procedure TGameCourse.AdjustColWidth;
const
  DefaultWidth:array[0..4] of word=(25,125,50,50,50);
  DefaultSum=300;
var
  i:integer;
begin
  for i:=0 to 4 do
    FStringGrid.ColWidths[i]:=DefaultWidth[i];
  //percent of width
  if FStringGrid.ClientWidth-20>DefaultSum then
   for i:=0 to FStringGrid.ColCount-1 do
    FStringGrid.ColWidths[i]:=round(FStringGrid.ColWidths[i]/DefaultSum*(FStringGrid.ClientWidth-20));
end;

initialization
  {$I GameCourse.res}

end.

