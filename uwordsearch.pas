{ Form to browse and to search the dictionary

  v3.1.3; 2015-Mar-01
  Copyleft (C) GPLv3: 1996-2015 Heiko Tietze heiko_tietze@web.de

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

unit uwordsearch;

{$mode objfpc}{$H+}

{$I conditions.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Themes,
  StdCtrls, Spin, Buttons, ExtCtrls, Grids, LazUTF8, LCLProc, LCLType,
  Menus, Math, Clipbrd, {$ifdef UseRegEx}uregexpr, {$endif}
  ulanguage;

type

  { TfmWordSearch }
  TWordSearchMode=(wmAll=0,wmSearch=1,wmNone=2);

  TfmWordSearch = class(TForm)
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    cbCategory: TComboBox;
    edPattern: TEdit;
    edWord: TEdit;
    gbWord: TGroupBox;
    gbSearch: TGroupBox;
    lbPattern: TLabel;
    lbWordNumber: TLabel;
    stWordCount: TStaticText;
    lbWord: TLabel;
    lbWordCategory: TLabel;
    lbWordMeaning: TLabel;
    miWordSearchCopy: TMenuItem;
    mmWordMeaning: TMemo;
    pmWordSearchCopy: TPopupMenu;
    rgSearchType: TRadioGroup;
    seWordNumber: TSpinEdit;
    sbSearch: TSpeedButton;
    sgSearchResult: TStringGrid;
    Splitter: TSplitter;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure cbCategoryChange(Sender: TObject);
    procedure edWordChange(Sender: TObject);
    procedure edUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure miWordSearchCopyClick(Sender: TObject);
    procedure sbSearchClick(Sender: TObject);
    procedure seWordNumberChange(Sender: TObject);
    procedure sgSearchResultCompareCells(Sender: TObject; ACol, ARow, BCol, BRow: Integer; var Result: integer);
    procedure sgSearchResultDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgSearchResultHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure sgSearchResultResize(Sender: TObject);
    procedure sgSearchResultSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    FWordSearchMode: TWordSearchMode;
    procedure SetWordSearchMode(const AValue: TWordSearchMode);
  public
    procedure UpdateDictionaryInfo;
    procedure UpdateLanguage;
    property Mode:TWordSearchMode read FWordSearchMode write SetWordSearchMode;
  end;

var
  fmWordSearch: TfmWordSearch;

implementation

uses
  udictionary, uconfig, utypes, ugameoptions;

{ TfmWordSearch }

procedure TfmWordSearch.miWordSearchCopyClick(Sender: TObject);
var i:integer;
    s:string;
begin
  s:='';
  for i:=1 to sgSearchResult.RowCount-1 do
    s:=s+sgSearchResult.Cells[0,i]+LineBreak;
  ClipBoard.AsText:=s;
end;

procedure TfmWordSearch.UpdateDictionaryInfo;
begin
  if assigned(Dictionary) then
  begin
    seWordNumber.MaxValue:=Dictionary.Info[itWordCount];
    if Dictionary.Info[itWordCount]>0 then seWordNumber.MinValue:=1;
    stWordCount.Caption:=inttostr(Dictionary.Info[itWordCount]);
    seWordNumber.Enabled:=(Dictionary.Info[itWordCount]>0) and (FWordSearchMode<>wmNone);
    edWord.Enabled:=(Dictionary.Info[itWordCount]>0) and (FWordSearchMode<>wmNone);
    {$ifdef darwin}
    cbCategory.OnChange:=nil;
    {$endif}
    cbCategory.Items.Assign(Dictionary.Categories);
    {$ifdef darwin}
    cbCategory.OnChange:=@cbCategoryChange;
    {$endif}
  end;
  edWordChange(self);
end;

procedure TfmWordSearch.UpdateLanguage;
var
  aLabel:TLabel;
begin
  aLabel:=lbWord;
  if lbWordNumber.Width>aLabel.Width then
    aLabel:=lbWordNumber;
  if lbWordCategory.Width>aLabel.Width then
    aLabel:=lbWordCategory;
  if lbWordNumber.Width>aLabel.Width then
    aLabel:=lbWordNumber;
  edWord.AnchorSide[akLeft].Control:=aLabel;
  seWordNumber.AnchorSide[akLeft].Control:=aLabel;
  cbCategory.AnchorSide[akLeft].Control:=aLabel;
end;

procedure TfmWordSearch.seWordNumberChange(Sender: TObject);
begin
  if (seWordNumber.Value>0) and (seWordNumber.Value<=Dictionary.Info[itWordCount]) then
  begin
    edWord.Text:=Dictionary[seWordNumber.Value-1];//0..Count-1
    {$ifdef darwin} edWordChange(self);{$endif}
  end;
end;

procedure TfmWordSearch.sgSearchResultCompareCells(Sender: TObject; ACol, ARow,
  BCol, BRow: Integer; var Result: integer);
begin
  with sgSearchResult do
  begin
    if Tag=-1 then
      Result:=-CompareValue(LazUTF8.UTF8Length(Cells[aCol,aRow]),LazUTF8.UTF8Length(Cells[bCol,bRow]));
    if (Tag=1) or (Result=0) then
      Result:=CompareText(Cells[aCol,aRow],Cells[bCol,bRow]);
  end;
end;

procedure TfmWordSearch.sgSearchResultDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  i:integer;
begin
  with sgSearchResult.Canvas do
  begin
    if aRow=0 then
    begin
      if ThemeServices.ThemesEnabled then
      begin
        ThemeServices.DrawElement(Handle,ThemeServices.GetElementDetails(thHeaderDontCare),aRect, nil);
        TextRect(aRect, aRect.Left+2, aRect.Top+1, sgSearchResult.Columns[aCol].Title.Caption);
      end else
      begin
        Brush.Style:=bsSolid;
        Brush.Color:=clBtnFace;
        FillRect(aRect);
        Frame3d(aRect,1,bvRaised);
        Font.Color:=clWindowText;
        TextRect(aRect, aRect.Left+2, aRect.Top+1,sgSearchResult.Columns[aCol].Title.Caption);
      end;
    end else
    begin
      if Dictionary.IsWordInDictionary(sgSearchResult.Cells[aCol,aRow],i)=frExclude then
        Brush.Color:=clYellow else
        Brush.Color:=clWindow;
      FillRect(aRect);
      if (gdSelected in aState) then
        DrawFocusRect(aRect);
      TextRect(aRect,2,sgSearchResult.DefaultRowHeight div 2-sgSearchResult.Canvas.TextHeight('A') div 2,sgSearchResult.Cells[aCol,aRow]);
    end;
  end;
end;

procedure TfmWordSearch.sgSearchResultHeaderClick(Sender: TObject;IsColumn: Boolean; Index: Integer);
begin
  sgSearchResult.SortColRow(IsColumn,Index);
  sgSearchResult.Tag:=sgSearchResult.Tag*-1;
end;

procedure TfmWordSearch.sgSearchResultResize(Sender: TObject);
begin
  sgSearchResult.ColWidths[0]:=sgSearchResult.ClientWidth;
end;

procedure TfmWordSearch.sgSearchResultSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  edWord.Text:=sgSearchResult.Cells[aCol,aRow];
end;

procedure TfmWordSearch.SetWordSearchMode(const AValue: TWordSearchMode);
var i:integer;
begin
  if FWordSearchMode<>aValue then
  begin
    FWordSearchMode:=aValue;

    for i:=0 to gbSearch.ControlCount-1 do
      gbSearch.Controls[i].Enabled:=FWordSearchMode in [wmAll];

    for i:=0 to gbWord.ControlCount-1 do
      gbWord.Controls[i].Enabled:=FWordSearchMode in [wmAll,wmSearch];
  end;
end;

procedure TfmWordSearch.edUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  aAvail:string;
  z:integer;
  aHintWindow:THintWindow;
  aRect:TRect;
begin
  if not (ord(UTF8Key[1]) in [VK_BACK,VK_RETURN,VK_ESCAPE]) then
  begin
    aAvail:=fmGameOptions.AvailableLetters;
    z:=LazUTF8.UTF8Pos(UTF8Key,aAvail);
    if z=0 then
      z:=LazUTF8.UTF8Pos(LazUTF8.UTF8UpperCase(UTF8Key),aAvail);
    if z=0 then
      z:=LazUTF8.UTF8Pos(LazUTF8.UTF8LowerCase(UTF8Key),aAvail);
    if (z=0) then
    begin
      if (Sender=edWord) or
         ((rgSearchType.ItemIndex=0) and ((UTF8Key<>'?') and (UTF8Key<>'*'))) or
         ((rgSearchType.ItemIndex=2) and (UTF8Key<>'?')) then
      begin
//        OnMessage(smWarning,Language.Format(rWordSearch_MissingCharEx,[UTF8Key]));
        aHintWindow:=THintWindow.Create(self);
        aHintWindow.BiDiMode:=Language.BiDiMode;
//        aHintWindow.HideInterval:=Application.HintHidePause;
        aHintWindow.AutoHide:=true;
        aRect:=aHintWindow.CalcHintRect(TEdit(Sender).Width,Language.Format(rWordSearch_MissingCharEx,[UTF8Key]),nil);
        with TEdit(Sender) do
          OffsetRect(aRect,ClientOrigin.x,ClientOrigin.y+Height);
        aHintWindow.ActivateHint(aRect,Language.Format(rWordSearch_MissingCharEx,[UTF8Key]));

        UTF8Key:='';
      end;
    end else
      UTF8Key:=LazUTF8.UTF8Copy(aAvail,z,1);
  end;
  if (Sender=edPattern) and (UTF8Key=chr(VK_RETURN)) then
    sbSearchClick(self);
end;

procedure TfmWordSearch.edWordChange(Sender: TObject);
var
  i:integer;
  z:TFindResult;
begin
  z:=Dictionary.IsWordInDictionary(edWord.Text,i,true);
  case z of
   frInclude : edWord.Color:=clWhite;
   frExclude : edWord.Color:=clYellow;
   frNone    : edWord.Color:=clRed;
  end;
  edWord.Font.Color:=LightOrDark(edWord.Color);
  if (i<Dictionary.Info[itWordCount]) then
  begin
    if z<>frNone then
    begin
      mmWordMeaning.Text:=Dictionary.Meaning[i]; //meaning is cleared if not found
      {$ifdef darwin}
      cbCategory.OnChange:=nil;
      {$endif}
      cbCategory.ItemIndex:=cbCategory.Items.IndexOf(Dictionary.Category[i]);
      {$ifdef darwin}
      cbCategory.OnChange:=@cbCategoryChange;
      {$endif}
      seWordNumber.Value:=i+1;                   //word number is set to nearest value if not found
    end else
      mmWordMeaning.Text:='';
  end;
end;

procedure TfmWordSearch.FormCreate(Sender: TObject);
begin
  inherited;
  self.Left:=Screen.Width div 2-self.Width div 2;
  self.Top:=Screen.Height div 2-self.Height div 2;
  Config.ReadWindowPosition(self);
end;

procedure TfmWordSearch.FormShow(Sender: TObject);
begin
  gbSearch.Left:=Config.Read('Position/WordSearch',gbSearch.Left);
  seWordNumber.Value:=1;
end;

procedure TfmWordSearch.FormHide(Sender: TObject);
begin
  Config.Write('Position/WordSearch',gbSearch.Left);
end;

procedure TfmWordSearch.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_INSERT,VK_C:
      if (Shift=[ssCtrl]) then //Crtl+Insert or Ctrl+C
        miWordSearchCopyClick(self);
    VK_ESCAPE:
      Close;
  end;
end;

procedure TfmWordSearch.btnDeleteClick(Sender: TObject);
begin
  Dictionary.Delete(edWord.Text);
end;

procedure TfmWordSearch.btnAddClick(Sender: TObject);
var
  i:integer;
  s1,s2:string;
begin
  s1:=mmWordMeaning.Text;
  s2:=cbCategory.Text;
  i:=Dictionary.Add(edWord.Text);
  Dictionary.Meaning[i]:=s1;
  Dictionary.Category[i]:=s2;
  seWordNumber.Value:=i+1;
end;

procedure TfmWordSearch.cbCategoryChange(Sender: TObject);
var
  i:integer;
  s:string;
begin
  s:=cbCategory.Text;
  if (edWord.Text<>'') and (Dictionary.IsWordInDictionary(edWord.Text,i)<>frNone) then
  begin
    Dictionary.Meaning[i]:=mmWordMeaning.Text;
    Dictionary.Category[i]:=s;
    seWordNumber.Value:=i+1;
  end;
end;

procedure TfmWordSearch.sbSearchClick(Sender: TObject);
var i : integer;
    {$ifdef UseRegEx}
    s : string;
    r : TRegExpr;
    {$endif}
begin
  sgSearchResult.BeginUpdate;
  try
    sgSearchResult.RowCount:=1;
    sgSearchResult.Tag:=-1;
    case rgSearchType.ItemIndex of
    0,1: {$ifdef UseRegEx}
        if Dictionary.Info[itWordCount]>0 then
        begin
          r:=TRegExpr.Create;
          sgSearchResult.BeginUpdate;
          try
            try
              if rgSearchType.ItemIndex=0 then
              begin
                s:=edPattern.Text;
                if s[1]='*' then
                  System.Delete(s,1,1) else
                  s:='^'+s;
                if s[length(s)]='*' then
                  System.Delete(s,length(s),1) else
                  s:=s+'$';
                s:=StringReplace(s,'?','.',[rfReplaceAll]);
                s:=StringReplace(s,'*','.*',[rfReplaceAll]);
                r.Expression:=UTF8Decode(LazUTF8.UTF8LowerCase(s));
              end else
                r.Expression:=UTF8Decode(LazUTF8.UTF8LowerCase(edPattern.Text));
              for i:=0 to Dictionary.Info[itWordCount]-1 do
              begin
                if assigned(OnProgress) then
                  {$ifdef win64}
                   OnProgress(self,round(comp((i/Dictionary.Info[itWordCount])*100)));
                  {$else}
                   OnProgress(self,round((i/Dictionary.Info[itWordCount])*100));
                  {$endif}
                if r.Exec(UTF8Decode(LazUTF8.UTF8LowerCase(Dictionary[i]))) then
                begin
                  sgSearchResult.RowCount:=sgSearchResult.RowCount+1;
                  sgSearchResult.Cells[0,sgSearchResult.RowCount-1]:=Dictionary[i];
                end;
              end;
            except
              on E:Exception do OnMessage(smError,E.Message);
            end;
          finally
            sgSearchResult.EndUpdate;
            r.Free;
          end;
        end else
          OnMessage(smError,'Please load a dictionary')
        {$endif};
    2,3: with TStringList.Create do
        try
          CommaText:=Dictionary.WordsByLetters(LazUTF8.UTF8UpperCase(edPattern.Text));
          for i:=0 to Count-1 do
          if (rgSearchType.ItemIndex=2) or (LazUTF8.UTF8length(Strings[i])=LazUTF8.UTF8Length(edPattern.Text)) then
          begin
            sgSearchResult.RowCount:=sgSearchResult.RowCount+1;
            sgSearchResult.Cells[0,sgSearchResult.RowCount-1]:=Strings[i];
          end;
        finally
          Free;
        end;
    end;//case
    sgSearchResult.Columns[0].Title.Caption:=Language.Plural(rWordSearch_SearchResult,sgSearchResult.RowCount-1);
//    sgSearchResult.Row:=1;
  finally
    sgSearchResult.EndUpdate;
  end;
end;

initialization
  {$I uwordsearch.lrs}

end.
