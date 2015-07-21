{ Dialog to load remote games

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

unit uremotegames;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, Buttons, Grids, DateUtils, LCLIntf,
  LCLType, Math, Themes, Menus, upoll, utcpclient, utcptypes,
  unetwork;

type

  { TfmRemote }

  TfmRemote = class(TForm)
    btnRemoteCancel: TBitBtn;
    btnRemoteLoad: TBitBtn;
    imArrow: TImage;
    imEnd: TImage;
    pnRemote: TPanel;
    sgRemote: TStringGrid;
    procedure btnRemoteCancelClick(Sender: TObject);
    procedure btnRemoteLoadClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure sgRemoteCompareCells(Sender: TObject; ACol, ARow, BCol, BRow: Integer; var Result: integer);
    procedure sgRemoteDblClick(Sender: TObject);
    procedure sgRemoteDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgRemoteHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure sgRemoteSelection(Sender: TObject; aCol, aRow: Integer);
  private
  public
  end; 

var
  fmRemote: TfmRemote;

implementation

uses
  uconfig;

{ TfmRemote }

procedure TfmRemote.FormActivate(Sender: TObject);
const
  cWidth:array[0..4] of byte=(24,110,110,155,75);
var
  i:integer;
begin
  sgRemote.Tag:=-1;
  sgRemote.SortColRow(true,0);
  sgRemote.Tag:=1;
  sgRemote.Options:=sgRemote.Options-[goRowSelect];
  btnRemoteLoad.Enabled:=false;
  for i:=0 to 4 do
    sgRemote.ColWidths[i]:=cWidth[i];
  FormResize(self);
  Screen.Cursor:=crDefault;
end;

procedure TfmRemote.FormCreate(Sender: TObject);
begin
  inherited;
  Left:=Screen.Width div 2-Width div 2;
  Top:=Screen.Height div 2-Height div 2;
  Config.ReadWindowPosition(self);
end;

procedure TfmRemote.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key=chr(VK_ESCAPE) then
    btnRemoteCancelClick(self);
end;

procedure TfmRemote.FormResize(Sender: TObject);
var
  i,z:integer;
  s:string;
begin
  if sgRemote.Row>-1 then
    s:=sgRemote.Cells[5,sgRemote.Row] else
    s:=''; //store last selected col
  sgRemote.AutoSizeColumns;
  sgRemote.ColWidths[0]:=24;
  with sgRemote do
  begin
    z:=0;
    for i:=1 to ColCount do
      inc(z,ColWidths[i]);
    if z<Width then
    for i:=1 to ColCount do
      ColWidths[i]:=ColWidths[i]+(Width-z) div (ColCount-1);
  end;
  if s<>'' then
   for i:=1 to sgRemote.RowCount-1 do
    if s=sgRemote.Cells[5,i] then
     sgRemote.Row:=i;
end;

procedure TfmRemote.btnRemoteCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TfmRemote.sgRemoteSelection(Sender: TObject; aCol, aRow: Integer);
begin
  sgRemote.Options:=sgRemote.Options+[goRowSelect];
  btnRemoteLoad.Tag:=aRow;
  btnRemoteLoad.Enabled:=true;
end;

procedure TfmRemote.btnRemoteLoadClick(Sender: TObject);
begin
  Hide;
  fmRemote.Enabled:=false;
  try
    Poll.Init(TCPClient.Players[TCPClient.PlayerData.PlayerName].Mates);
    TCPClient.OnSend('nwPoll','group',
                  'Content='+TCPClient.PlayerData.PlayerName+nwDelimiter+
                  'PollContent='+inttostr(integer(pcResume)));
    if Poll.Waitfor(true) then
    begin
      TCPClient.OnSend('nwLoadGame','group','Name='+sgRemote.Cells[5,btnRemoteLoad.Tag]);
    end;
  finally
    fmRemote.Enabled:=true;
  end;
end;

procedure TfmRemote.sgRemoteCompareCells(Sender: TObject; ACol, ARow, BCol, BRow: Integer; var Result: integer);
begin
  with sgRemote do
   case aCol of
     0,4 : Result:=sgRemote.Tag*CompareValue(StrToInt(Cells[aCol,aRow]),StrToInt(Cells[bCol,bRow]));
     1,2 : Result:=sgRemote.Tag*CompareDateTime(StrToDateTime(Cells[aCol,aRow]),StrToDateTime(Cells[bCol,bRow]));
       3 : Result:=sgRemote.Tag*CompareText(Cells[aCol,aRow],Cells[bCol,bRow]);
   end; //case;
end;

procedure TfmRemote.sgRemoteDblClick(Sender: TObject);
begin
  btnRemoteLoadClick(self);
end;

procedure TfmRemote.sgRemoteDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  with sgRemote.Canvas do
  begin
    if (aRow=0) then
    begin
      if ThemeServices.ThemesEnabled then
      begin
        ThemeServices.DrawElement(Handle,ThemeServices.GetElementDetails(thHeaderDontCare),aRect, nil);
        if aCol<5 then
          TextRect(aRect, aRect.Left+3, aRect.Top+1, sgRemote.Columns[aCol].Title.Caption);
      end else
      begin
        Brush.Style:=bsSolid;
        Brush.Color:=clBtnFace;
        FillRect(aRect);
        Frame3d(aRect,1,bvRaised);
        Font.Color:=clWindowText;
        InflateRect(aRect,1,1);
        TextRect(aRect, aRect.Left+5, aRect.Top+1,sgRemote.Columns[aCol].Title.Caption);
      end;
    end else
    begin
      if (gdSelected in aState) and (aCol>0) then
      begin
        Brush.Color:=clHighlight;
        Font.Color:=clHighlightText;
      end else
      begin
        Brush.Color:=clWindow;
        Font.Color:=clWindowText;
      end;
      Brush.Style:=bsSolid;
      FillRect(aRect);
      if (aCol=0) then
      begin
        if sgRemote.Cells[aCol,aRow]='1' then
          sgRemote.Canvas.Draw(aRect.Left,aRect.Top,imEnd.Picture.Bitmap) else
        if sgRemote.Cells[aCol,aRow]='2' then
          sgRemote.Canvas.Draw(aRect.Left,aRect.Top,imArrow.Picture.Bitmap);
      end else
      begin
        InflateRect(aRect,-5,0);
//        if aCol<5 then //6=filename
          DrawText(sgRemote.Canvas.Handle,
                   PChar(sgRemote.Cells[aCol,aRow]),
                   length(sgRemote.Cells[aCol,aRow]),
                   aRect,
                   DT_LEFT or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS);// or DT_MODIFYSTRING);
      end;
    end;
  end;
end;

procedure TfmRemote.sgRemoteHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  sgRemote.SortColRow(IsColumn,Index);
  sgRemote.Tag:=sgRemote.Tag*-1;
end;

initialization
  {$I uremotegames.lrs}

end.

