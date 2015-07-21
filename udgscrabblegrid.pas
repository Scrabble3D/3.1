{ 2D view of the game based on TDrawGrid

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

unit udgscrabblegrid;

{$mode objfpc}{$H+}

{$I conditions.inc}


interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, Grids,
  LResources, LCLIntf, LCLType, LCLProc, Math,

  utypes, uscrabblegrid,
  uletter, uscrabble
  ;

type

  { TDgScrabbleGrid }

  TDgScrabbleGrid = class(TScrabbleGrid)
  private //draw
    FFps,FFrameCount,FLastFrame:longword;
    GridView: TCustomDrawGrid;
    procedure DoDrawCell(Sender: TObject; aCol, aRow: Integer;aRect: TRect; aState: TGridDrawState);
    procedure PaintCoords(aRect: TRect; aCol,aRow: integer);
    procedure PaintBonus(aRect: TRect; aValue:string);
    procedure PaintMarkers(aRect: TRect; aCol, aRow: integer);
  protected //abstract
    FSelectedCell: TPoint;
    function GetSelectedCell: TPoint; override;
    function GetFps: integer; override;
    function MouseToCell(x, y: integer; var ax,ay,az: byte):boolean; override;
    procedure SetBoardSize(AValue: byte); override;
  public
    constructor Create(aOwner: TComponent);override;
    destructor Destroy; override;
    procedure Paint(Sender:TObject); override;
    procedure DoResize(Sender: TObject);
    function ScreenShot:TBitmap; override;
  end;

implementation

procedure TDgScrabbleGrid.DoDrawCell(Sender: TObject; aCol, aRow: Integer;aRect: TRect; aState: TGridDrawState);
var
  aLetter: TLetter;
  i,x,y,z: byte;
  a:single;
  aFieldType:byte;
begin
  if not (gsDestroying in Scrabble.GameState) and
    (BoardSize>0) then
  begin
    //outbox
    if (aCol=0) or (aRow=0) or
       (aCol>BoardSize) or (aRow>BoardSize) then
       PaintCoords(aRect,aCol,aRow) else
    //inbox
    begin
      if BiDiMode=bdLeftToRight then
        i:=aCol-1 else
        i:=BoardSize-aCol;
      Scrabble.Convert2DTo3D(i,aRow-1,x,y,z);
      aLetter:=OnGetFieldLetter(x,y,z);

      with GridView.Canvas do
      begin
        Brush.Color:=OnGetFieldColor(x,y,z,aLetter,a,aFieldType);
        Brush.Style:=bsSolid;
        FillRect(aRect);
        //"gridline"
        Pen.Width:=1; Pen.Color:=clBlack;
        Line(aRect.Left,aRect.Top,aRect.Right,aRect.Top);
        Line(aRect.Left,aRect.Top,aRect.Left,aRect.Bottom);
        Pen.Color:=clSilver; Line(aRect.Right-1,aRect.Top,aRect.Right-1,aRect.Bottom);
        Line(aRect.Left,aRect.Bottom-1,aRect.Right,aRect.Bottom-1);
        if aLetter=nil then
        begin
          //bonus field description
          if ShowBonusText and assigned(OnGetBonusText) then
            PaintBonus(aRect,OnGetBonusText(x,y,z));
          //bonus field markers
          if ShowBonusMarkers and (aFieldType=0) then
             PaintMarkers(aRect,aCol,aRow)
        end else //Letter=nil
        begin
          GridView.Canvas.Frame3D(aRect,clSilver,clBlack,1);

          if assigned(OnDrawLetter) then
            OnDrawLetter(aLetter,GridView.Canvas);
        end;//Letter=nil
      end;//with Canvas
    end;//acol,arow>0
  end else
    GridView.Canvas.FillRect(GridView.ClientRect); //Scrabble.BoardSize>0
  if (aCol=GridView.ColCount-1) and (aRow=GridView.RowCount-1) then
  begin
    inc(FFrameCount);
    if (GetTickCount-FLastFrame>=1000) then
    begin
      Ffps:=FFrameCount;
      FLastFrame:=GetTickCount;
      FFrameCount:=0;
    end;
  end;
end;

procedure TDgScrabbleGrid.PaintMarkers(aRect: TRect; aCol, aRow: integer);
var
  x,y,z,SizeOfBonusMarker : byte;
  p: array of TPoint;
  i: integer;
  aColor: TColor;
  a: single;
  aFieldType: byte;
begin
  if GridView.DefaultColWidth>200 then exit;
  if not assigned(OnGetFieldColor) then exit;

  SizeOfBonusMarker:=12-GridView.DefaultColWidth div 20;
  with GridView do
  try
    setlength(p,3);
    for i:=0 to 3 do
    begin
      case i of
       0 : if aCol>1 then
           begin
             Scrabble.Convert2DTo3D(aCol-2,aRow-1,x,y,z);
             aColor:=OnGetFieldColor(x,y,z,nil,a,aFieldType);
             if BiDiMode=bdLeftToRight then
             begin
               p[0].X:=aRect.Left+1; p[0].Y:=aRect.Bottom-(DefaultColWidth div 2)-(DefaultColWidth div SizeOfBonusMarker);
               p[1].X:=aRect.Left+1+(DefaultColWidth div SizeOfBonusMarker); p[1].Y:=aRect.Bottom-(DefaultColWidth div 2);
               p[2].X:=aRect.Left+1; p[2].Y:=aRect.Bottom-(DefaultColWidth div 2)+(DefaultColWidth div SizeOfBonusMarker);
             end else
             begin
               p[0].X:=aRect.Right-2; p[0].Y:=aRect.Bottom-(DefaultColWidth div 2)-(DefaultColWidth div SizeOfBonusMarker);
               p[1].X:=aRect.Right-2-(DefaultColWidth div SizeOfBonusMarker); p[1].Y:=aRect.Bottom-(DefaultColWidth div 2);
               p[2].X:=aRect.Right-2; p[2].Y:=aRect.Bottom-(DefaultColWidth div 2)+(DefaultColWidth div SizeOfBonusMarker);
             end;
           end else continue;
       1 : if aRow>1 then
           begin
             Scrabble.Convert2DTo3D(aCol-1,aRow-2,x,y,z);
             aColor:=OnGetFieldColor(x,y,z,nil,a,aFieldType);
             p[0].X:=aRect.Left+(DefaultColWidth div 2)-(DefaultColWidth div SizeOfBonusMarker); p[0].Y:=aRect.Top+1;
             p[1].X:=aRect.Left+(DefaultColWidth div 2); p[1].Y:=aRect.Top+1+(DefaultColWidth div SizeOfBonusMarker);
             p[2].X:=aRect.Left+(DefaultColWidth div 2)+(DefaultColWidth div SizeOfBonusMarker); p[2].Y:=aRect.Top+1;
           end else continue;
       2 : if aCol<BoardSize then
           begin
             Scrabble.Convert2DTo3D(aCol,aRow-1,x,y,z);
             aColor:=OnGetFieldColor(x,y,z,nil,a,aFieldType);
             if BiDiMode=bdLeftToRight then
             begin
               p[0].X:=aRect.Right-2; p[0].Y:=aRect.Bottom-(DefaultColWidth div 2)-(DefaultColWidth div SizeOfBonusMarker);
               p[1].X:=aRect.Right-2-(DefaultColWidth div SizeOfBonusMarker); p[1].Y:=aRect.Bottom-(DefaultColWidth div 2);
               p[2].X:=aRect.Right-2; p[2].Y:=aRect.Bottom-(DefaultColWidth div 2)+(DefaultColWidth div SizeOfBonusMarker);
             end else
             begin
               p[0].X:=aRect.Left+1; p[0].Y:=aRect.Bottom-(DefaultColWidth div 2)-(DefaultColWidth div SizeOfBonusMarker);
               p[1].X:=aRect.Left+1+(DefaultColWidth div SizeOfBonusMarker); p[1].Y:=aRect.Bottom-(DefaultColWidth div 2);
               p[2].X:=aRect.Left+1; p[2].Y:=aRect.Bottom-(DefaultColWidth div 2)+(DefaultColWidth div SizeOfBonusMarker);
             end;
           end else continue;
       3 : if aRow<BoardSize then
           begin
             Scrabble.Convert2DTo3D(aCol-1,aRow,x,y,z);
             aColor:=OnGetFieldColor(x,y,z,nil,a,aFieldType);
             p[0].X:=aRect.Left+(DefaultColWidth div 2)-(DefaultColWidth div SizeOfBonusMarker); p[0].Y:=aRect.Bottom-2;
             p[1].X:=aRect.Left+(DefaultColWidth div 2); p[1].Y:=aRect.Bottom-2-(DefaultColWidth div SizeOfBonusMarker);
             p[2].X:=aRect.Left+(DefaultColWidth div 2)+(DefaultColWidth div SizeOfBonusMarker); p[2].Y:=aRect.Bottom-2;
           end else continue;
      end;//case
      Canvas.Pen.Color:=aColor;//fmGameOptions.Color[ft];
      Canvas.Brush.Color:=aColor;//fmGameOptions.Color[ft];
      if aFieldType>0 then
        Canvas.Polygon(p);
    end;//for to
  finally
    setlength(p,0);
  end;// try
end;

function TDgScrabbleGrid.MouseToCell(x, y: integer; var ax, ay, az: byte): boolean;
var
  z:byte;
begin
  GridView.MouseToCell(x,y,FSelectedCell.x,FSelectedCell.y);
  if (FSelectedCell.x<=BoardSize) and (FSelectedCell.y<=BoardSize) then
  begin
    if (FSelectedCell.x>0) and (FSelectedCell.y>0) then
    begin
      if BiDiMode=bdLeftToRight then
        z:=FSelectedCell.x-1 else
        z:=BoardSize-FSelectedCell.x;
      Scrabble.Convert2DTo3D(z,FSelectedCell.y-1,ax,ay,az);
    end;
    Result:=true;
  end else
    Result:=false;
end;

procedure TDgScrabbleGrid.SetBoardSize(AValue: byte);
begin
  inherited;
  GridView.ColCount:=BoardSize+2; //additional cells around board
  GridView.RowCount:=BoardSize+2;
  DoResize(self); //adjust cell size
end;

function TDgScrabbleGrid.GetFps: integer;
begin
  Result:=FFps;
end;

function TDgScrabbleGrid.GetSelectedCell: TPoint;
begin
  Result:=FSelectedCell;
end;

procedure TDgScrabbleGrid.PaintBonus(aRect: TRect; aValue:string);
var
  i:integer;
  r:TRect;
begin
  with GridView do
  begin
    r:=aRect;
    //font size
    Font.Color:=LightOrDark(Brush.Color);
    Font.Name:='Arial';
    Font.Size:=13;
    repeat
      Font.Size:=Font.Size-1;
      i:=DrawText(Canvas.Handle, PChar(aValue), -1, r, DT_CALCRECT or DT_WORDBREAK or DT_CENTER);
    until (i<=DefaultColWidth) or (Font.Size<6);
    //small font size
    if Font.Size<6 then
    begin
      Font.Name:='Small Fonts';
      Font.Size:=1;
      repeat
        Font.Size:=Font.Size+1;
        i:=DrawText(Canvas.Handle, PChar(aValue), -1, r, DT_CALCRECT or DT_WORDBREAK or DT_CENTER);
      until i>=DefaultColWidth;
      Font.Size:=Font.Size-1;
    end;
//      i:=DrawText(Canvas.Handle, PChar(aValue), -1, r, DT_CALCRECT or DT_WORDBREAK or DT_CENTER);
    i:=(aRect.Bottom-aRect.Top)-(r.Bottom-r.Top); aRect.Top:=aRect.Top+abs(i div 2);
    aRect.Bottom:=aRect.Bottom-1;
    DrawText(Canvas.Handle, PChar(aValue), -1, aRect, DT_WORDBREAK or DT_CENTER);
  end; //with Canvas
end;

procedure TDgScrabbleGrid.PaintCoords(aRect: TRect; aCol,aRow: integer);
var s:string;
begin
  with GridView.Canvas do
  begin
    //clear rect
    Brush.Style:=bsSolid;
//    Brush.Color:=fmGameOptions.cbBackGroundColor.ButtonColor;
    FillRect(aRect);
    //3D topleft
    Font.Name:='Arial';
    Font.Size:=8;
//    Font.Color:=LightOrDark(fmGameOptions.cbBackGroundColor.ButtonColor);
    if (aCol=0) and (aRow=0) and (Scrabble.Dimension=D3) then
    begin
      case Scrabble.ActiveDimension.Axis of
       dx : s:='z='+Scrabble.PosToString(dz,Scrabble.ActiveDimension.Position,not UseGreekLetter);
       dy : s:='y='+Scrabble.PosToString(dy,Scrabble.ActiveDimension.Position,not UseGreekLetter);
       dz : s:='x='+Scrabble.PosToString(dx,Scrabble.ActiveDimension.Position,not UseGreekLetter);
      end;
      TextOut(aRect.Left+((aRect.Right-aRect.Left) div 2-TextWidth(s) div 2),
              aRect.Top+((aRect.Bottom-aRect.Top) div 2-TextHeight(s) div 2), s);
    end;
    //coords
    if (aCol=0) and (aRow>0) and (aRow<GridView.RowCount-1) then
    begin
      case Scrabble.ActiveDimension.Axis of
       dx,dz : s:=Scrabble.PosToString(dy,aRow-1,not UseGreekLetter);
       dy : s:=Scrabble.PosToString(dz,aRow-1,not UseGreekLetter);
      end;
      TextOut(aRect.Right-TextWidth(s)-4,
              aRect.Top+((aRect.Bottom-aRect.Top) div 2-TextHeight(s) div 2), s);
    end;
    if (aRow=0) and (aCol>0) and (aCol<GridView.ColCount-1) then
    begin
      case Scrabble.ActiveDimension.Axis of
       dx,dy : {if fmGameOptions.rbRightToLeft.Checked then
                 s:=Scrabble.PosToString(dx,Scrabble.BoardSize-aCol,fmGameOptions.cbRoman.Checked) else}
                 s:=Scrabble.PosToString(dx,aCol-1,not UseGreekLetter);
       dz    : s:=Scrabble.PosToString(dz,aCol-1,not UseGreekLetter);
      end;
      TextOut(aRect.Left+((aRect.Right-aRect.Left) div 2-TextWidth(s) div 2),
              aRect.Bottom-TextHeight(s)-2, s);
    end;
  end;//with Canvas
end;

procedure TDgScrabbleGrid.DoResize(Sender: TObject);
begin
//  if not Application.Terminated then
  with GridView do
  begin
    DefaultColWidth:=Min(Parent.Width-5,Parent.Height-30) div ColCount;
    DefaultRowHeight:=DefaultColWidth;
    if BoardSize>0 then
    begin
      Width:=DefaultColWidth*(BoardSize+2);
      Height:=DefaultRowHeight*(BoardSize+2);
    end else
    begin
      Width:=DefaultColWidth;
      Height:=DefaultRowHeight;
    end;
    Left:=(Parent.Width-Width) div 2;
    Top:=22+(Parent.Height-22-Height) div 2;
    LeftCol:=0;
    TopRow:=0;
  end;
end;

function TDgScrabbleGrid.ScreenShot: TBitmap;
begin
  Result:=TBitmap.Create;
  Result.Width:=GridView.Width;
  Result.Height:=GridView.Height;
  GridView.Canvas.Lock;
  try
    GridView.Repaint;
    Result.Canvas.CopyRect(Rect(0,0,Result.Width,Result.Height),
                           GridView.Canvas,
                           GridView.ClientRect);
  finally
    GridView.Canvas.UnLock;
  end;
end;

constructor TDgScrabbleGrid.Create(aOwner: TComponent);
begin
  inherited;// Create(aOwner);
  GridView:=TCustomDrawGrid.Create(aOwner);
  with GridView do
  begin
    Parent:=aOwner as TWinControl;
    BorderStyle:=bsNone;
    ColCount:=1;  RowCount:=1;
    DefaultDrawing:=false;
    FixedCols:=0; FixedRows:=0;
    Flat:=true;
    FocusRectVisible:=false;
    Options:=[];//[goVertLine,goHorzLine];
    ScrollBars:=ssNone;
    ShowHint:=true;
    Align:=alNone;//alClient;
    DoubleBuffered:=true;
    ControlStyle:=ControlStyle+[csDisplayDragImage];
    OnDrawCell:=@DoDrawCell;
    OnDragDrop:=@DoDragDrop;
    OnDragOver:=@DoDragOver;
    OnMouseDown:=@DoMouseDown;
    OnShowHint:=@DoShowHint;
  end;
  DoResize(self);
end;

destructor TDgScrabbleGrid.Destroy;
begin
  GridView.Clear; //exception on Darwin if omitted
  GridView.Free;
  inherited Destroy;
end;

procedure TDgScrabbleGrid.Paint(Sender:TObject);
begin
  GridView.BiDiMode:=self.BiDiMode;
  GridView.Repaint;
end;

end.

