{ Scrabble pieces

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

unit upieces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, Forms,
  utypes,       //AbstractScrabbleDragObject
  uletter,      //TLetter
  ugameoptions; //Color

type

  { TScrabbleDragObject }

  TScrabbleDragObject=class(TAbstractScrabbleDragObject)
    private
      FDragImages : TDragImageList;
      FData       : TLetter;
      FCurrentCursor : string;
    protected
      function GetData:TLetter;override;
      function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    public
      constructor Create(Sender:TControl; const aData:TLetter); reintroduce;
      destructor Destroy; override;
      function GetDragImages: TDragImageList; override;
      property Data:TLetter read GetData;
    end; //TScrabbleDragObject

  { TPiece }

  { TPieceList }

  TPieceList = class(TList)
    private
      FParent : TCustomControl;
    public
      constructor Create(aParent:TCustomControl);
      destructor Destroy; override;
      procedure Resize(Sender: TObject);
    end;

  TPiece=class(TCustomControl)
    private
      FPieceList          : TPieceList;          //used for resizing
      Fx,Fy               : integer;             //minimal distance of cursorpos at dragover
      FData               : TLetter;             //content for painting
      procedure DoStartDrag(Sender: TObject; var DragObject: TDragObject); reintroduce;
      procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); reintroduce;
      procedure DoEndDrag(Sender, Target: TObject; X, Y: Integer); reintroduce;
      procedure DoPaint(Sender: TObject);
    protected
      procedure PaintTo(aCanvas:TCanvas;aRect:TRect;aLetter:TLetter;IgnoreDragging:boolean=false); reintroduce; //used for DragImage
    public
      constructor Create(aParent:TComponent); override;
      destructor Destroy; override;
      property PieceList : TPieceList read FPieceList write FPieceList;
      property Data      : TLetter read FData write FData;
      property OnMouseDown;
    end; //TPiece

    procedure CalcFontWidth(aCanvas:TCanvas; aRect:TRect; var LetterSize,NumberSize:byte; var PieceWidth:integer);

const
  PieceSize=40;
var
  PieceWidth: integer;

implementation

var
  NumberSize, LetterSize: byte;

{ TScrabbleDragObject }

constructor TScrabbleDragObject.Create(Sender: TControl; const aData:TLetter);
const
  cSize=20;
  cRect:TRect=(Left:0; Top:0; Right:cSize; Bottom:cSize);
var
  bmp : TBitmap;
begin
  inherited Create(Sender);
  FData:=aData;

  FDragImages:=TDragImageList.Create(nil);
  bmp:=TBitmap.Create;
  try
    bmp.Width:=cSize;
    bmp.Height:=cSize;
    (Control as TPiece).PaintTo(bmp.Canvas,cRect,FData,true);
    FDragImages.Width:=cSize;
    FDragImages.Height:=cSize;
    {i:=}FDragImages.AddMasked(bmp, clNone);
  finally
    bmp.Free
  end;
  if FDragImages.Count>0 then
    FDragImages.SetDragImage(0, cSize, cSize);//Control.Width div 2, Control.Height div 2);//-5,-10
end;

destructor TScrabbleDragObject.Destroy;
begin
  FDragImages.Clear;
  FDragImages.Free;
  inherited Destroy;
end;

function TScrabbleDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
const CursorName:array[TScrabbleDragCursor] of string=('left','right','exchange','drop','nodrop');
begin
  if Accepted then
  begin
    if FCurrentCursor<>CursorName[FCursor] then
    begin
      Screen.Cursors[crLow]:=LoadCursorFromLazarusResource(CursorName[FCursor]);
      FCurrentCursor:=CursorName[FCursor];
    end;
  end else
  begin
    if FCurrentCursor<>'nodrop' then
    begin
      Screen.Cursors[crLow]:=LoadCursorFromLazarusResource('nodrop');
      FCurrentCursor:='nodrop'
    end;
  end;
  Result:=crLow;
end;

function TScrabbleDragObject.GetData: TLetter;
begin
  Result:=FData;
end;

function TScrabbleDragObject.GetDragImages: TDragImageList;
begin
  Result:=FDragImages;
end;

{ TPiecesList }

procedure CalcFontWidth(aCanvas:TCanvas; aRect:TRect; var LetterSize,NumberSize:byte; var PieceWidth:integer);
begin
  PieceWidth:=aRect.Right-aRect.Left;
  with aCanvas do
  begin
    Font.Name:=fmGameOptions.edFont.Text;

    Font.Size:=8;
    while (Font.Size<100) and
          (TextHeight('ABC')<round(PieceWidth*(fmGameOptions.seLetterSize.Value/100))) do
      Font.Size:=Font.Size+1;
    LetterSize:=Font.Size-1;

    Font.Size:=8;
    while (Font.Size<100) and
          (TextHeight('ABC')<round(PieceWidth*(fmGameOptions.seNumberSize.Value/100))) do
      Font.Size:=Font.Size+1;
    NumberSize:=Font.Size-1;
{
    {$ifdef Debug}
    OnMessage(smDebug,inttostr(PieceWidth)+':'+
                      inttostr(fmGameOptions.seLetterSize.Value)+'='+inttostr(LetterSize)+','+
                      inttostr(fmGameOptions.seNumberSize.Value)+'='+inttostr(NumberSize));
    {$endif}
}  end;//with canvas
end;

constructor TPieceList.Create(aParent:TCustomControl);
begin
  inherited Create;
  FParent:=aParent;
  FParent.OnResize:=@Resize;
end;

destructor TPieceList.Destroy;
var
  i:integer;
  aPiece:TPiece;
begin
  for i:=0 to Count-1 do
  begin
    aPiece:=TPiece(Items[i]);
    aPiece.Free;
    aPiece:=nil;  //FreeAndNil(aPiece);
  end;
  Clear;
  inherited;
end;

procedure TPieceList.Resize(Sender: TObject);
var
  i,w,h,l : integer;
begin
  if FParent.ClientWidth-4>PieceSize then
    w:=FParent.ClientWidth-4 else w:=PieceSize;
  h:=integer(FParent.Parent=Application.Mainform)*20+5;
  l:=FParent.ClientWidth div 2-(PieceSize+2)*Count div 2;
  if l<0 then l:=0;

  for i:=0 to Count-1 do
  with TPiece(Items[i]) do
  begin
    Left:=l+(i mod (w div (PieceSize+2)))*(PieceSize+2)+4;
    Top:=(Parent.Tag)+(i div (w div (PieceSize+2)))*(PieceSize+2)+h;
    {$ifdef LCLQt}Invalidate;{$endif}
  end;
  if Count>0 then
   with TPiece(Items[Count-1]) do
    Parent.Height:=Top+Height+4;
end;

{ TPiece }

constructor TPiece.Create(aParent:TComponent);
begin
  inherited Create(aParent);
  Parent:=aParent as TWinControl;
  Height:=PieceSize;
  Width:=PieceSize;
  OnStartDrag:=@DoStartDrag;
  OnDragOver:=@DoDragOver;
  OnEndDrag:=@DoEndDrag;
  OnPaint:=@DoPaint;
  ControlStyle:=ControlStyle+[csDisplayDragImage];
end;

destructor TPiece.Destroy;
begin
  inherited Destroy;
end;

procedure TPiece.DoPaint(Sender: TObject);
begin
  if (Data<>nil) then //and (Data.State<>lsDragging) then
    PaintTo(Canvas,GetClientRect,Data);
end;

procedure TPiece.DoStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  DragObject:=TScrabbleDragObject.Create(Sender as TControl,self.Data);
end;

procedure TPiece.DoDragOver(Sender, Source: TObject; X, Y: Integer;State: TDragState; var Accept: Boolean);
begin
  if (Source is TAbstractScrabbleDragObject) and
     {$ifdef LCLQt}
       ((Sender as TPiece).Data.State<>lsDragging)
     {$else}
       Visible
     {$endif} then
  begin
    if abs(fx-x)+abs(fy-y)<3 then exit;
    fx:=x;fy:=y;
    Accept:=true;
    if (x<PieceSize div 3) then
      (Source as TAbstractScrabbleDragObject).CursorType:=scInsertLeft else
    if (x>PieceSize-PieceSize div 3) then
      (Source as TAbstractScrabbleDragObject).CursorType:=scInsertRight else
      (Source as TAbstractScrabbleDragObject).CursorType:=scExchange;
    (Source as TScrabbleDragObject).GetDragCursor(Accept,x,y);
  end else Accept:=false;
end;

{$Warning scrabblegrid adjustbymouse is still false if dragging ends here}

procedure TPiece.DoEndDrag(Sender, Target: TObject; X, Y: Integer);
var i:integer;
begin
  Screen.Cursors[crLow]:=crDefault;
  if Target is TPiece then
  begin
    with (Target as TPiece).PieceList do
    try
      if (x<PieceSize div 3) then           //insert left
      begin
        if IndexOf((Sender as TPiece))>IndexOf((Target as TPiece)) then //target was left from sender
         for i:=IndexOf((Sender as TPiece)) downto IndexOf((Target as TPiece))+1 do
          Exchange(i,i-1)
        else                                                            //target was right from sender
         for i:=IndexOf((Sender as TPiece)) to IndexOf((Target as TPiece))-2 do
          Exchange(i,i+1);
      end else
      if (x>PieceSize-PieceSize div 3) then //insert right
      begin
        if IndexOf((Sender as TPiece))>IndexOf((Target as TPiece)) then
         for i:=IndexOf((Sender as TPiece)) downto IndexOf((Target as TPiece))+2 do
          Exchange(i,i-1)
        else
         for i:=IndexOf((Sender as TPiece)) to IndexOf((Target as TPiece))-1 do
          Exchange(i,i+1);
      end else                              //exchange
        Exchange(IndexOf(Sender as TPiece),IndexOf(Target as TPiece));
    finally
      (Sender as TPiece).Data.State:=lsRack;
      (Sender as TPiece).Visible:=true;
      (Target as TPiece).Data.State:=lsRack;
      (Target as TPiece).Visible:=true;
      (Sender as TPiece).PieceList.Resize(nil);
    end; //with Pieces
  end;
  if (Target=nil) and (Sender is TPiece) then //no change if target is grid
  with (Sender as TPiece) do
  begin
    Data.State:=lsRack;
    Visible:=true;
    {$ifdef LCLQt}Invalidate;{$endif}
  end;
end;

procedure TPiece.PaintTo(aCanvas: TCanvas;aRect:TRect;aLetter:TLetter;IgnoreDragging:boolean);
const LeftShift=3;
var s   : string;
begin
  if (aLetter=nil) or ((aLetter.State=lsDragging) and not IgnoreDragging) then
  begin
    aCanvas.Brush.Color:=clBtnFace;
    aCanvas.FillRect(aRect);
  end else
  with aCanvas do
  begin
    Brush.Color:=Color;
    FillRect(aRect);

    //framed (Frame3d seems to be buggy)
    Pen.Width:=1;
    Pen.Color:=clSilver;
    Line(aRect.Left,aRect.Top,aRect.Right,aRect.Top);
    Line(aRect.Left,aRect.Top,aRect.Left,aRect.Bottom);
    Pen.Color:=clBlack;
    Line(aRect.Right-1,aRect.Top,aRect.Right-1,aRect.Bottom);
    Line(aRect.Left,aRect.Bottom-1,aRect.Right,aRect.Bottom-1);

    if (aRect.Bottom-aRect.Top)<>PieceWidth then
      CalcFontWidth(aCanvas,aRect,LetterSize,NumberSize,PieceWidth);
    //text
    if Caption<>'' then //Scrabble.Paused
    begin
      s:=Caption;
      if not aLetter.IsJoker then
      begin
        Font.Name:=fmGameOptions.edFont.Text;
        Font.Color:=clBlack;
        Brush.Style:=bsClear;
        Font.Size:=LetterSize;
        TextOut(aRect.Left+((aRect.Right-aRect.Left) div 2-TextWidth(s) div 2)-LeftShift,
                aRect.Top+((aRect.Bottom-aRect.Top) div 2-TextHeight(s) div 2),s);
        Font.Size:=NumberSize;
        s:=IntToStr(aLetter.Value);
        TextOut(aRect.Right-TextWidth(s)-3, aRect.Bottom-TextHeight(s)-1, s);
      end else
        StretchDraw(aRect,fmGameOptions.imJoker.Picture.Bitmap); //Joker
      //Random letter
      if aLetter.IsRandom then
        StretchDraw(aRect,fmGameOptions.imRandom.Picture.Bitmap);
    end;
  end;//with Canvas
end;

initialization

{$I cursors.res}

end.
