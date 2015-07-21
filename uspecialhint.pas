{ Rich hint with bitmap

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

unit uspecialhint;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, LCLType;

type

  { TSpecialHintWindow }

  TSpecialHintWindow = class(TCustomForm)
    private
      FBitmap: TBitmap;
      procedure DoClose(Sender: TObject; var CloseAction: TCloseAction); reintroduce;
      procedure DoDeactivate(Sender: TObject);
      procedure DoMouseLeave(Sender: TObject);
      procedure DoPaint(Sender: TObject);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy;override;
      property Bitmap:TBitmap read FBitmap write FBitmap;
    end;

implementation

{ TSpecialHintWindow }

procedure TSpecialHintWindow.DoDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TSpecialHintWindow.DoMouseLeave(Sender: TObject);
begin
  Close;
end;

procedure TSpecialHintWindow.DoPaint(Sender: TObject);
begin
  Canvas.Draw(0,0,FBitmap);
end;

procedure TSpecialHintWindow.DoClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

constructor TSpecialHintWindow.Create(AOwner: TComponent);
begin
  inherited CreateNew(aOwner);
  fCompStyle:=csHintWindow;
  Parent:=nil;
  Color:=clInfoBk;
  Canvas.Font:=Screen.HintFont;
  Canvas.Brush.Style:=bsClear;
  BorderStyle:=bsNone;
  Caption := 'Scrabble3D';
  ShowInTaskBar:=stNever;
  FormStyle:=fsStayOnTop;
  BringToFront;

  FBitmap:=TBitmap.Create;
  //max size, shrink before paint
  with FBitmap do
  begin
    Width:=300;
    Height:=300;
    Canvas.Brush.Color:=clInfoBk;
    Canvas.Brush.Style:=bsSolid;
    Canvas.Font.Color:=clInfoText;
    Canvas.FillRect(0,0,Width,Height);
  end;

  OnClose:=@DoClose;
  OnPaint:=@DoPaint;
  OnMouseLeave:=@DoMouseLeave;
  {$ifdef WINDOWS}
  OnDeactivate:=@DoDeactivate;
  {$endif}
end;

destructor TSpecialHintWindow.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

end.

