{ Abstract scrabblegrid functions

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

unit uscrabblegrid;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics,
  uscrabble, uboard, uletter, utypes;

type

  TOnDrawLetter=procedure(aLetter:TLetter;aCanvas:TCanvas) of object;
  TOnGetFieldColor=function(x,y,z:byte; aLetter:TLetter; var a:single; var FieldType:byte):TColor of object;
  TOnGetFieldLetter=function(x,y,z:byte):TLetter of object;
  TOnAskForJoker=function:WideChar of object;
  TOnGetHint=function(const x,y,z:byte):string of object;
  TOnGetBonusText=function(const x,y,z:byte):string of object;

  { TScrabbleGrid }

  TScrabbleGrid = class(TWinControl)
  private
    FOnAskForJoker: TOnAskForJoker;
    FOnDrawLetter: TOnDrawLetter;
    FOnGetFieldColor: TOnGetFieldColor;
    FOnGetFieldLetter: TOnGetFieldLetter;
    FOnGetHint: TOnGetHint;
    FOnGetBonusText: TOnGetBonusText;

    FBoardSize: byte;
    FBSHalf: single;
    FShowBonusMarkers: boolean;
    FShowBonusText: boolean;

    FUseGreekLetter: boolean;
    FBiDiMode: TBiDiMode;

    procedure SetShowBonusMarkers(AValue: boolean);
    procedure SetShowBonusText(AValue: boolean);
    procedure SetUseGreekLetter(AValue: boolean);
  protected
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure SetBiDiMode(AValue: TBiDiMode);override;
    property BSHalf : single read FBSHalf;
  protected //abstract
    procedure SetBoardSize(AValue: byte); virtual;
    function MouseToCell(x, y: integer; var ax,ay,az: byte):boolean; virtual; abstract;
    function GetSelectedCell: TPoint; virtual; abstract;
    function GetFps: integer;  virtual; abstract;
  public //abstract
    procedure Paint(Sender:TObject); virtual; abstract;
    function ScreenShot:TBitmap; virtual; abstract;
  public
    property OnGetFieldColor:TOnGetFieldColor read FOnGetFieldColor write FOnGetFieldColor;
    property OnGetFieldLetter:TOnGetFieldLetter read FOnGetFieldLetter write FOnGetFieldLetter;
    property OnAskForJoker:TOnAskForJoker read FOnAskForJoker write FOnAskForJoker;
    property OnDrawLetter:TOnDrawLetter read FOnDrawLetter write FOnDrawLetter;
    property OnGetHint:TOnGetHint read FOnGetHint write FOnGetHint;
    property OnGetBonusText:TOnGetBonusText read FOnGetBonusText write FOnGetBonusText;

    property BoardSize:byte read FBoardSize write SetBoardSize;
    property UseGreekLetter:boolean read FUseGreekLetter write SetUseGreekLetter;
    property BiDiMode:TBiDiMode read FBiDiMode write SetBiDiMode;
    property Fps:integer read GetFps;
    property SelectedCell:TPoint read GetSelectedCell;
    property ShowBonusText:boolean read FShowBonusText write SetShowBonusText;
    property ShowBonusMarkers:boolean read FShowBonusMarkers write SetShowBonusMarkers;
  end;

implementation

{ TScrabbleGrid }

procedure TScrabbleGrid.SetBiDiMode(AValue: TBiDiMode);
begin
  if aValue<>FBiDiMode then
  begin
    FBiDiMode:=aValue;
    Paint(self);
  end;
end;

procedure TScrabbleGrid.SetBoardSize(AValue: byte);
begin
  if aValue<>FBoardSize then
  begin
    FBoardSize:=aValue;
    FBSHalf:=FBoardSize/2;
    Paint(self);
  end;
end;

procedure TScrabbleGrid.SetUseGreekLetter(AValue: boolean);
begin
  if aValue<>FUseGreekLetter then
  begin
    FUseGreekLetter:=aValue;
    Paint(self);
  end;
end;

procedure TScrabbleGrid.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  ax,ay,az : byte;
  aLetter  : TLetter;
begin
  if (Source is TAbstractScrabbleDragObject) and
     MouseToCell(x,y,ax,ay,az) then
  begin
    aLetter:=TLetter((Source as TAbstractScrabbleDragObject).Data);
    if aLetter.IsJoker and (aLetter.What=ltJoker) and assigned(FOnAskForJoker) then
      aLetter.What:=FOnAskForJoker();
    Scrabble.PlaceLetterAt(aLetter,ax,ay,az);
  end;
end;

procedure TScrabbleGrid.DoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
//const
//  MinMousePosDiff=5;
var
  ax,ay,az : byte;
  aLetter  : TLetter;
begin
  {$IFDEF workaround_mousepos}
  if abs(x-fx)+abs(y-fy)<MinMousePosDiff then
    exit;
  fx:=x;fy:=y;
  {$ENDIF}
  if (Source is TAbstractScrabbleDragObject) and
     (gsRunning in Scrabble.GameState) and
     (Scrabble.LocalPlayer=Scrabble.CurrentPlayer) and
     not Scrabble.IsTimeout and
     not (gsGameEnd in Scrabble.GameState) and
     not (gsNextPlayer in Scrabble.GameState) and
     not (gsKibitz in Scrabble.GameState) and
     (MouseToCell(x,y,ax,ay,az)) and
     (SelectedCell.X>0) and
     (SelectedCell.Y>0) then
  begin
    aLetter:=Scrabble.BoardLetter[ax,ay,az];
    Accept:=((aLetter=nil) and
             (Scrabble.CurrentPlayer=Scrabble.LocalPlayer)
            ) or
           (aLetter.IsJoker and (gsJokerExchange in Scrabble.GameState) and
            (aLetter.State=lsPlaced) and
            (aLetter.What=TLetter((Source as TAbstractScrabbleDragObject).Data).What) and
            ((Scrabble.MoveState=msNone) or (Scrabble.MoveState=msJokerExchanged))
           );
  end else
    Accept:=false;
  (Source as TAbstractScrabbleDragObject).CursorType:=scDrop;
end;

procedure TScrabbleGrid.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ax,ay,az:byte;
  aLetter:TLetter;
  aDim:TActiveDimension;
begin
  if (csDestroying in ComponentState) or
     (gsNextPlayer in Scrabble.GameState) then
    exit;

  if (ssCtrl in Shift) and (Button=mbRight) then
  begin
    Scrabble.RestoreBoard;
    exit;
  end;

  if MouseToCell(x,y,ax,ay,az) then
  begin
    if (SelectedCell.X>0) and (SelectedCell.Y>0) then
    begin
      aLetter:=Scrabble.BoardLetter[ax,ay,az];
      if (aLetter<>nil) and (aLetter.State=lsBoard) then
      begin
        Scrabble.RemoveLetterFrom(ax,ay,az,boolean(Button=mbRight));  //begindrag
        Paint(self);
      end;
    end else //x&y > 0
    begin
      //legend clicked?
      if (Scrabble.Dimension=D3) and
         ((SelectedCell.X=0) or (SelectedCell.Y=0)) then
      begin
        aDim:=Scrabble.ActiveDimension;
        //0,0: toggle through dim's
        if (SelectedCell.X=0) and (SelectedCell.Y=0) then
          aDim.Axis:=TDimension((integer(aDim.Axis)+1) mod 3) else
        //change x
        if (SelectedCell.X=0) and (SelectedCell.Y<=FBoardSize) then
        begin
          case aDim.Axis of
           dx:aDim.Axis:=dy;
           dy:aDim.Axis:=dx;
           dz:aDim.Axis:=dy;
          end;
          aDim.Position:=SelectedCell.Y-1;
        end else
        //change y
        if (SelectedCell.Y=0) and (SelectedCell.X<=FBoardSize) then
        begin
          case aDim.Axis of
           dx:aDim.Axis:=dz;
           dy:aDim.Axis:=dz;
           dz:aDim.Axis:=dx;
          end;
          aDim.Position:=SelectedCell.X-1;
        end;
        Scrabble.ActiveDimension:=aDim;
      end;
    end; //x|y=0
  end;
end;

procedure TScrabbleGrid.DoShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  ax,ay,az: byte;
begin
  if MouseToCell(HintInfo^.CursorPos.X,HintInfo^.CursorPos.Y,ax,ay,az) and
    (SelectedCell.X>0) and (SelectedCell.Y>0) and
    (assigned(FOnGetHint)) then
    HintInfo^.HintStr:=FOnGetHint(ax,ay,az) else
    HintInfo^.HintStr:='';
end;

procedure TScrabbleGrid.SetShowBonusText(AValue: boolean);
begin
  if FShowBonusText<>aValue then
  begin
    FShowBonusText:=aValue;
    Paint(self);
  end;
end;

procedure TScrabbleGrid.SetShowBonusMarkers(AValue: boolean);
begin
  if FShowBonusMarkers<>aValue then
  begin
    FShowBonusMarkers:=aValue;
    Paint(self);
  end;
end;

end.

