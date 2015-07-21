{ Scrabble core data
  TLetter: a single piece with position, state, caption etc.

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

unit uletter;

{$mode objfpc}
{$H+}

interface

uses Classes, fgl;

type
  TDimension=(dx=0,dy=1,dz=2);

  TLetterState=(lsEmpty,    //no letter placed
                lsBag,      //in bag
                lsRack,     //on rack, can be changed or dragged
                lsDragging, //on rank, just dragging; need for removing a letter from board by calling in Pieces.DoUpdate
                lsChange,   //on rank, marked for change, can't be dragged
                lsBoard,    //on board, currently placed, can be dragged
                lsPlaced    //on board placed, can't be dragged
               );

  { TLetter }
  TLetter=class
    private
      FWhere   : array[TDimension] of byte; //position, assigned by PlaceLetterAt, 255,255,255 if not placed
      function GetWhere(index: TDimension): byte;
      procedure SetWhere(index: TDimension; const aValue: byte);
    public
      When    : Word;                      //placed at move #  (0 if not placed)
      Who     : Byte;                      //placed by player # (0 if not placed)
      What    : WideChar;                  //content, assigned on NewGame (or if ... IsJoker)
      Value   : byte;
      RackPos : byte;                      //position, assigned by NextPlayer or ExchangeLetter
      State   : TLetterState;              //important how to handle the letter
      IsJoker : boolean;                   //
      IsRandom : boolean;                  //randomly defined letters on game start
    public
      constructor Create;
      procedure AssignLetter(Source:TLetter);
      property Where[index:TDimension]:byte read GetWhere write SetWhere;
    end;//TLetter

  { TLetterList }
  TLetterList = specialize TFPGList<TLetter>;

implementation

{ TLetter }

constructor TLetter.Create;
begin
  inherited Create;
  When:=255;
  Who:=255;
  What:=#0;
  FWhere[dx]:=255;
  FWhere[dy]:=255;
  FWhere[dz]:=255;
  RackPos:=255;
  State:=lsBag;
  IsJoker:=false;
  IsRandom:=false;
end;

procedure TLetter.AssignLetter(Source: TLetter);
begin
  When:=Source.When;
  Who:=Source.Who;
  What:=Source.What;
  Value:=Source.Value;
  FWhere[dx]:=Source.Where[dx];
  FWhere[dy]:=Source.Where[dy];
  FWhere[dz]:=Source.Where[dz];
  RackPos:=Source.RackPos;
  State:=Source.State;
  IsJoker:=Source.IsJoker;
  IsRandom:=Source.IsRandom;
end;

function TLetter.GetWhere(index: TDimension): byte;
begin
  Result:=FWhere[index];
end;

procedure TLetter.SetWhere(index: TDimension; const aValue: byte);
begin
  FWhere[index]:=aValue;
end;

end.

