{ Scrabble core routines;
  TBoard: list of TLetters defined in uletter with procedures to check move

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

unit uboard;

{$mode objfpc}
{$H+}

{$I conditions.inc}

interface

uses
  Classes, uletter;

type
  TMove=record
    UsedLetters    : array of TLetter;
    Dimension      : TDimension;
    PlacedWord     : string;
    ConnectedWords : string;
    UnknownWords   : string;
    Value          : Word;
    IsScrabble     : boolean;
    Checked        : boolean;
  end;

  TActiveDimension=record
    Axis     : TDimension;
    Position : Byte;
  end; //TActiveDimension

  TIsKnownWord=function(const aValue: string; var aNotFound: string; DoAsk:boolean): Boolean of object;

  TLastError=(leNone,leNoLetter,leFirstMove,leDimension,leConnection,leSuccessive,leUnknownWord,leLetterChanged,leLowScore);

  TFieldType=(ftNormal=0,
              ftDoubleLetter=1,ftTripleLetter=2,ftQuadLetter=3,
              ftDoubleWord=4,ftTripleWord=5,ftQuadWord=6,
              ftStart=7,ftLetter=8,ftNewLetter=9,
              ftMalusSingleLetter=10,ftMalusDoubleLetter=11,ftMalusTripleLetter=12,ftMalusQuadLetter=13);

  TFieldTypeArray=array of array of array of TFieldType;

  { TBoard }

  TBoard=class
  private
    FFirstMove   : boolean;
    FLastMove    : TMove;
    FFieldType   : TFieldTypeArray;
    FBoardSize   : byte;
    FRackSize    : byte;
    FIsKnownWord : TIsKnownWord;
    FScrabbleBonus: byte;
    FLeastValue: byte;
    FBoardLetters: array of array of array of TLetter;
    function GetFieldType(x,y,z:byte): TFieldType;
    function GetBoardLetter(x,y,z:byte): TLetter;

    function CheckNoLetter:boolean;
    function CheckFirstMove:boolean;
    function CheckDimension:boolean;
    function CheckSuccessive:boolean;
    function CheckConnection:boolean;
    procedure SetBoardLetter(x, y, z: byte; AValue: TLetter);
    procedure SetFieldType(x, y, z: byte; const AValue: TFieldType);
    procedure SetBoardSize(const aValue: byte);
    procedure SetRackSize(const aValue: byte);
  protected
    LastError  : TLastError;
    LetterList : TLetterList;
    function CheckMove(AskForWord:boolean=false): TLastError; virtual;
    procedure ClearMove;
    procedure ClearLetterList;
    function WordAtPos(x,y,z : Byte; Dimension:TDimension; out aValue:word; out IsNew:boolean):string;
    property LastMove: TMove read FLastMove;
    property RackSize : byte read FRackSize write SetRackSize;
    property BoardSize : byte read FBoardSize write SetBoardSize;
    property BoardLetter[x,y,z:byte]:TLetter read GetBoardLetter write SetBoardLetter;
    property FieldType[x,y,z:byte]:TFieldType read GetFieldType write SetFieldType;
    property LeastValue:byte read FLeastValue write FLeastValue;
  public
    constructor Create(const Parent:TBoard=nil); //if Parent is set, all data are copied
    destructor Destroy; override;
    property OnIsKnownWord: TIsKnownWord read FIsKnownWord write FIsKnownWord;
    property Letters: TLetterList read LetterList;
    property ScrabbleBonus: byte read FScrabbleBonus write FScrabbleBonus;
    property FirstMove: boolean read FFirstMove;
  end;


implementation

{ TBoard }

constructor TBoard.Create(const Parent:TBoard);
var
  x,y,z: integer;
  aLetter: TLetter;
begin
  inherited Create;
  LetterList:=TLetterList.Create;
  LastError:=leNone;
  FBoardSize:=0;
  FLeastValue:=0;
  FScrabbleBonus:=50;
  if Parent<>nil then
  begin
    BoardSize:=Parent.BoardSize;
    RackSize:=Parent.RackSize;
    FIsKnownWord:=Parent.FIsKnownWord;
    FScrabbleBonus:=Parent.ScrabbleBonus;
    FLeastValue:=Parent.LeastValue;
    for x:=0 to FBoardSize-1 do
     for y:=0 to FBoardSize-1 do
      for z:=0 to FBoardSize-1 do
      begin
        FFieldType[x,y,z]:=Parent.FieldType[x,y,z];
        FBoardLetters[x,y,z]:=nil;
      end;

    for x:=0 to Parent.LetterList.Count-1 do
    begin
      aLetter:=TLetter.Create;
      aLetter.AssignLetter(Parent.LetterList[x]);
      if aLetter.State in [lsBoard,lsPlaced] then
        FBoardLetters[aLetter.Where[dx],aLetter.Where[dy],aLetter.Where[dz]]:=aLetter;
      LetterList.Add(aLetter);
    end;
  end;
end;

destructor TBoard.Destroy;
begin
  BoardSize:=0;
  RackSize:=0;
  ClearMove;
  ClearLetterList;
  LetterList.Free;
  inherited Destroy;
end;

procedure TBoard.ClearLetterList;
var
  i: integer;
  aLetter: TLetter;
begin
  for i:=0 to LetterList.Count-1 do
  begin
    aLetter:=TLetter(LetterList[i]);
    aLetter.Free;
  end;
  LetterList.Clear;
  FFirstMove:=true;
end;

procedure TBoard.ClearMove;
begin
  with FLastMove do
  begin
    setlength(UsedLetters,0);
    Dimension:=dx;
    PlacedWord:='';
    ConnectedWords:='';
    Value:=0;
    IsScrabble:=false;
    Checked:=false;
  end;
end;

procedure TBoard.SetBoardSize(const aValue: byte);
begin
  if FBoardSize<>aValue then
  begin
    FBoardSize:=aValue;
    setlength(FBoardLetters,FBoardSize,FBoardSize,FBoardSize);
    setlength(FFieldType,FBoardSize,FBoardSize,FBoardSize);
  end;
end;

procedure TBoard.SetRackSize(const aValue: byte);
begin
  if FRackSize<>aValue then
  begin
    FRackSize:=aValue;
  end;
end;

procedure TBoard.SetFieldType(x, y, z: byte; const AValue: TFieldType);
begin
  if (x<FBoardSize) and (y<FBoardSize) and (z<FBoardSize) then
    FFieldType[x,y,z]:=aValue;
end;

procedure TBoard.SetBoardLetter(x, y, z: byte; aValue: TLetter);
begin
  if (x<FBoardSize) and (y<FBoardSize) and (z<FBoardSize) then
    FBoardLetters[x,y,z]:=aValue;
end;

function TBoard.GetBoardLetter(x,y,z:byte): TLetter;
begin
  if (x<FBoardSize) and (y<FBoardSize) and (z<FBoardSize) then
    Result:=FBoardLetters[x,y,z] else
    Result:=nil;

{ //not fast enough for drawing full 3d cube;
  //FBoardLetters are asisgned in scrabble.updatemovestate or bruteforce.trytoplacemove
  Result:=nil;
  for i:=0 to LetterList.Count-1 do
  begin
    if (LetterList[i].State in [lsPlaced,lsBoard]) and
       (LetterList[i].Where[dx]=x) and
       (LetterList[i].Where[dy]=y) and
       (LetterList[i].Where[dz]=z) then
      exit(LetterList[i]);
  end;}
end;

function TBoard.GetFieldType(x,y,z:byte): TFieldType;
begin
  if (x<FBoardSize) and (y<FBoardSize) and (z<FBoardSize) then
    Result:=FFieldType[x,y,z];
end;

function TBoard.CheckNoLetter: boolean;
begin
  Result:=length(FLastMove.UsedLetters)>0;
end;

function TBoard.CheckFirstMove: boolean;
var
  i: integer;
begin
  Result:=false;
  for i:=0 to length(FLastMove.UsedLetters)-1 do
   with FLastMove.UsedLetters[i] do
    if FieldType[Where[dx],Where[dy],Where[dz]]=ftStart then Result:=true;
end;

function TBoard.CheckDimension: boolean;
var
  i: integer;
begin
  Result:=true;
  if length(FLastMove.UsedLetters)>1 then
  with FLastMove.UsedLetters[0] do
  begin
    if (Where[dx]=FLastMove.UsedLetters[1].Where[dx]) and (Where[dy]=FLastMove.UsedLetters[1].Where[dy]) then FLastMove.Dimension:=dz else
    if (Where[dx]=FLastMove.UsedLetters[1].Where[dx]) and (Where[dz]=FLastMove.UsedLetters[1].Where[dz]) then FLastMove.Dimension:=dy else
    if (Where[dy]=FLastMove.UsedLetters[1].Where[dy]) and (Where[dz]=FLastMove.UsedLetters[1].Where[dz]) then FLastMove.Dimension:=dx else
    Result:=false;
  end else
  with FLastMove.UsedLetters[0] do
  begin
    if (((Where[dx]>0) and (BoardLetter[Where[dx]-1,Where[dy],Where[dz]]<>nil)) or
        ((Where[dx]<FBoardSize) and (BoardLetter[Where[dx]+1,Where[dy],Where[dz]]<>nil))) then FLastMove.Dimension:=dx else
    if (((Where[dy]>0) and (BoardLetter[Where[dx],Where[dy]-1,Where[dz]]<>nil)) or
        ((Where[dy]<FBoardSize-1) and (BoardLetter[Where[dx],Where[dy]+1,Where[dz]]<>nil))) then FLastMove.Dimension:=dy else
        FLastMove.Dimension:=dz;
  end;
  for i:=1 to length(FLastMove.UsedLetters)-1 do
  with FLastMove.UsedLetters[0] do
  case FLastMove.Dimension of
    dx : if (Where[dy]<>FLastMove.UsedLetters[i].Where[dy]) or (Where[dz]<>FLastMove.UsedLetters[i].Where[dz]) then Result:=false;
    dy : if (Where[dx]<>FLastMove.UsedLetters[i].Where[dx]) or (Where[dz]<>FLastMove.UsedLetters[i].Where[dz]) then Result:=false;
    dz : if (Where[dx]<>FLastMove.UsedLetters[i].Where[dx]) or (Where[dy]<>FLastMove.UsedLetters[i].Where[dy]) then Result:=false;
  end;//case
end;

function TBoard.CheckSuccessive: boolean;
 procedure SortMove;
 var
   i,j: integer;
   swaped: boolean;
   tmp: TLetter;
 begin
    for i:=1 to length(FLastMove.UsedLetters)-1 do
    begin
      swaped:=false;
      for j:=0 to length(FLastMove.UsedLetters)-2 do
      begin
        if (FLastMove.UsedLetters[j].Where[FLastMove.Dimension]>FLastMove.UsedLetters[j+1].Where[FLastMove.Dimension]) then
        begin
          tmp:=FLastMove.UsedLetters[j]; FLastMove.UsedLetters[j]:=FLastMove.UsedLetters[j+1]; FLastMove.UsedLetters[j+1]:=tmp;
          swaped:=true;
        end;
      end;
      if not swaped then break;
    end;//for i:=1 to high(FLastMove) do
 end;
var
  i: integer;
  value: word;
  b: boolean;
begin
  Result:=true;
  SortMove;
  for i:=FLastMove.UsedLetters[0].Where[FLastMove.Dimension] to FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1].Where[FLastMove.Dimension] do
   case FLastMove.Dimension of
    dx : if BoardLetter[i,FLastMove.UsedLetters[0].Where[dy],FLastMove.UsedLetters[0].Where[dz]]=nil then Result:=false;
    dy : if BoardLetter[FLastMove.UsedLetters[0].Where[dx],i,FLastMove.UsedLetters[0].Where[dz]]=nil then Result:=false;
    dz : if BoardLetter[FLastMove.UsedLetters[0].Where[dx],FLastMove.UsedLetters[0].Where[dy],i]=nil then Result:=false;
   end;
   if Result then
   begin
     FLastMove.PlacedWord:=WordAtPos(FLastMove.UsedLetters[0].Where[dx],FLastMove.UsedLetters[0].Where[dy],FLastMove.UsedLetters[0].Where[dz],FLastMove.Dimension,Value,b);
     FLastMove.Value:=FLastMove.Value+Value;
   end;
end;

function TBoard.CheckConnection: boolean;
type TNeighbor=set of TDimension;
 function HasNeighbor(x,y,z:byte):TNeighbor;
 begin
   Result:=[];
   if (x<FBoardSize-1) and (BoardLetter[x+1,y,z]<>nil) and (BoardLetter[x+1,y,z].State=lsPlaced) then include(Result,dx);
   if (x>0) and (BoardLetter[x-1,y,z]<>nil) and (BoardLetter[x-1,y,z].State=lsPlaced) then include(Result,dx);
   if (y<FBoardSize-1) and (BoardLetter[x,y+1,z]<>nil) and (BoardLetter[x,y+1,z].State=lsPlaced) then include(Result,dy);
   if (y>0) and (BoardLetter[x,y-1,z]<>nil) and (BoardLetter[x,y-1,z].State=lsPlaced) then include(Result,dy);
   if (z<FBoardSize-1) and (BoardLetter[x,y,z+1]<>nil) and (BoardLetter[x,y,z+1].State=lsPlaced) then include(Result,dz);
   if (z>0) and (BoardLetter[x,y,z-1]<>nil) and (BoardLetter[x,y,z-1].State=lsPlaced) then include(Result,dz);
 end;
var
  i: integer;
  Neighbor: TNeighbor;
  v: word;
  b: boolean;
begin
  Result:=false;
  with FLastMove.UsedLetters[0] do
  begin
    //prior the first or after the last placed letter
    case FLastMove.Dimension of
     dx : if (Where[dx]>0) and (BoardLetter[Where[dx]-1,Where[dy],Where[dz]]<>nil) or
             (FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1].Where[dx]<FBoardSize-1) and
             (BoardLetter[FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1].Where[dx]+1,Where[dy],Where[dz]]<>nil)
             then Result:=true;
     dy : if (Where[dy]>0) and (BoardLetter[Where[dx],Where[dy]-1,Where[dz]]<>nil) or
             (FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1].Where[dy]<FBoardSize-1) and
             (BoardLetter[Where[dx],FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1].Where[dy]+1,Where[dz]]<>nil)
             then Result:=true;
     dz : if (Where[dz]>0) and (BoardLetter[Where[dx],Where[dy],Where[dz]-1]<>nil) or
             (FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1].Where[dz]<FBoardSize-1) and
             (BoardLetter[Where[dx],Where[dy],FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1].Where[dz]+1]<>nil)
              then Result:=true;
    end;
    for i:=Where[FLastMove.Dimension] to FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1].Where[FLastMove.Dimension] do
    begin
      case FLastMove.Dimension of
        dx : begin
               Neighbor:=HasNeighbor(i,FLastMove.UsedLetters[0].Where[dy],FLastMove.UsedLetters[0].Where[dz]);
               if Neighbor<>[] then Result:=true; //dx
               if (BoardLetter[i,FLastMove.UsedLetters[0].Where[dy],FLastMove.UsedLetters[0].Where[dz]].State=lsBoard) then
               begin
                 if (dy in Neighbor) then
                 begin
                   FLastMove.ConnectedWords:=FLastMove.ConnectedWords+WordAtPos(i,FLastMove.UsedLetters[0].Where[dy],FLastMove.UsedLetters[0].Where[dz],dy,v,b)+',';
                   FLastMove.Value:=FLastMove.Value+v;
                 end;
                 if (dz in Neighbor) then
                 begin
                   FLastMove.ConnectedWords:=FLastMove.ConnectedWords+WordAtPos(i,FLastMove.UsedLetters[0].Where[dy],FLastMove.UsedLetters[0].Where[dz],dz,v,b)+',';
                   FLastMove.Value:=FLastMove.Value+v;
                 end;
               end;
             end;
        dy : begin
               Neighbor:=HasNeighbor(FLastMove.UsedLetters[0].Where[dx],i,FLastMove.UsedLetters[0].Where[dz]);
               if Neighbor<>[] then Result:=true;
               if (BoardLetter[FLastMove.UsedLetters[0].Where[dx],i,FLastMove.UsedLetters[0].Where[dz]].State=lsBoard) then
               begin
                 if (dx in Neighbor) then
                 begin
                   FLastMove.ConnectedWords:=FLastMove.ConnectedWords+WordAtPos(FLastMove.UsedLetters[0].Where[dx],i,FLastMove.UsedLetters[0].Where[dz],dx,v,b)+',';
                   FLastMove.Value:=FLastMove.Value+v;
                 end;
                 if (dz in Neighbor) then
                 begin
                   FLastMove.ConnectedWords:=FLastMove.ConnectedWords+WordAtPos(FLastMove.UsedLetters[0].Where[dx],i,FLastMove.UsedLetters[0].Where[dz],dz,v,b)+',';
                   FLastMove.Value:=FLastMove.Value+v;
                 end;
               end;
             end;
        dz : begin
               Neighbor:=HasNeighbor(FLastMove.UsedLetters[0].Where[dx],FLastMove.UsedLetters[0].Where[dy],i);
               if Neighbor<>[] then Result:=true;
               if (BoardLetter[FLastMove.UsedLetters[0].Where[dx],FLastMove.UsedLetters[0].Where[dy],i].State=lsBoard) then
               begin
                 if (dx in Neighbor) then
                 begin
                   FLastMove.ConnectedWords:=FLastMove.ConnectedWords+WordAtPos(FLastMove.UsedLetters[0].Where[dx],FLastMove.UsedLetters[0].Where[dy],i,dx,v,b)+',';
                   FLastMove.Value:=FLastMove.Value+v;
                 end;
                 if (dy in Neighbor) then
                 begin
                   FLastMove.ConnectedWords:=FLastMove.ConnectedWords+WordAtPos(FLastMove.UsedLetters[0].Where[dx],FLastMove.UsedLetters[0].Where[dy],i,dy,v,b)+',';
                   FLastMove.Value:=FLastMove.Value+v;
                 end;
               end;
             end;//dz
        end;//case
    end;//for to
  end //with
end;

function TBoard.WordAtPos(x, y, z: Byte; Dimension: TDimension; out aValue: word; out IsNew:boolean): string;
 procedure ValueOfLetterByField(aLetter:TLetter;var v:integer;var m:word);
 begin
   if aLetter.State=lsPlaced then
     inc(v,aLetter.Value) else
   case FFieldtype[aLetter.Where[dx],aLetter.Where[dy],aLetter.Where[dz]] of
    ftNormal,ftLetter,ftNewLetter : inc(v,aLetter.Value);
    ftDoubleLetter : inc(v,aLetter.Value*2);
    ftTripleLetter : inc(v,aLetter.Value*3);
    ftQuadLetter : inc(v,aLetter.Value*4);
    ftStart      : begin inc(v,aLetter.Value); m:=m*2; end;
    ftDoubleWord : begin inc(v,aLetter.Value); m:=m*2; end;
    ftTripleWord : begin inc(v,aLetter.Value); m:=m*3; end;
    ftQuadWord : begin inc(v,aLetter.Value); m:=m*4; end;
    ftMalusSingleLetter:dec(v,aLetter.Value*1);
    ftMalusDoubleLetter:dec(v,aLetter.Value*2);
    ftMalusTripleLetter:dec(v,aLetter.Value*3);
    ftMalusQuadLetter:dec(v,aLetter.Value*4);
   end; //case
 end;
var i       : integer;
    aLetter : TLetter;
    v       : integer;
    m       : word;
begin
  Result:='';
  IsNew:=false;
  v:=0;m:=1;
  //forward
  case Dimension of
   dx : i:=x;
   dy : i:=y;
   dz : i:=z;
  end;
  aLetter:=BoardLetter[x,y,z];
  while (i>=0) and (aLetter<>nil) do
  begin
    Result:=UTF8Encode(widestring(aLetter.What))+Result;
    if aLetter.State=lsBoard then IsNew:=true;
    ValueOfLetterByField(aLetter,v,m);
    dec(i);
    if i>=0 then
      case Dimension of
       dx : aLetter:=BoardLetter[i,y,z];
       dy : aLetter:=BoardLetter[x,i,z];
       dz : aLetter:=BoardLetter[x,y,i];
      end;
  end;
  //backward
  case Dimension of
   dx : i:=x;
   dy : i:=y;
   dz : i:=z;
  end;
  aLetter:=BoardLetter[x,y,z];
  inc(i);

  while (i<FBoardSize) and (aLetter<>nil) do
  begin
    case Dimension of
     dx : aLetter:=BoardLetter[i,y,z];
     dy : aLetter:=BoardLetter[x,i,z];
     dz : aLetter:=BoardLetter[x,y,i];
    end;
    if aLetter<>nil then
    begin
      Result:=Result+UTF8Encode(widestring(aLetter.What));
      if aLetter.State=lsBoard then IsNew:=true;
      ValueOfLetterByField(aLetter,v,m);
    end;
    inc(i);
  end;
  if v*m>0 then
    aValue:=v*m else
    aValue:=0;
end;

function TBoard.CheckMove(AskForWord:boolean): TLastError;
var
  i: integer;
begin
  FFirstMove:=true;
  ClearMove;

  for i:=0 to LetterList.Count-1 do
   case LetterList[i].State of
    lsPlaced : FFirstMove:=false;
    lsBoard  : begin
                 setlength(FLastMove.UsedLetters,length(FLastMove.UsedLetters)+1);
                 FLastMove.UsedLetters[length(FLastMove.UsedLetters)-1]:=LetterList[i];
               end;
    end;//case

  if not CheckNoLetter then LastError:=leNoLetter else
   if FFirstMove and not CheckFirstMove then LastError:=leFirstMove else
    if not CheckDimension then LastError:=leDimension else
     if not CheckSuccessive then LastError:=leSuccessive else
      if not FFirstMove and not CheckConnection then LastError:=leConnection else
      begin
        if assigned(FIsKnownWord) and
           not FIsKnownWord(FLastMove.PlacedWord+','+FLastMove.ConnectedWords,FLastMove.UnknownWords,AskForWord) then
          LastError:=leUnknownWord else
          LastError:=leNone;
      end;
  if (LastError in [leNone,leUnknownWord]) and (length(FLastMove.UsedLetters)=FRackSize) then
  begin
    FLastMove.IsScrabble:=true;
    inc(FLastMove.Value,FScrabbleBonus);
  end;
  if (length(FLastMove.ConnectedWords)>0) and (FLastMove.ConnectedWords[length(FLastMove.ConnectedWords)]=',') then
    system.delete(FLastMove.ConnectedWords,length(FLastMove.ConnectedWords),1);
  FLastMove.Checked:=true;
  if (LastError in [leNone,leUnknownWord]) and (FLastMove.Value<FLeastValue) then
    LastError:=leLowScore;
  Result:=LastError;
end;

end.

