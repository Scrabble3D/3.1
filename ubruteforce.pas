{ Brute force algorithm

  AI computes a move by
  1. collecting the needed operations (col x row x depth in 3d -> n(x)+n(y)*n(z)^3 (?)
     operations are not included if no neighbors are found
     TSingleOperation is derived from the current or actual board (on history browsing)
  2. run operations (single or multithreaded)
     2.1 compute words that could be generated from available letters -> udictionary
         (activate UseDAWG switch in udictionary to organize dic with DAWG structure)
     2.2 try to place all words at all positions
     2.3 check move
     2.4 add to bestmove if valid
  3. sort bestmove by value
  4. place bestmove[0 resp. comp settings] or exchange joker/letters

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

unit ubruteforce;

{$mode objfpc}{$H+}

{$I conditions.inc}

{$ifdef Debug}
 {$Warning Bruteforce:PerformanceDebug}
 {$define PerformanceDebug}
{$endif}

{.$define UseMTProcs} //no feedback with mtp

interface

uses
  Classes, SysUtils, Controls, LCLIntf, LazUTF8, LCLType, Forms, fgl,
  {$ifdef UseMTProcs}
  mtprocs,
  {$endif}
  Clipbrd,
  uletter,
  uscrabble,
  uboard,
  utypes, //TOnMessage
  udictionary,
  ulanguage;

type

  { TSingleOperation }

  TBestMove=class
    Value    : word;
    Text     : string;
    Position : TActiveDimension;
    Letters  : TLetterList;
  end;
  TBestMoveList = specialize TFPGList<TBestMove>;

  TOnGetRackLetter = function(aMoveNumber:word;aRackLetter:byte):integer of Object;

  TSingleOperation=class(TBoard)
    private
      FThread           : TThread;
      FActiveDimension  : TActiveDimension; //dx..dz
      FBestMoves        : TBestMoveList;
      FColRow           : byte;             //single Column; 0..BoardSize
      FVertical         : boolean;          //FColRow is col or row
      FLeftToRight      : boolean;          //right to left direction for e.g. arabic lang
      FPlacedLetters    : TLetterList;
      FRackLetters      : array of TLetter;
      FAborted          : boolean;
      FDummy            : TStringList;
      procedure ClearBoard;
      function GetRackLetter(aRackPos: byte): TLetter;
      function GetAvailableLetters:string;
      procedure SetRackLetter(aRackPos: byte; AValue: TLetter);
      function TryToPlaceWord(aValue:string;aPos:byte):boolean;
      function ShuffleJoker(var start:byte):boolean;
      function IsKnownWord(const aValue: string; var aNotFound: string;DoAsk:boolean=false): boolean;
    protected
      procedure Run;         //collect letters, get WordsByLetters(), TryToPlaceWord() and add to FBestMoves
      property RackLetter[aRackPos:byte]:TLetter read GetRackLetter write SetRackLetter;
    public
      constructor Create(aDim:TActiveDimension;aColRow:byte;aVertical,aLeftToRight:boolean);
      destructor Destroy; override;
      property Aborted:boolean read FAborted write FAborted;
      property Dimension:TActiveDimension read FActiveDimension;
      property Thread: TThread read FThread write FThread;
    end;//TSingleOperation
  TProcessingList = specialize TFPGList<TSingleOperation>;

  { TBruteForce }
  TBruteForceSettings=record                                                             // fm.GameOptions
    Performance      : byte;    //10..100%                                               / tbCompPerformance
    Deterministic    : boolean; //how to sort result list                                / cbCompDeterministic
    CompleteExchange : boolean; //exchange high value letters                            / cbCompleteExchange
    ExchangeJoker    : byte;    //exchange joker if result value is less this value      / tbCompExchange
    ExchangeValue    : byte;    //exchange letters if result value is less this value... / tbCompChangeValue
    ExchangeLetters  : byte;    //... and if result length is less this value            / tbCompChangeLetters
    UseJoker         : byte;    //use joker in advance of at least points                / tbCompUseJokers
    RunThreaded      : boolean; //single- or multithreaded                               / cbParallel
    RomanLabels      : boolean; //roman or greek                                         / cbRoman
    LeftToRight      : boolean; //lefttoright or righttoleft                             / rbLeftToRight
    IsFirstMove      : boolean; //computed from gamecourse
  end;

  TBruteForce=class
    private
      FBestMoves  : TBestMoveList;
      FPaused     : boolean;
      FProgress   : PtrInt;
      FProcessing : TProcessingList;
      FAborted    : boolean;
      FListPos    : integer;
      FMaxThreads : integer;
      FSettings   : TBruteForceSettings;
      FOnGetRackLetter  : TOnGetRackLetter;
      function GetBestMovesCount: integer;
      function GetBoardLetter(x, y, z: byte): TLetter;
      function GetWordAtPos(ax, ay, az: byte): string;
      function RackLetterCount(aValue:TBestMove):byte;
      procedure SetAborted(const aValue: boolean);
      procedure SetupProcessing;
      function SortBestMoves:word;
      procedure TryToExchangeJoker;
      {$ifndef UseMTProcs}
      procedure RunThreaded;
      {$endif}
      procedure MarkLettersForChange;
    public
      procedure CopyBestMoves;
    public
      constructor Create;
      destructor Destroy; override;
      {$ifdef PerformanceDebug}
      procedure CopyPerformance;
      {$endif}
      procedure ClearMove;
      function ComputeMove(aBruteForceSettings:TBruteForceSettings): Word;
      procedure Report(const aNumber:integer;var aValue:word;var aWord:string;var aPosition:string;var aDimension:TActiveDimension);
      procedure PlaceBestMove(const aValue:integer; const CanExchange:boolean);
      property BestMovesCount: integer read GetBestMovesCount;
      property BoardLetter[x,y,z:byte]: TLetter read GetBoardLetter;
      property Aborted: boolean read FAborted write SetAborted;
      property Paused: boolean read FPaused write FPaused;
      property WordAtPos[ax,ay,az:byte]: string read GetWordAtPos;
      property OnGetRackLetter:TOnGetRackLetter read FOnGetRackLetter write FOnGetRackLetter;
      property ResultNumber: integer read FListPos;
    end;

  { TSingleThread }

  TSingleThread=class(TThread)
    private
      FOperation : TSingleOperation;
    protected
      procedure Execute; override;
    public
      constructor Create;
      destructor Destroy;override;
      property Operation:TSingleOperation read FOperation write FOperation;
    end;


var
  BruteForce    : TBruteForce;

implementation

var
  Criticalsection: TRTLCriticalsection;
  ThreadsRunning: integer;
  TargetMoveValue: double;   //helper var for sorting
  {$ifdef PerformanceDebug}
  zBegin: longword;      //total
  zSetup: longword;      //setup
  zWbL,zWbLc: longword;  //wordsbyletter
  zCM,zCMc: longword;    //checkmove
  zTtP,zTtPc: longword;  //trytoplace
  slPerformance: TStringList;
  {$endif}

{ TBrutForce }

constructor TBruteForce.Create;
begin
  inherited Create;
  FBestMoves:=TBestMoveList.Create;
  FProcessing:=TProcessingList.Create;
  {$ifdef PerformanceDebug}
  slPerformance:=TStringList.Create;
  slPerformance.Add('Total'+#9+'Setup'+#9+'WBL'+#9+'WBLc'+#9+'TtP'+#9+'TtPc'+#9+'CM'+#9+'CMc'+#9+'Sort'+#9+'BMC');
  {$endif}
end;

destructor TBruteForce.Destroy;
begin
  ClearMove;
  FBestMoves.Free;
  FProcessing.Free;
  {$ifdef PerformanceDebug}
  slPerformance.Free;
  {$endif}
  inherited Destroy;
end;

procedure TBruteForce.ClearMove;
var
  i,j: integer;
begin
  for i:=0 to FBestMoves.Count-1 do
  begin
    for j:=0 to FBestMoves[i].Letters.Count-1 do
      FBestMoves[i].Letters[j].Free;
    FBestMoves[i].Letters.Clear;
    FBestMoves[i].Letters.Free;
    FBestMoves[i].Free;
  end;
  FBestMoves.Clear;
end;

{$ifdef PerformanceDebug}
procedure TBruteForce.CopyPerformance;
var
  i:integer;
begin
  ClipBoard.Clear;
  for i:=0 to slPerformance.Count-1 do
    ClipBoard.AsText:=ClipBoard.AsText+slPerformance[i]+#10#13;
  slPerformance.Clear;
  slPerformance.Add('Total'+#9+'Setup'+#9+'WBL'+#9+'WBLc'+#9+'TtP'+#9+'TtPc'+#9+'CM'+#9+'CMc'+#9+'Sort'+#9+'BMC');
end;
{$endif}

function TBruteForce.ComputeMove(aBruteForceSettings:TBruteForceSettings): Word;
 {$ifdef UseMTProcs}
 procedure RunMTProcs(Index: PtrInt; Processing: TProcessingList; Item: TMultiThreadProcItem);
 begin
   TSingleOperation(Processing[Index]).Run; //no progress with mtprocs
 end;
 {$endif}
var
  i,j: integer;
  z: Longword;
begin
  FAborted:=false;
  FListPos:=MaxInt;
  FMaxThreads:=GetSystemThreadCount;
  FSettings:=aBruteForceSettings;
  Scrabble.GameState:=Scrabble.GameState+[gsComputing];
  {$ifdef PerformanceDebug}
  zBegin:=GetTickCount;
  zWbL:=0;zWbLc:=0;
  zCM:=0;zCMc:=0;
  zTtP:=0;zTtPc:=0;
  {$endif}
  if BestMovesCount=0 then //do not rerun same calculation
  begin
    Screen.Cursor:=crHourGlass;
    z:=GetTickCount;
    SetupProcessing;     //setup
    {$ifdef PerformanceDebug}
    zSetup:=GetTickCount-z;
    {$endif}
    FProgress:=0;
    OnMessage(smDebug,inttostr(FProcessing.Count)+' calculations to be done');
    if aBruteForceSettings.RunThreaded then    //run parallel
    begin
      {$ifdef UseMTProcs}
      ProcThreadPool.DoParallelLocalProc(@RunMTProcs,0,FProcessing.Count-1,FProcessing);
      {$else}
      System.InitCriticalsection(Criticalsection);
      RunThreaded;
      System.DoneCriticalsection(Criticalsection);
      {$endif}
    end else
    begin                                      //or run single threaded
      for i:=0 to FProcessing.Count-1 do
      begin
        OnProgress(self,round((i/FProcessing.Count)*100));
        FProcessing[i].Run;
        while FPaused do
        begin
          sleep(10);
          Application.ProcessMessages;
        end;
        if FAborted then break;
      end;
    end;
    OnMessage(smDebug,'finished in '+inttostr(GettickCount-z)+' ms');
    //bestmoves
    for i:=FProcessing.Count-1 downto 0 do
    begin
      for j:=0 to FProcessing[i].FBestMoves.Count-1 do
        FBestMoves.Add(FProcessing[i].FBestMoves[j]);
      FProcessing[i].Free;
    end;
    FProcessing.Clear;

    if BestMovesCount=0 then
        OnMessage(smInformation,rBruteforce_NoValidWords) else
        OnMessage(smDebug,inttostr(BestMovesCount)+' valid words found');
  end;
  z:=GetTickCount;
  Result:=SortBestMoves; //sort
  OnProgress(self,101);               //reset progress bar
  {$ifdef PerformanceDebug}
  slPerformance.Add(inttostr(GetTickCount-zBegin)+#9+inttostr(zSetup)+#9+
                    inttostr(zWbL)+#9+inttostr(zWbLc)+#9+
                    inttostr(zTtP)+#9+inttostr(zTtPc)+#9+
                    inttostr(zCM)+#9+inttostr(zCMc)+#9+
                    inttostr(GetTickCount-z)+#9+
                    inttostr(BestMovesCount));
  {$endif}
  Scrabble.GameState:=Scrabble.GameState-[gsComputing];
  Screen.Cursor:=crDefault;
end;

{$ifndef UseMTProcs}
procedure TBruteForce.RunThreaded;
var
  aStart,aEnd: integer;
  aThread: TSingleThread;
begin
  aStart:=0;
  aEnd:=FProcessing.Count;
  ThreadsRunning:=0;
  repeat
    if FAborted then
      aStart:=aEnd;
    if (ThreadsRunning<FMaxThreads) and
       not FPaused and
       (aStart<aEnd) then
    begin
      OnProgress(self,round((aStart/aEnd)*100));
      aThread:=TSingleThread.Create;
      aThread.Operation:=FProcessing[aStart];
      aThread.Start;
      inc(aStart);
    end;
    if FPaused then
      Application.ProcessMessages;
    sleep(0);
  until (aStart>=FProcessing.Count-1) and (ThreadsRunning=0);
end;
{$endif}

procedure TBruteForce.TryToExchangeJoker;
var aLetter, aRack : TLetter;
    x,y,z,i        : integer;
    s              : string;
begin
  for x:=0 to Scrabble.BoardSize-1 do
   for y:=0 to Scrabble.BoardSize-1 do
    for z:=0 to Scrabble.BoardSize-1 do
    begin
      aLetter:=Scrabble.BoardLetter[x,y,z];
      if (aLetter<>nil) and (aLetter.IsJoker) then
       for i:=0 to Scrabble.RackSize-1 do
       begin
         aRack:=Scrabble.RackLetter[Scrabble.CurrentPlayer,i];
         if (aRack<>nil) and (aRack.What=aLetter.What) then
         begin
           Scrabble.PlaceLetterAt(aRack,x,y,z);
           s:=Dictionary.ReplaceDigraphs(aLetter.What);

           case Scrabble.Dimension of
            D2 : OnMessage(smInformation,Language.Format(rMain_Exchange2D,
                     [Scrabble.Player[Scrabble.CurrentPlayer].Name,
                      Scrabble.PosToString(dy,y,FSettings.RomanLabels),
                      Scrabble.PosToString(dx,x,FSettings.RomanLabels),
                      s]));
            D3 : OnMessage(smInformation,Language.Format(rMain_Exchange3D,
                     [Scrabble.Player[Scrabble.CurrentPlayer].Name,
                      Scrabble.PosToString(dy,y,FSettings.RomanLabels),
                      Scrabble.PosToString(dx,x,FSettings.RomanLabels),
                      Scrabble.PosToString(dz,z,FSettings.RomanLabels),
                      s]));
           end;//case, if
           exit;
         end;
       end;//Rack
    end;//board
end;

procedure TBruteForce.MarkLettersForChange;
var
  i,j:integer;
  aLetter1,aLetter2:TLetter;
begin
  if FSettings.CompleteExchange then
  begin
    with TList.Create do
    try
      for i:=0 to Scrabble.RackSize-1 do
      begin
        aLetter1:=Scrabble.RackLetter[Scrabble.CurrentPlayer,i];
        if (aLetter1<>nil) and not aLetter1.IsJoker then Add(aLetter1);
      end;
      for i:=0 to Count-2 do
       for j:=i+1 to Count-1 do
        if TLetter(Items[i]).Value<TLetter(Items[j]).Value then Exchange(i,j);
      for i:=1 to Count-1 do
       Scrabble.ToggleChangeState(TLetter(Items[i]).RackPos);
    finally
      Free;
    end;
  end else
  begin
    //mark multiple letters for change
    for i:=0 to Scrabble.RackSize-2 do
     for j:=i+1 to Scrabble.RackSize-1 do
     begin
       aLetter1:=Scrabble.RackLetter[Scrabble.CurrentPlayer,i];
       aLetter2:=Scrabble.RackLetter[Scrabble.CurrentPlayer,j];
       if (aLetter1<>nil) and (aLetter2<>nil) and
          (aLetter1.What=aLetter2.What) and (aLetter1.State=lsRack) then
         Scrabble.ToggleChangeState(i);//aLetter1.State:=lsChange;
     end;
    //mark letter with highest value for exchange
    if Scrabble.MoveState=msNone then
    with TList.Create do
    try
      for i:=0 to Scrabble.RackSize-1 do
      begin
        aLetter1:=Scrabble.RackLetter[Scrabble.CurrentPlayer,i];
        if aLetter1<>nil then Add(aLetter1);
      end;
      for i:=0 to Count-2 do
       for j:=i+1 to Count-1 do
        if TLetter(Items[i]).Value<TLetter(Items[j]).Value then Exchange(i,j);
      if Count>0 then Scrabble.ToggleChangeState(TLetter(Items[0]).RackPos);
    finally
      Free;
    end;
  end;
end;

procedure TBruteForce.CopyBestMoves;
var
  i,j: integer;
  s: string;
  c: widestring;
begin
  ClipBoard.Clear;
  s:='';
  for i:=0 to FBestMoves.Count-1 do
  with FBestMoves[i] do
  begin
    //word
    s:=s+Text;
    //value
    s:=s+','+inttostr(FBestMoves[i].Value);
    //positon
    s:=s+','+Scrabble.PosToString(dx,Letters[0].Where[dx])+Scrabble.PosToString(dy,Letters[0].Where[dy]);
    if Scrabble.Dimension=D3 then
    begin
      s:=s+Scrabble.PosToString(dz,Letters[0].Where[dz]);
      //s:=s+inttostr(Position.Position);
    end;
{    case Position.Axis of
     dx:s:=s+'x';
     dy:s:=s+'y';
     dz:s:=s+'z';
    end;
}    //placed letters; jokers in lowercase
    c:='';
    for j:=0 to Letters.Count-1 do
    begin
      if Letters[j].IsJoker then
        c:=c+widelowercase(Letters[j].What) else
        c:=c+wideuppercase(Letters[j].What);
    end;
    s:=s+','+UTF8Encode(c);
    //line wrap
    s:=s+#10#13;
  end;
  ClipBoard.AsText:=s;
end;

procedure TBruteForce.Report(const aNumber:integer;
                             var aValue:word;
                             var aWord:string;
                             var aPosition:string;
                             var aDimension:TActiveDimension);
begin
  FListPos:=aNumber;
  if (aNumber>=0) and (FBestMoves.Count>aNumber) then
  with FBestMoves[aNumber] do
  begin
    aValue:=round(Value);
    aWord:=Text;
    aDimension:=Position;
    aPosition:=Scrabble.PosToString(dy,Letters[0].Where[dy],FSettings.RomanLabels)+','+
               Scrabble.PosToString(dx,Letters[0].Where[dx],FSettings.RomanLabels);
    if Scrabble.Dimension=D3 then
      aPosition:=aPosition+','+Scrabble.PosToString(dz,Letters[0].Where[dz],FSettings.RomanLabels);
  end else
  begin
    aValue:=0;
    aWord:='';
    aDimension:=Scrabble.ActiveDimension;
    if Scrabble.Dimension=D3 then
      aPosition:='0,0,0' else
      aPosition:='0,0';
  end;
end;

procedure TBruteForce.SetupProcessing;
 function HasNeighbor(aDimension: TActiveDimension; aColRow: byte; aVertical: boolean): boolean;
 type NeighborPos=(nbLeftX,nbRightX,nbLeftY,nbRightY,nbLeftZ,nbRightZ);
 var i:integer;
     j:NeighborPos;
     x,y,z:byte;
     aLetter:TLetter;
 begin
   Result:=false;
   for i:=0 to Scrabble.BoardSize-1 do
   begin
     case aVertical of
      true  : if not FSettings.LeftToRight then
                Scrabble.Convert2DTo3D(Scrabble.BoardSize-i-1,aColRow,x,y,z,aDimension) else
                Scrabble.Convert2DTo3D(i,aColRow,x,y,z,aDimension);
      false : Scrabble.Convert2DTo3D(aColRow,i,x,y,z,aDimension);
     end;
     Result:=true;
     //start field or placed letter
     if (Scrabble.FieldType[x,y,z]=ftStart) and
        ((Scrabble.BoardLetter[x,y,z]=nil) or
         (Scrabble.BoardLetter[x,y,z].When=Scrabble.ActualMove) or
         FSettings.IsFirstMove
        ) then exit;
     for j:=low(NeighborPos) to high(NeighborPos) do
     begin
       aLetter:=nil;
       case j of
        nbLeftX  : if (x>0) then aLetter:=Scrabble.BoardLetter[x-1,y,z];
        nbRightX : if (x<Scrabble.BoardSize-1) then aLetter:=Scrabble.BoardLetter[x+1,y,z];
        nbLeftY  : if (y>0) then aLetter:=Scrabble.BoardLetter[x,y-1,z];
        nbRightY : if (y<Scrabble.BoardSize-1) then aLetter:=Scrabble.BoardLetter[x,y+1,z];
        nbLeftZ  : if (Scrabble.Dimension=D3) and (z>0) then aLetter:=Scrabble.BoardLetter[x,y,z-1];
        nbRightZ : if (Scrabble.Dimension=D3) and (z<Scrabble.BoardSize-1) then aLetter:=Scrabble.BoardLetter[x,y,z+1];
       end;
       if (aLetter<>nil) and (aLetter.State=lsPlaced) and (aLetter.When>Scrabble.ActualMove-1) then aLetter:=nil;
       if (aLetter<>nil) then exit;
     end;
     Result:=false;
   end;//for to
 end;//function
var
  i,j,k,x,y,z : byte;
  aLetterIndex:integer;
  aLetter: TLetter;
  dim: TDimension;
  ad: TActiveDimension;
  Operation: TSingleOperation;
begin
  for dim:=dx to dz do
   for i:=0 to Scrabble.BoardSize-1 do
   begin
     ad.Axis:=dim;ad.Position:=i;
     if (Scrabble.Dimension=D3) or ((dim=dx) and (i=0)) then
     for j:=0 to 1 do //col|row
      for k:=0 to Scrabble.BoardSize-1 do
      begin
        if HasNeighbor(ad,k,boolean(j)) then
        begin
          Operation:=TSingleOperation.Create(ad,k,boolean(j),FSettings.LeftToRight);
          Operation.OnIsKnownWord:=@Operation.IsKnownWord;
          //override letters from gamecourse history
          if (Scrabble.ActualMove<Scrabble.CurrentMove) and
             assigned(FOnGetRackLetter) then
          begin
            //take back placed letter
            for z:=0 to Operation.LetterList.Count-1 do
            begin
              aLetter:=Operation.LetterList[z];
              if ((aLetter.State=lsPlaced) and (aLetter.When>Scrabble.ActualMove)) or
                 (aLetter.State=lsRack) then
              begin
                aLetter.State:=lsBag;
                aLetter.RackPos:=255;
                aLetter.When:=255;
                aLetter.Where[dx]:=255;aLetter.Where[dy]:=255;aLetter.Where[dz]:=255;
                if aLetter.IsJoker then aLetter.What:=ltJoker;
              end;
            end;
            //clear and update rack from game history
            for z:=0 to Scrabble.RackSize-1 do
            begin
              Operation.RackLetter[z]:=nil;
              aLetterIndex:=FOnGetRackLetter(Scrabble.ActualMove,z);
              if aLetterIndex>-1 then
              begin
                aLetter:=Operation.LetterList[aLetterIndex];
                aLetter.When:=Scrabble.ActualMove;
                aLetter.State:=lsRack;
                aLetter.RackPos:=z;
                aLetter.Who:=(Scrabble.ActualMove mod Scrabble.NumberOfPlayers);
                if aLetter.IsJoker then aLetter.What:=ltJoker;
                Operation.RackLetter[aLetter.RackPos]:=aLetter;
              end;
            end;
            //clear board
            for x:=0 to Operation.BoardSize-1 do
             for y:=0 to Operation.BoardSize-1 do
              for z:=0 to Operation.BoardSize-1 do
               Operation.BoardLetter[x,y,z]:=nil;
            //update board
            for z:=0 to Operation.LetterList.Count-1 do
            begin
              aLetter:=Operation.LetterList[z];
              if aLetter.State in [lsBoard,lsPlaced] then
               Operation.BoardLetter[aLetter.Where[dx],aLetter.Where[dy],aLetter.Where[dz]]:=aLetter;
            end;
          end;
          FProcessing.Add(Operation);
        end;
      end;//for j,for k
   end;//for dim,for i
end;

function TBruteForce.GetBestMovesCount: integer;
begin
  if assigned(FBestMoves) then
    Result:=FBestMoves.Count;
end;

function TBruteForce.GetBoardLetter(x, y, z: byte): TLetter;
var
  i:integer;
begin
  if FProcessing.Count>0 then
    Result:=FProcessing[0].BoardLetter[x,y,z] else
    Result:=Scrabble.BoardLetter[x,y,z];

  //hide placed letters of actual move
  //if no calculation done yet (maxint)
  //or in case of no result (FBestMoves.Count)
  if (Result<>nil) and
     (FListPos<MaxInt) and
     (FBestMoves.Count>0) and
     (Result.When=Scrabble.ActualMove) then
    Result:=nil;

  if FBestMoves.Count>FListPos then
   with FBestMoves[FListPos] do
    for i:=0 to Letters.Count-1 do
     with Letters[i] do
      if (Where[dx]=x) and (Where[dy]=y) and (Where[dz]=z) then
       exit(Letters[i]);
end;

function TBruteForce.GetWordAtPos(ax, ay, az: byte): string;
var
  s: string;
  i: word;
  b: boolean;
begin
  if (FProcessing.Count>0) then
  begin
    Result:='';
    s:=FProcessing[0].WordAtPos(ax,ay,az,dx,i,b);
    if UTF8Length(s)>1 then Result:=s+',';
    s:=FProcessing[0].WordAtPos(ax,ay,az,dy,i,b);
    if UTF8Length(s)>1 then Result:=Result+s+',';
    s:=FProcessing[0].WordAtPos(ax,ay,az,dz,i,b);
    if UTF8Length(s)>1 then Result:=Result+s+',';
  end else
    Result:=Scrabble.WordAtPos[ax,ay,az,false];

  if FBestMoves.Count>FListPos then
    Result:=Result+FBestMoves[FListPos].Text;
end;

function TBruteForce.RackLetterCount(aValue: TBestMove): byte;
var
  i:integer;
begin
  Result:=0;
  for i:=0 to aValue.Letters.Count-1 do
   if aValue.Letters[i].State<>lsPlaced then
    inc(Result);
end;

procedure TBruteForce.SetAborted(const aValue: boolean);
var
  i:integer;
begin
  if aValue then FPaused:=false;
  FAborted:=aValue;
  for i:=0 to FProcessing.Count-1 do
    FProcessing[i].Aborted:=aValue;
end;

function SortByNumberOfLetters(item1,item2:Pointer):integer;
begin
  if TBestMove(item1).Letters.Count<TBestMove(item2).Letters.Count then Result:=1 else
  if TBestMove(item1).Letters.Count>TBestMove(item2).Letters.Count then Result:=-1 else
  begin
    if TBestMove(item1).Value<TBestMove(item2).Value then Result:=1 else
    if TBestMove(item1).Value>TBestMove(item2).Value then Result:=-1 else
      Result:=CompareText(TBestMove(item1).Text,TBestMove(item2).Text);
  end;
end;

procedure TBruteForce.PlaceBestMove(const aValue:integer; const CanExchange:boolean);
var
  i,j,MaxRackLetters: integer;
  tmp: TLetter;
begin
  if (CanExchange) then
  begin
    //try to exchange joker
    if (gsJokerExchange in Scrabble.GameState) and
       ((FBestMoves.Count=0) or (FBestMoves[0].Value<FSettings.ExchangeJoker)) then
      TryToExchangeJoker;

    //mark letters for change
    if (Scrabble.MoveState=msNone) then
    begin
      MaxRackLetters:=0;
      for i:=0 to FBestMoves.Count-1 do
      begin
        j:=RackLetterCount(FBestMoves[i]);
        if j>MaxRackLetters then MaxRackLetters:=j;
      end;
      if (FBestMoves.Count=0) or
         ((FBestMoves[0].Value<FSettings.ExchangeValue) and
          (MaxRackLetters<FSettings.ExchangeLetters)) then
          MarkLettersForChange;
    end;
  end;

  if not (Scrabble.MoveState in [msJokerExchanged,msLetterExchange]) and
     (FBestMoves.Count>aValue) then
  begin
    Scrabble.RestoreBoard;
    with FBestMoves[aValue] do
    begin
      Scrabble.ActiveDimension:=Position;
      for i:=0 to Letters.Count-1 do
      with Letters[i] do
      begin
        for j:=0 to Scrabble.RackSize-1 do
        begin
          tmp:=Scrabble.RackLetter[Scrabble.CurrentPlayer,j];
          if (tmp<>nil) and (tmp.State=lsRack) and
             (tmp.What=What) and (tmp.IsJoker=IsJoker) then
          begin
            Scrabble.PlaceLetterAt(tmp,Where[dx],Where[dy],Where[dz]);
            break;
          end else tmp:=nil;
        end; //RackSize
        if tmp=nil then
        for j:=0 to Scrabble.RackSize-1 do
        begin
          tmp:=Scrabble.RackLetter[Scrabble.CurrentPlayer,j];
          if (tmp<>nil) and (tmp.State=lsRack) and (tmp.IsJoker) then
          begin
            tmp.What:=What;
            Scrabble.PlaceLetterAt(tmp,Where[dx],Where[dy],Where[dz]);
            break;
          end;
        end; //RackSize
      end; //with TLetter
    end; //with TBestMove
  end;//if aValue<Count
end;

function SortByValue(const Item1,Item2:TBestMove):integer;
begin
  if Item1.Value<Item2.Value then
    Result:=1 else
  if Item1.Value>Item2.Value then
    Result:=-1 else
    Result:=CompareText(Item1.Text,Item2.Text);
end;

function SortByPerformance(const Item1,Item2: TBestMove):integer;
begin
  if abs(TargetMoveValue-Item1.Value)<abs(TargetMoveValue-Item2.Value) then
    Result:=-1 else
  if abs(TargetMoveValue-Item1.Value)>abs(TargetMoveValue-Item2.Value) then
    Result:=1 else
    Result:=CompareText(Item1.Text,Item2.Text);
end;

function SortByCountAndValue(const Item1,Item2:TBestMove):integer;
begin
  if (Item1.Letters.Count=round(TargetMoveValue)) then
  begin
    if (Item2.Letters.Count<TargetMoveValue) then
      Result:=-1 else
    begin
      if (Item1.Value<Item2.Value) then
        Result:=1 else
      if (Item1.Value>Item2.Value) then
        Result:=-1 else
        Result:=CompareText(Item1.Text,Item2.Text);
    end;
  end else
  if (Item2.Letters.Count=round(TargetMoveValue)) then
    Result:=1 else
  begin
    if (Item1.Value<Item1.Value) then
      Result:=1 else
    if (Item1.Value>Item2.Value) then
      Result:=-1 else
      Result:=CompareText(Item2.Text,Item1.Text);
  end;
end;

function TBruteForce.SortBestMoves:word;

 function HasJoker(index:integer):boolean;
 var i:integer;
 begin
   Result:=false;
   with FBestMoves[index] do
    for i:=0 to Letters.Count-1 do
     if Letters[i].IsJoker then Result:=true;
 end;

var
  i,j,k: integer;
  aMove,bMove: TBestMove;
  aList,bList: TBestMoveList;
  equal: boolean;
begin
  if BestMovesCount>0 then
  begin
    //clear duplicates (if R was placed, both AT and RAT are valid at same position with same result)
    for i:=FBestMoves.Count-1 downto 1 do
    begin
      aMove:=TBestMove(FBestMoves[i]);
      for j:=0 to i-1 do
      begin
        bMove:=TBestMove(FBestMoves[j]);
        if (bMove.Text=aMove.Text) and
           (bMove.Position.Position=aMove.Position.Position) and
           (bMove.Position.Axis=aMove.Position.Axis) and
           (bMove.Value=aMove.Value) and
           (bMove.Letters.Count=aMove.Letters.Count) then
        begin
          equal:=true;
          for k:=0 to aMove.Letters.Count-1 do
           equal:=equal and
            (bMove.Letters[k].What=aMove.Letters[k].What) and
            (bMove.Letters[k].Where[dx]=aMove.Letters[k].Where[dx]) and
            (bMove.Letters[k].Where[dy]=aMove.Letters[k].Where[dy]) and
            (bMove.Letters[k].Where[dz]=aMove.Letters[k].Where[dz]);
          if equal then
          begin
            for k:=0 to aMove.Letters.Count-1 do
              aMove.Letters[k].Free;
            aMove.Letters.Clear;
            aMove.Letters.Free;
            aMove.Free;
            FBestMoves.Delete(i);
            break;
          end;
        end;
      end;
    end;

    //sort
    FBestMoves.Sort(@SortByValue);
    Result:=round(FBestMoves[0].Value);

    if (FSettings.Performance<100) then //only set in call by computer itself
    begin
      case FSettings.Deterministic of
       true :  begin
                 TargetMoveValue:=Result-Result*((100-FSettings.Performance)/100);
                 FBestMoves.Sort(@SortByPerformance);
               end;
       false : begin
                 aList:=TBestMoveList.Create;
                 bList:=TBestMoveList.Create;
                 try
                   for i:=0 to FBestMoves.Count-1  do
                    if random(100)<FSettings.Performance then
                     aList.Add(FBestMoves[i]) else
                     bList.Add(FBestMoves[i]);
                   FBestMoves.Clear;
                   for i:=0 to aList.Count-1 do FBestMoves.Add(aList[i]);
                   for i:=0 to bList.Count-1 do FBestMoves.Add(bList[i]);
                 finally
                   aList.Free;
                   bList.Free;
                 end;
               end;
      end; //case
    end;

    if (Scrabble.ActualMove=Scrabble.CurrentMove) and
       ((Scrabble.Player[Scrabble.CurrentPlayer].Name<>'Computer') or
        (FSettings.Performance=100)
       ) then
    begin
      //all letters first at game end
      if (Scrabble.NumberOfLettersLeft=0) then
      begin
        TargetMoveValue:=0;
        for i:=0 to Scrabble.RackSize-1 do
         if (Scrabble.RackLetter[Scrabble.LocalPlayer,i]<>nil) then
          TargetMoveValue:=TargetMoveValue+1;
        FBestMoves.Sort(@SortByCountAndValue);

        //keep at least one letter if game wouldn't be won
        if (TargetMoveValue>1) and
           (RackLetterCount(FBestMoves[0])=TargetMoveValue) then
         for i:=0 to Scrabble.NumberOfPlayers-1 do
          if (i<>Scrabble.CurrentPlayer) and
             (Scrabble.Player[i].Points>Scrabble.Player[Scrabble.CurrentPlayer].Points+FBestMoves[0].Value) then
        begin
          TargetMoveValue:=TargetMoveValue-1;
          FBestMoves.Sort(@SortByCountAndValue);
          break;
        end;
      end else
      //use word with joker only in advance of points
      if HasJoker(0) then
      begin
        i:=1;
        while (i<FBestMoves.Count) and HasJoker(i) do inc(i);
        if i<FBestMoves.Count then //maybe all words have joker
         if FBestMoves[i].Value>FBestMoves[0].Value-FSettings.UseJoker then
           FBestMoves.Move(i,0);
      end;
    end;
    FListPos:=MaxInt; //do not show random letters
  end else Result:=0;
end;

{ TSingleOperation }

constructor TSingleOperation.Create(aDim:TActiveDimension;aColRow:byte;aVertical,aLeftToRight:boolean);
var
  i: integer;
  aLetter: TLetter;
begin
  inherited Create(Scrabble);
  FAborted:=false;
  setlength(FRackLetters,RackSize);
  for i:=0 to LetterList.Count-1 do
  begin
    aLetter:=LetterList[i];
    if (aLetter.State=lsRack) and
       (aLetter.Who=(Scrabble.ActualMove mod Scrabble.NumberOfPlayers)) then
      FRackLetters[aLetter.RackPos]:=aLetter;
  end;
  FBestMoves:=TBestMoveList.Create;
  FDummy:=TStringList.Create;
  FActiveDimension:=aDim;
  FColRow:=aColRow;
  FVertical:=aVertical;
  FPlacedLetters:=TLetterList.Create;
  FLeftToRight:=aLeftToRight;
  FThread:=nil;
end;

destructor TSingleOperation.Destroy;
begin
  FDummy.Free;
  FBestMoves.Free;//don't clear since content was added to bruteforce.bestmoves
  FPlacedLetters.Free;
  setlength(FRackLetters,0);
  inherited Destroy;
end;

procedure TSingleOperation.ClearBoard;
var
  i: integer;
  aLetter: TLetter;
begin
  //clear board
  for i:=0 to FPlacedLetters.Count-1 do
  begin
    aLetter:=FPlacedLetters[i];
//    if aLetter.State=lsBoard then
    begin
      BoardLetter[aLetter.Where[dx],aLetter.Where[dy],aLetter.Where[dz]]:=nil;
      aLetter.Where[dx]:=255;
      aLetter.Where[dy]:=255;
      aLetter.Where[dz]:=255;
      if aLetter.IsJoker then aLetter.What:=ltJoker;
      aLetter.When:=255;
      aLetter.State:=lsRack;
    end;
  end;
  //clear placed letters list
  FPlacedLetters.Clear;
end;

function TSingleOperation.GetAvailableLetters: string;
var x,y,z : byte;
    i     : integer;
    aLetter : TLetter;
begin
  //collect placed letters in a row or col
  Result:='';
  for i:=0 to RackSize-1 do
  begin
    aLetter:=RackLetter[i];
    if aLetter<>nil then Result:=Result+UTF8Encode(widestring(aLetter.What));
  end;
  for i:=0 to BoardSize-1 do
  begin
    case FVertical of
     true : Scrabble.Convert2DTo3D(i,FColRow,x,y,z,FActiveDimension);
     false: Scrabble.Convert2DTo3D(FColRow,i,x,y,z,FActiveDimension);
    end;
    aLetter:=BoardLetter[x,y,z];
    if aLetter<>nil then
      Result:=Result+UTF8Encode(widestring(aLetter.What));
  end;
end;

procedure TSingleOperation.SetRackLetter(aRackPos: byte; AValue: TLetter);
begin
  FRackLetters[aRackPos]:=aValue;
end;

function TSingleOperation.GetRackLetter(aRackPos: byte): TLetter;
begin
  if (FRackLetters[aRackPos]=nil) or
     (FRackLetters[aRackPos].State<>lsRack) then
    Result:=nil else
    Result:=FRackLetters[aRackPos];
end;

function TSingleOperation.TryToPlaceWord(aValue: string; aPos: byte): boolean;
var
  x,y,z   : byte;
  i,j     : integer;
  aWord : widestring;
  tmp : TLetter;
begin
  Result:=false;
  aWord:=UTF8Decode(aValue);
  for i:=1 to length(aWord) do
  begin
    case FVertical of
     true : if not FLeftToRight then
              Scrabble.Convert2DTo3D(BoardSize-(i-1+aPos)-1,FColRow,x,y,z,FActiveDimension) else
              Scrabble.Convert2DTo3D(i-1+aPos,FColRow,x,y,z,FActiveDimension);
     false: Scrabble.Convert2DTo3D(FColRow,i-1+aPos,x,y,z,FActiveDimension);
    end;
    if (BoardLetter[x,y,z]=nil) then
    begin
      //get piece from rack
      tmp:=nil;
      if tmp=nil then
       for j:=0 to RackSize-1 do
       begin
         tmp:=RackLetter[j];
         if (tmp<>nil) and (tmp.What=aWord[i]) then break else tmp:=nil;
       end;
      if tmp=nil then //letter not found, trying joker
       for j:=0 to RackSize-1 do
       begin
         tmp:=RackLetter[j];
         if (tmp<>nil) and (tmp.IsJoker) then break else tmp:=nil;
       end;
      //apply if found
      if tmp<>nil then
      begin
        tmp.State:=lsBoard;
        if tmp.IsJoker then tmp.What:=aWord[i];
        tmp.Where[dx]:=x; tmp.Where[dy]:=y; tmp.Where[dz]:=z;
        tmp.When:=Scrabble.ActualMove;
        BoardLetter[x,y,z]:=tmp;
        FPlacedLetters.Add(tmp);//what and where are stored in a letter and placed after comparison with Rack in placebestmove
      end else
        exit;
    end else
      if BoardLetter[x,y,z].What<>aWord[i] then
        exit;
  end;
  Result:=true;
end;

function TSingleOperation.ShuffleJoker(var start:byte): boolean;
var
  x,y,z : byte;
  aLetter: TLetter;
  i,j: integer;
begin
  for i:=start to RackSize-1 do
   if (FRackLetters[i]<>nil) and
      (FRackLetters[i].IsJoker) and
      (FRackLetters[i].State=lsBoard) then
    for j:=0 to RackSize-1 do
     if (i<>j) and
        (FRackLetters[j]<>nil) and
        (not FRackLetters[j].IsJoker) and
        (FRackLetters[j].State=lsBoard) and
        (FRackLetters[i].What=FRackLetters[j].What) then
     begin
       //swap placed letters
       x:=FRackLetters[i].Where[dx];
       y:=FRackLetters[i].Where[dy];
       z:=FRackLetters[i].Where[dz];
       FRackLetters[i].Where[dx]:=FRackLetters[j].Where[dx];
       FRackLetters[i].Where[dy]:=FRackLetters[j].Where[dy];
       FRackLetters[i].Where[dz]:=FRackLetters[j].Where[dz];
       FRackLetters[j].Where[dx]:=x;
       FRackLetters[j].Where[dy]:=y;
       FRackLetters[j].Where[dz]:=z;
       //update board
       BoardLetter[x,y,z]:=FRackLetters[j];
       with FRackLetters[i] do
         BoardLetter[Where[dx],Where[dy],Where[dz]]:=FRackLetters[i];
       //adjust FRackLetters for shufflejokers
       aLetter:=FRackLetters[i];
       FRackLetters[i]:=FRackLetters[j];
       FRackLetters[j]:=aLetter;
       FRackLetters[i].RackPos:=FRackLetters[j].RackPos;
       FRackLetters[j].RackPos:=aLetter.RackPos;
       start:=i+1;
       exit(true);
     end;
  Result:=false;//nothing to swap
end;

function TSingleOperation.IsKnownWord(const aValue: string;
  var aNotFound: string; DoAsk: boolean): boolean;
var
  b: boolean;
  i,j: integer;
begin
  Result:=true;
  FDummy.CommaText:=aValue;
  if (FThread<>nil) and (gsCLABBERS in Scrabble.GameState) then
    System.EnterCriticalsection(Criticalsection); //Dictionary.IsWordInDictionary needs thread awareness but enter/leave cs takes much time
  try
    for i:=0 to FDummy.Count-1 do
    if (FDummy[i]<>'') then
    begin
      if (gsCLABBERS in Scrabble.GameState) then
      begin
        with TStringList.Create do
        try
          b:=false;
          CommaText:=Dictionary.WordsByLetters(FDummy[i]);
          for j:=0 to Count-1 do
          if (UTF8Length(Strings[j])=UTF8Length(FDummy[i])) then
          begin
            b:=true;
            break;
          end;
          if not b then
            exit(false);
        finally
          Free;
        end;
      end else //CLABBERS
        if (Dictionary.IsWordInDictionary(FDummy[i],j)<>frInclude) then
          exit(false);
    end; //for i to FDummy.Count
  finally
    if (FThread<>nil) and (gsCLABBERS in Scrabble.GameState) then
      System.LeaveCriticalsection(Criticalsection);
  end;
end;

procedure TSingleOperation.Run;
var
  i: integer;
  j,k: byte;
  start: byte;
  IsValid: boolean;
  aBestMove: TBestMove;
  aLetter: TLetter;
  {$ifdef PerformanceDebug} z:longword;{$endif}
begin
  with TStringList.Create do    //TStringList to store words combined of available letters
  try
    {$ifdef PerformanceDebug} z:=GetTickCount;{$endif}
    if FThread<>nil then
      System.EnterCriticalsection(Criticalsection);
    Commatext:=Dictionary.WordsByLetters(GetAvailableLetters);
    if FThread<>nil then
      System.LeaveCriticalsection(Criticalsection);
    {$ifdef PerformanceDebug} inc(zWbL,GetTickCount-z);inc(zWbLc); {$endif}
    for i:=0 to Count-1 do                                  //try to place found words for a) each word
     for j:=0 to BoardSize-UTF8Length(Strings[i]) do        //                             b) every position
     begin
       if FAborted then exit;
       {$ifdef PerformanceDebug} z:=GetTickCount; {$endif}
       IsValid:=TryToPlaceWord(Strings[i],j);
       start:=0;//shuffle start
       {$ifdef PerformanceDebug} inc(zTtP,GetTickCount-z);inc(zTtPc); {$endif}
       if IsValid then
       repeat
         {$ifdef PerformanceDebug} z:=GetTickCount; {$endif}
         if (CheckMove=leNone) then
         begin
           aBestMove:=TBestMove.Create;
           aBestMove.Value:=LastMove.Value;
           aBestMove.Position:=FActiveDimension;
           aBestMove.Text:=LastMove.PlacedWord;
           aBestMove.Letters:=TLetterList.Create;
           for k:=0 to FPlacedLetters.Count-1 do
           begin
             aLetter:=TLetter.Create;   //FPlacedLetters does not create letters but pointers to board
             aLetter.AssignLetter(FPlacedLetters[k]);
             aBestMove.Letters.Add(aLetter);
           end;
           FBestMoves.Add(aBestMove);
         end;
         {$ifdef PerformanceDebug} inc(zCM,GetTickCount-z);inc(zCMc); {$endif}
       until (LastError<>leNone) or not ShuffleJoker(start);       //repeat if joker could be placed at another position
       ClearBoard;                   //remove placed letters from fboard and fletters
     end;
  finally
    InterlockedDecrement(ThreadsRunning);
    Free;
  end;//TStringList
end;

{ TSingleThread }

constructor TSingleThread.Create;
begin
  InterlockedIncrement(ThreadsRunning);
  inherited Create(true); //suspended
  FreeOnTerminate:=true;
end;

destructor TSingleThread.Destroy;
begin
  FOperation:=nil;
  inherited Destroy;
end;

procedure TSingleThread.Execute;
begin
  FOperation.Thread:=self;
  FOperation.Run;
end;

end.
