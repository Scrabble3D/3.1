{Copyright:      Hagen Reddmann  mailto:HaReddmann@T-Online.de
 Author:         Hagen Reddmann
 Remarks:        All rights reserved
 Version:        Shareware, use only with permission of the author
                 Delphi 5, designed and testet under D5
 Description:    Directed Acyclic Word Graph Dictionary
                 usefull for wordgames such as crosswords or scrabble
}
unit udawg;

{$mode delphi}{$H+}

interface

uses Classes;

type
  PByte            = ^Byte;

  PDawgNode        = ^TDawgNode;
  TDawgNode        = Cardinal;

  TDawgNodeList    = array of TDawgNode;

  TDawgSymbol      = Byte;
  TDawgChar        = Char;
  TDawgCharSet     = set of TDawgChar;
  TDawgMapping     = array[TDawgChar] of TDawgChar;
  TDawgSymbolCount = array[TDawgSymbol] of Cardinal;

  PDawgNodeArray   = ^TDawgNodeArray;
  TDawgNodeArray   = array[0..MaxInt shr 2 -1] of TDawgNode;

  TDawgMode        = (dmEmpty, dmDAWG, dmPackedDAWG);

  TEnumWordsCallback = function(UserData: Pointer; Word: PChar; WordLength: Integer): Boolean; register;

  TDawg = class(TInterfacedObject)
  private
    FTrie: PDawgNodeArray;      // our node trie
    FTrieSize: TDawgNode;
    FNodeCount: TDawgNode;      // highest free node index in FTrie
    FFreeRoot: TDawgNode;       // first free unused node
    FFreeNodes: TDawgNode;      // count of free nodes
    FRootNode: TDawgNode;       // start of our trie
    FMaxLength: Integer;        // max char length of words
    FCount: Integer;            // count of words in our trie
    FMode: TDawgMode;
    FMap: array[TDawgSymbol] of TDawgNode;
    FWordLength: Integer;       // current word length of Insert()
    FNodeIndex: TDawgNode;      // current node of Insert()
    FParentIndex: TDawgNode;    // current parent node of Insert()
    procedure SetMode(Value: TDawgMode);
  protected
    function  AllocFree(Length: Cardinal = 1): TDawgNode;
    procedure InsertFree(Index,Length: TDawgNode);
    procedure Expand(Amount: Cardinal = 0);
    function  CreateSymbols(Pattern: PByte; Length: Integer): TDawgNodeList;
  public
    constructor Create;
    destructor Destroy; override;

    function Size: Integer;

    procedure Insert(Buffer: PByte = nil; BufferSize: Integer = 0);
    procedure InsertString(const Word: String);
    procedure InsertStrings(Strings: TStrings);

    procedure Clear;
    procedure Unpack;
    procedure Pack;

    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: String);

    procedure SaveWordsToStream(Stream: TStream; const Separator: String = #13#10);
    procedure SaveWordsToFile(const FileName: String; const Separator: String = #13#10);
    procedure LoadWordsFromStream(Stream: TStream);
    procedure LoadWordsFromFile(const FileName: String);

    procedure SetMapping(const Chars: TDawgMapping;
                         const Terminators: TDawgCharSet = [#0,#10,#13];
                         const Ignore: TDawgCharSet = [];
                         const Wildcards: String = '*?#');

    function Exists(const Pattern: String): Boolean; overload;
    function Exists(Pattern: PByte; Length: Integer): Boolean; overload;

    function Enum(Callback: TEnumWordsCallback; UserData: Pointer = nil): String;
    function SymbolCount(var Counter: TDawgSymbolCount): Cardinal;

    function Search(const Pattern: String; Found: TDawg; WithLength: Integer = 0): Boolean;
    function SearchCombinatoric(const Pattern: String; Found: TDawg; MinLength: Integer = 2): Boolean;

    property Count: Integer read FCount;
    property Mode: TDawgMode read FMode write SetMode default dmEmpty;
  end;

implementation

uses SysUtils;

const
  NodeBits     = SizeOf(TDawgNode) * 8;
  SymbolBits   = SizeOf(TDawgSymbol) * 8;
  SymbolShift  = NodeBits - SymbolBits;
  SymbolMask   = (1 shl SymbolBits -1) shl SymbolShift;
  EOW          = 1 shl (SymbolShift -1);  // End of Word
  EON          = 1 shl (SymbolShift -2);  // End of Node
  NodePtrShift = SymbolShift -3;
  MaxNodeCount = 1 shl NodePtrShift;
  NodePtrMask  = MaxNodeCount -1;
  NodeEmpty    = $FFFFFFFF;
  MaxFreeLen   = 1 shl (NodeBits - NodePtrShift) -1;

  Terminator    = $FFFFFFFF;    // terminators for Words, like #0, #10, #13 or ',;.'
  NullSymbol    = $00FFFFFF;    // ignored symbols and mask for special symbols
  Asterisk      = $01FFFFFF;    // multi letter wildcard  '*'
  Wildcard      = $02FFFFFF;    // single letter wildcard '?'
  Numeric       = $03FFFFFF;    // single number wildcard '#'

{  TDawgNode
     Symbol: Byte;
     Next: array[0..2] of Byte; // 22 Bits Offset
     Link: Cardinal;
   end;

   TLink = packed record
     Index: Cardinal;    // Dokumentenindex
     Position: Cardinal; // 1. position des Wortes
   end;

   TDokument = packed record
     Name: array[0..260] of Char;
   end;

}
const
  sDAWGIdent: String[7] = 'DAWG'#$0A#0#0;

procedure TDawg.SetMode(Value: TDawgMode);
begin
  if Value <> FMode then
    case Value of
      dmEmpty: Clear;
//      dmTrie: MakeTrie;
      dmDAWG: Unpack;
      dmPackedDAWG: Pack;
    end;
end;

procedure TDawg.SaveWordsToStream(Stream: TStream; const Separator: String);
// Save the DAWG as text file each word separated by Separator.
// We use an iterative trie walking, it's not faster as recursive version,
// but seem's as more elegant.
// Need about 180 ms for an packed DAWG with 544.000 words on P4 1.5GHz
// and the word text file are 6.41 Mb big.
// The packed DAWG need 1.59 Mb.
const
  BufSize = 1024 * 4;
var
  Buffer: String;
  Stack: TDawgNodeList;
  StackPos: TDawgNode;
  Node: TDawgNode;
  Word,Sep,Buf: PByteArray;
  SepLen,BufPos,I: Integer;
begin
  if FCount <= 0 then Exit;
  Sep := Pointer(Separator);
  SepLen := Length(Separator);
  SetLength(Buffer, (FMaxLength + SepLen) * 2 + BufSize);
  Word := Pointer(Buffer);
  Buf := @Word[FMaxLength];
  BufPos := 0;
  SetLength(Stack, FMaxLength);
  Stack[0] := FRootNode;
  StackPos := 0;
  Node := FTrie[FRootNode];
  repeat
    Word[StackPos] := Node shr SymbolShift;
    if Node and EOW <> 0 then
    begin
      for I := 0 to StackPos do Buf[BufPos + I] := Word[I];
      Inc(BufPos, StackPos +1);
      for I := 0 to SepLen -1 do Buf[BufPos + I] := Sep[I];
      Inc(BufPos, SepLen);
      if BufPos >= BufSize then
      begin
        Stream.WriteBuffer(Buf[0], BufPos);
        BufPos := 0;
      end;
    end;
    Node := Node and NodePtrMask;
    if Node <> 0 then
    begin
      Inc(StackPos);
      Stack[StackPos] := Node;
      Node := FTrie[Node];
    end else
    begin
      while FTrie[Stack[StackPos]] and EON <> 0 do
      begin
        if StackPos = 0 then
        begin
          if BufPos > 0 then Stream.WriteBuffer(Buf[0], BufPos);
          Exit;
        end;
        Dec(StackPos);
      end;
      Inc(Stack[StackPos]);
      Node := FTrie[Stack[StackPos]];
    end;
  until False;
end;

procedure TDawg.SaveWordsToFile(const FileName: String; const Separator: String = #13#10);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveWordsToStream(Stream, Separator);
  finally
    Stream.Free;
  end;
end;

procedure TDawg.LoadWordsFromStream(Stream: TStream);
// load words from a stream
const
  BufSize = 1024 * 4;
var
  Buffer: String;
  Buf: PByte;
  BufLen: Integer;
begin
  try
    SetLength(Buffer, BufSize);
    Buf := Pointer(Buffer);
    repeat
      BufLen := Stream.Read(Buf^, BufSize);
      Insert(Buf, BufLen);
    until BufLen = 0;
    Insert;
  except
    Clear;
    raise;
  end;
end;

procedure TDawg.LoadWordsFromFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadWordsFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDawg.SaveToStream(Stream: TStream);
// save DAWG as binary
begin
  if FParentIndex <> 0 then Insert;
  Stream.WriteBuffer(sDAWGIdent[1], Length(sDAWGIdent));
  Stream.WriteBuffer(FMode, SizeOf(FMode));
  Stream.WriteBuffer(FCount, SizeOf(FCount));
  Stream.WriteBuffer(FMaxLength, SizeOf(FMaxLength));
  Stream.WriteBuffer(FRootNode, SizeOf(FRootNode));
  Stream.WriteBuffer(FFreeRoot, SizeOf(FFreeRoot));
  Stream.WriteBuffer(FFreeNodes, SizeOf(FFreeNodes));
  Stream.WriteBuffer(FNodeCount, SizeOf(FNodeCount));
  Stream.WriteBuffer(FMap, SizeOf(FMap));
  Stream.WriteBuffer(FTrie^, FNodeCount * SizeOf(TDawgNode));
end;

procedure TDawg.LoadFromStream(Stream: TStream);
// load binary DAWG

  procedure Error;
  begin
    raise Exception.Create('Invalid DAWG Stream Format');
  end;

var
  Ident: ShortString;
begin
  try
    SetLength(Ident, Length(sDAWGIdent));
    Stream.ReadBuffer(Ident[1], Length(sDAWGIdent));
    if Ident <> sDAWGIdent then Error;
    Stream.ReadBuffer(FMode, SizeOf(FMode));
    Stream.ReadBuffer(FCount, SizeOf(FCount));
    Stream.ReadBuffer(FMaxLength, SizeOf(FMaxLength));
    Stream.ReadBuffer(FRootNode, SizeOf(FRootNode));
    Stream.ReadBuffer(FFreeRoot, SizeOf(FFreeRoot));
    Stream.ReadBuffer(FFreeNodes, SizeOf(FFreeNodes));
    Stream.ReadBuffer(FNodeCount, SizeOf(FNodeCount));
    Stream.ReadBuffer(FMap, SizeOf(FMap));
    FTrieSize := FNodeCount;
    FreeMem(FTrie);
    GetMem(FTrie, FNodeCount * SizeOf(TDawgNode));
    Stream.ReadBuffer(FTrie^, FNodeCount * SizeOf(TDawgNode));
    FTrie[0] := EON or EOW;
    FWordLength := 0;
    FParentIndex := 0;
    FNodeIndex := 0;
  except
    Clear;
    raise;
  end;
end;

procedure TDawg.SaveToFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDawg.LoadFromFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;


function TDawg.SymbolCount(var Counter: TDawgSymbolCount): Cardinal;
// a iterative version is only marginal faster ~2%
type
  PDawgSymbolCount = ^TDawgSymbolCount;
  
  function DoCount(Counter: PDawgSymbolCount; Word: PByteArray; WordLength: Integer): Boolean; register;
  var
    I: Integer;
  begin
    for I := 0 to WordLength -1 do Inc(Counter^[Word[I]]);
    Result := False;
  end;

var
  I: Integer;
begin
  FillChar(Counter, SizeOf(Counter), 0);
  Enum(@DoCount, @Counter);
  Result := 0;
  for I := Low(Counter) to High(Counter) do Inc(Result, Counter[I]);
end;

constructor TDawg.Create;
begin
  inherited Create;
  Clear;
end;

destructor TDawg.Destroy;
begin
  FreeMem(FTrie);
  inherited Destroy;
end;

function TDawg.Size: Integer;
begin
  Result := FTrieSize * SizeOf(TDawgNode);
end;

procedure TDawg.Clear;
// Node at Index = 0 aren't used and must preserved to ensure that the other
// nodeindex works properly
var
  I: Cardinal;
begin
  ReallocMem(FTrie, SizeOf(TDawgNode));
  FTrie[0] := EON or EOW;
  FTrieSize := 1;
  FNodeCount := 1;
  FRootNode := 0;
  FFreeRoot := 0;
  FFreeNodes := 0;
  FMaxLength := 0;
  FWordLength := 0;
  FNodeIndex := 0;
  FParentIndex := 0;
  FCount := 0;
  for I := Low(FMap) to High(FMap) do
    FMap[I] := TDawgNode(I shl SymbolShift);
// setup word terminators
  FMap[ 0] := Terminator;
  FMap[10] := Terminator;
  FMap[13] := Terminator;
  FMap[Ord(',')] := Terminator;
  FMap[Ord(';')] := Terminator;
  FMap[Ord(':')] := Terminator;
  FMap[Ord('*')] := Asterisk;
  FMap[Ord('?')] := Wildcard;
  FMap[Ord('#')] := Numeric;
  FMode := dmEmpty;
end;

procedure TDawg.SetMapping(const Chars: TDawgMapping; const Terminators: TDawgCharSet;
                const Ignore: TDawgCharSet; const Wildcards: String);
var
  I: TDawgChar;
  J: Integer;
begin
  if FParentIndex <> 0 then Insert;
  FillChar(FMap, SizeOf(FMap), $FF);
  for I := Low(I) to High(I) do FMap[Ord(I)] := Ord(Chars[I]) shl SymbolShift;
  for I := Low(I) to High(I) do
  begin
    if I in Ignore then FMap[Ord(I)] := NullSymbol;
    if I in Terminators then FMap[Ord(I)] := Terminator;
  end;
  if Terminators = [] then FMap[0] := Terminator; // ensure at least one terminator exists
  for J := 1 to Length(Wildcards) do
    FMap[Ord(Wildcards[J])] := NullSymbol or (J shl SymbolShift);
end;

procedure TDawg.Expand(Amount: Cardinal = 0);
// expand FTrie memory if needed

  procedure Error;
  begin
    raise Exception.Create('The DAWG run out of space');
  end;

const
  GrowRatio = 1024; // must be a power of 2
begin
  Inc(Amount, FNodeCount);
  if Amount > MaxNodeCount then Error;
  Amount := (Amount + GrowRatio) and not (GrowRatio -1);
  if Amount > FTrieSize then
  begin
    FTrieSize := Amount;
    ReallocMem(FTrie, FTrieSize * SizeOf(TDawgNode));
  end;
end;

procedure TDawg.Unpack;
// unpack a packed DAWG trie
// - the final trie contains only nodes there have one parent node
// - the nodes are sorted ascending in memory
// - the trie contains no free nodes
//
// I tried inplaced unpacking, but that's not possible because on packed DAWG's,
// to each node can point more as one other node in any order. Such nodes are
// processed first after the actual processed nodes. So we don't known if there
// exists nodes to our current node and so we can't free it before have fully
// finished unpacking.
var
  New: PDawgNodeArray;
  NewIndex,NewSize: TDawgNode;

  function DoCopy(Index: TDawgNode): TDawgNode;
  var
    Count,Node,NodeIndex: TDawgNode;
  begin
    Result := NewIndex;
    Count := 0;
    while FTrie[Index + Count] and EON = 0 do Inc(Count);
    Inc(NewIndex, Count +1);
    if NewIndex > NewSize then
    begin
      NewSize := NewIndex + NewSize shr 1;
      ReallocMem(New, NewSize * SizeOf(TDawgNode));
    end;
    Count := 0;
    repeat
      Node := FTrie[Index + Count];
      NodeIndex := Node and NodePtrMask;
      if NodeIndex <> 0 then
        Node := Node and not NodePtrMask or DoCopy(NodeIndex);
      New[Result + Count] := Node;
      Inc(Count);
    until Node and EON <> 0;
  end;

begin
  New := nil;
  if FParentIndex <> 0 then Insert;
  if FMode = dmPackedDAWG then
  try
    NewSize := FNodeCount - FFreeNodes;
    GetMem(New, NewSize * SizeOf(TDawgNode));
    NewIndex := 1;
    FRootNode := DoCopy(FRootNode);
    ReallocMem(New, NewIndex * SizeOf(TDawgNode));
    FreeMem(FTrie);
    FTrie := New;
    FTrieSize := NewIndex;
    FNodeCount := NewIndex;
    FFreeNodes := 0;
    FFreeRoot := 0;
    FMode := dmDAWG;
  except
    FreeMem(New);
    Clear;
    raise;
  end;
end;

function IsPrime(Value: Cardinal): Boolean;
var
  i: Cardinal;
begin
  if Value <> 2 then
   for i:=2 to round(Sqrt(Value)) do
    if Value mod I = 0 then
      exit(false);
  Result:=Value>0;
end;

procedure TDawg.Pack;
// Pack a DAWG, in such a way that equal word prefixes and suffixes are shared.
// An unpacked DAWG shares only the same word prefixes.
// we must work here the nodes form FTrie[] to a new copy in New[]
// I tried some inplaced version, but all fails or was far slower
var
  Stack: PDawgNodeArray;
  StackSize,StackIndex: TDawgNode;
  New: PDawgNodeArray;
  NewIndex,NewSize: Cardinal;
  HashTable: PDawgNodeArray;
  HashTableSize: Cardinal;

  function DoCopy(Index: TDawgNode): TDawgNode;
  var
    Count,Node,NodeIndex,Hash: TDawgNode;
    I,Increment,HashIndex,HashEntry: TDawgNode;
  begin
    Hash := 0;
    Count := 0;
    NodeIndex := StackIndex;
    repeat
      Node := FTrie[Index + Count];
      Inc(Count);
      I := Node and NodePtrMask;
      if I <> 0 then Node := (Node and not NodePtrMask) or DoCopy(I);
      Hash := (Hash + Hash) xor Node;  // faster version as Hash := Rol(Hash, 1) xor Node
      if StackIndex >= StackSize then
      begin
        StackSize := StackIndex + StackSize shr 1;
        ReallocMem(Stack, StackSize * SizeOf(TDawgNode));
      end;
      Stack[StackIndex] := Node;
      Inc(StackIndex);
    until Node and EON <> 0;
    Hash := Hash mod HashTableSize;
    HashIndex := Hash;
    Increment := 17;
    repeat
      HashEntry := HashTable[HashIndex];
      if HashEntry = 0 then Break;
      I := 0;
      while (I < Count) and (New[HashEntry + I] = Stack[NodeIndex + I]) do Inc(I);
      if I = Count then
      begin
        Dec(StackIndex, Count);
        Result := HashEntry;
        Exit;
      end;
      Inc(HashIndex, Increment);
      if HashIndex >= HashTableSize then Dec(HashIndex, HashTableSize);
      Inc(Increment, 16);
      if Increment > HashTableSize then Dec(Increment, HashTableSize);
      if HashIndex = Hash then Abort; // Hashtable is full, we must do the work again with bigger table
    until False;
    I := NewIndex + Count;
    if I > NewSize then
    begin
      NewSize := I + NewSize shr 1;
      ReallocMem(New, NewSize * SizeOf(TDawgNode));
    end;
    for I := 0 to Count -1 do
      New[NewIndex + I] := Stack[NodeIndex + I];
    Dec(StackIndex, Count);
    HashTable[HashIndex] := NewIndex;
    Result := NewIndex;
    Inc(NewIndex, Count);
  end;

begin
  if FMode <> dmDAWG then Exit;
  if FParentIndex <> 0 then Insert;
  HashTable := nil;
  New := nil;
  StackSize := FMaxLength + FMaxLength;
  GetMem(Stack, StackSize * SizeOf(TDawgNode));
  try
    HashTableSize := FCount or 1;
    try
      NewSize := FTrieSize div 4;
      if NewSize < 1024 then NewSize := 1024;
      GetMem(New, NewSize * SizeOf(TDawgNode));
      repeat
        while not IsPrime(HashTableSize) do Inc(HashTableSize, 2);
        try
          ReallocMem(HashTable, HashTableSize * SizeOf(TDawgNode));
          FillChar(HashTable^, HashTableSize * SizeOf(TDawgNode), 0);
          NewIndex := 1;
          StackIndex := 0;
          FRootNode := DoCopy(FRootNode);
          FMode := dmPackedDAWG;
          Break;
        except
          on E: EAbort do Inc(HashTableSize, HashTableSize +1)
            else raise;
        end;
      until False;
      FreeMem(FTrie);
      FTrie := New;
      ReallocMem(New, NewIndex * SizeOf(TDawgNode));
      FTrieSize := NewIndex;
      FNodeCount := NewIndex;
      FFreeNodes := 0;
      FFreeRoot := 0;
    except
      FreeMem(New);
      Clear;
      raise;
    end;
  finally
    FreeMem(Stack);
    FreeMem(HashTable);
  end;
end;

function TDawg.AllocFree(Length: Cardinal): TDawgNode;
// find and allocate a free slot with length nodes
// if none found allocate at end of FTrie, if FTrie excides size grow it
var
  Next,Node,Prev,Len: TDawgNode;
begin
  Assert((Length > 0) and (Length <= MaxFreeLen));
  
  Prev := 0;
  Next := FFreeRoot;
  while Next <> 0 do
  begin
    Node := FTrie[Next];
    Len  := Node shr NodePtrShift;
    Node := Node and NodePtrMask;
    if Length <= Len then
    begin
      Result := Next;
      Dec(Len, Length);
      if Len > 0 then
      begin
        Inc(Next, Length);
        FTrie[Next] := Len shl NodePtrShift or Node;
        Node := Next;
      end;
      if Prev = 0 then FFreeRoot := Node
        else FTrie[Prev] := FTrie[Prev] and not NodePtrMask or Node;
      Dec(FFreeNodes, Length);
      Exit;
    end;
    Prev := Next;
    Next := Node;
  end;
  Result := FNodeCount;
  Inc(FNodeCount, Length);
  if FNodeCount > FTrieSize then Expand;
end;

procedure TDawg.InsertFree(Index,Length: TDawgNode);
// insert free space into free node list
// - we insert sorted here to speedup packing
// - we compact succesive free nodes
var
  Next,Prev,Node,Len: TDawgNode;
begin
  Assert((Length > 0) and (Length <= MaxFreeLen));

  Inc(FFreeNodes, Length);
  Prev := 0;
  Next := FFreeRoot;
  while Next <> 0 do
  begin
    Assert(Prev < Next);
    
    Node := FTrie[Next];
    Len := Node shr NodePtrShift;
// check if we can append free space
    if Next + Len = Index then
    begin
      Inc(Len, Length);
      if Len <= MaxFreeLen then
      begin
        FTrie[Next] := Len shl NodePtrShift or Node and NodePtrMask;
        Exit;
      end;
    end else
      if Next > Index then Break;
    Prev := Next;
    Next := Node and NodePtrMask;
  end;
// compact successive free nodes
  while Index + Length = Next do
  begin
    Node := FTrie[Next];
    Len := Length + Node shr NodePtrShift;
    if Len > MaxFreeLen then Break;
    Length := Len;
    Next := Node and NodePtrMask;
  end;
  FTrie[Index] := (Length shl NodePtrShift) or Next;
  if Prev = 0 then FFreeRoot := Index
    else FTrie[Prev] := FTrie[Prev] and not NodePtrMask or Index;
end;

procedure TDawg.Insert(Buffer: PByte; BufferSize: Integer);
// insert a buffer of words into Dawg
var
  Symbol,NodeIndex,NewIndex: TDawgNode;
label
  Start,InsertTerminator;
begin
  if FMode <> dmDAWG then
  begin
    Unpack;
    FMode := dmDAWG;
  end;
  if Buffer = nil then
  begin
    BufferSize := 0;
    goto InsertTerminator;
  end;
Start:
  while BufferSize > 0 do
  begin
    Symbol := FMap[Buffer^];
    Dec(BufferSize);
    Inc(Buffer);
    if Symbol and NullSymbol = 0 then
    begin
   // symbol is a valid symbol, find or insert it
      Inc(FWordLength);
      if FNodeIndex = 0 then
      begin
   // Node have no child, append symbol as new childnode
        NodeIndex := AllocFree;
        if FParentIndex <> 0 then FTrie[FParentIndex] := FTrie[FParentIndex] or NodeIndex
          else FRootNode := NodeIndex;
        FTrie[NodeIndex] := Symbol or EON;
        FParentIndex := NodeIndex;
      end else
      begin
   // scan trie for existing symbol
        NodeIndex := FNodeIndex;
        repeat
          if FTrie[NodeIndex] and SymbolMask = Symbol then
          begin // we have found it
            FNodeIndex := FTrie[NodeIndex] and NodePtrMask;
            FParentIndex := NodeIndex;
            goto Start;
          end else
            if FTrie[NodeIndex] and EON = 0 then Inc(NodeIndex)
              else Break;
        until False;
    // symbol must be inserted as new node, allocate space and update parent node
        NewIndex := AllocFree(NodeIndex +2 -FNodeIndex);
        if FParentIndex > 0 then
          FTrie[FParentIndex] := FTrie[FParentIndex] and not NodePtrMask or NewIndex
        else FRootNode := NewIndex;
    // copy old smaller node to new node
        NodeIndex := FNodeIndex;
        repeat
          if (Symbol <> Terminator) and (FTrie[NodeIndex] and SymbolMask >= Symbol) then
          begin
            FTrie[NewIndex] := Symbol;
            FParentIndex := NewIndex;
            Symbol := Terminator;
            Inc(NewIndex);
          end;
          if FTrie[NodeIndex] and EON <> 0 then Break;
          FTrie[NewIndex] := FTrie[NodeIndex];
          Inc(NodeIndex);
          Inc(NewIndex);
        until False;
        if Symbol <> Terminator then
        begin // new symbol is last entry of node
          FTrie[NewIndex] := FTrie[NodeIndex] and not EON;
          Inc(NewIndex);
          FTrie[NewIndex] := Symbol or EON;
          FParentIndex := NewIndex;
        end else FTrie[NewIndex] := FTrie[NodeIndex];
     // insert old node into free list
        InsertFree(FNodeIndex, NodeIndex + 1 - FNodeIndex);
        FNodeIndex := 0;
      end;
    end else
      if Symbol = Terminator then
      begin
   InsertTerminator:
        if FTrie[FParentIndex] and EOW = 0 then
        begin
     // symbol terminates current word
          FTrie[FParentIndex] := FTrie[FParentIndex] or EOW;
          if FWordLength > FMaxLength then FMaxLength := FWordLength;
          Inc(FCount);
        end;
        FWordLength := 0;
        FParentIndex := 0;
        FNodeIndex := FRootNode;
      end;
  end;
end;

procedure TDawg.InsertString(const Word: String);
begin
// insert chars
  Insert(PByte(Word), Length(Word));
// and terminate word, don't relay on LongString's null terminator
  Insert;
end;

procedure TDawg.InsertStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count -1 do InsertString(Strings[I]);
end;

function TDawg.CreateSymbols(Pattern: PByte; Length: Integer): TDawgNodeList;
// map pattern to internal used symbols
// we remove all succesive terminator and asterisk symbols
var
  Next,Prev: TDawgNode;
  I,J: Integer;
begin
  SetLength(Result, Length +1);
  J := 0;
  Prev := Terminator;
  for I := 0 to Length -1 do
  begin
    Next := FMap[Pattern^];
    Inc(Pattern);
    if (Next <> Prev) or ((Prev <> Asterisk) and (Prev <> Terminator)) then
    begin
      Result[J] := Next;
      Prev := Next;
      Inc(J);
    end;
  end;
  Result[J] := Terminator;
  SetLength(Result, J +1);
end;

function TDawg.Enum(Callback: TEnumWordsCallback; UserData: Pointer): String;
// Enum trough and build all words of the DAWG until Callback returns TRUE.
// If it returns TRUE then the actual Word is returned otherwise empty.
// About 33% faster as an recursive version.
var
  Stack: TDawgNodeList;
  StackPos: TDawgNode;
  Node: TDawgNode;
  Word: array of Byte;
begin
  if (FCount = 0) or not Assigned(Callback) then Exit;
  SetLength(Word, FMaxLength * 3 +1);
  SetLength(Stack, FMaxLength);
  Stack[0] := FRootNode;
  StackPos := 0;
  Node := FTrie[FRootNode];
  repeat
    Word[StackPos] := Node shr SymbolShift;
    if Node and EOW <> 0 then
    begin
      Inc(StackPos);
      Word[StackPos] := 0;
      if Callback(UserData, Pointer(Word), StackPos) then
      begin
        SetString(Result, PChar(Word), StackPos);
        Exit;
      end;
      Dec(StackPos);
    end;
    Node := Node and NodePtrMask;
    if Node <> 0 then
    begin
      Inc(StackPos);
      Stack[StackPos] := Node;
      Node := FTrie[Node];
    end else
    begin
      while FTrie[Stack[StackPos]] and EON <> 0 do
      begin
        if StackPos = 0 then Exit;
        Dec(StackPos);
      end;
      Inc(Stack[StackPos]);
      Node := FTrie[Stack[StackPos]];
    end;
  until False;
end;

function TDawg.Search(const Pattern: String; Found: TDawg; WithLength: Integer): Boolean;
// search all words to pattern and insert into Dawg Found
// sample patterns
//  '*A*' , search any words with one or more 'A'
//  'A*'  , search words with leading 'A'
//  '*A'  , search words with trailing 'A'
//  '?A*' , search words with second char is 'A'
//  '*#*' , search words with a number
// Patterns can be concatenated, like
//  '*A,A*,*B,*B'

{ follow some benchmarks,
  - used a Dawg with 200023 german words,
  - this Dawg need 811 Kb memory, as text file it require 2.54 Mb
  - my machine is a P4 1.5 GHz 512 Mb
  - loading this textfile wordlist with .LoadWordsFromFile() take 127 ms
  - packing this Dawg take 134 ms, so both actions take 261 ms
  - Dawg binary load 4 ms, save 23 ms
  - unpacking with .Unpack take 32 ms
  - save this Dawg as textfile wordlist take 71 ms

pattern                    time      entries found

"haus"                   0.003 ms,          1
"haus?"                  0.004 ms,          2
"haus??"                 0.008 ms,          5
"haus???"                0.014 ms,          7
"haus????"               0.032 ms,         35
"haus?????"              0.039 ms,         37
"haus*"                  0.211 ms,        333
"haus*e"                 0.122 ms,         65
"haus*e*"                0.300 ms,        258
"haus?e*"                0.040 ms,         65
"?haus"                  0.010 ms,          0
"??haus"                 0.073 ms,          1
"???haus"                0.490 ms,          4
"????haus"               1.454 ms,         27
"?????haus"              2.899 ms,         31
"*haus"                 41.880 ms,        144
"*haus*"                42.224 ms,        672
"?a*haus*"               5.579 ms,         70
"*a*haus*"              66.794 ms,        136
"*a*haus*,*b*haus*"    118.996 ms,        172
"a*"                    14.242 ms,      21493
"k*"                     7.241 ms,      10373
"z*"                     5.541 ms,       8333
"*a"                    40.221 ms,        828
"*k"                    40.719 ms,       1709
"*z"                    40.697 ms,       1116
"*a*"                  126.564 ms,      97243
"*k*"                   73.401 ms,      41656
"*z*"                   61.417 ms,      27220
"#*"                     0.003 ms,          0
"*#*"                   43.483 ms,          0
"*#"                    40.526 ms,          0
"*"                    146.523 ms,     200023
"?"                      0.007 ms,         15
"??"                     0.060 ms,        121
"???"                    0.307 ms,        511
"????"                   1.316 ms,       1672
"?????"                  3.588 ms,       3917
"??????"                 6.868 ms,       6810
"???????"               10.748 ms,       9724
"????????"              16.141 ms,      13943
"?????????"             22.812 ms,      19172
"??????????"            28.357 ms,      22876
"???????????"           32.665 ms,      25113
"?,??"                   0.063 ms,        136
"?,??,???"               0.377 ms,        647
"?,??,???,????"          1.717 ms,       2319

follow searches search only words with 7 chars, eg. param WithLength = 7

"haus"                   0.002 ms,          0
"haus?"                  0.003 ms,          0
"haus??"                 0.006 ms,          0
"haus???"                0.015 ms,          7
"haus????"               0.011 ms,          0
"haus?????"              0.011 ms,          0
"haus*"                  0.013 ms,          7
"haus*e"                 0.019 ms,          0
"haus*e*"                0.021 ms,          3
"haus?e*"                0.008 ms,          2
"?haus"                  0.010 ms,          0
"??haus"                 0.071 ms,          0
"???haus"                0.478 ms,          4
"????haus"               1.469 ms,          0
"?????haus"              2.890 ms,          0
"*haus"                  9.834 ms,          4
"*haus*"                 9.776 ms,         15
"?a*haus*"               1.401 ms,          1
"*a*haus*"              13.499 ms,          1
"*a*haus*,*b*haus*"     24.725 ms,          2
"a*"                     0.835 ms,        775
"k*"                     0.535 ms,        572
"z*"                     0.292 ms,        304
"*a"                     9.511 ms,        124
"*k"                     9.389 ms,         87
"*z"                     9.319 ms,         56
"*a*"                   14.088 ms,       3753
"*k*"                   11.280 ms,       1497
"*z*"                   10.491 ms,        849
"#*"                     0.002 ms,          0
"*#*"                    9.892 ms,          0
"*#"                    10.190 ms,          0
"*"                     11.265 ms,       9724

}

var
  HasFound: Boolean;
  Symbols: TDawgNodeList;
  Word: PByteArray;

  procedure DoSearch(Index: TDawgNode; SymbolPos: TDawgNode; WordLength: Integer);

    procedure DoFound;
    begin
      if (WithLength < 0) or (WordLength = WithLength) then
      begin
        Found.Insert(PByte(Word), WordLength +1);
        Found.Insert;
        HasFound := True;
      end;
    end;

  var
    Node,Next,Symbol: TDawgNode;
    Terminates: Boolean;
  begin
    Symbol := Symbols[SymbolPos];
    if (Symbol = Terminator) or ((WithLength >= 0) and (WordLength > WithLength)) then Exit;
    if Symbol = Asterisk then
    begin
      if Symbols[SymbolPos +1] <> Terminator then
      begin
        DoSearch(Index, SymbolPos +1, WordLength);
        repeat
          Node := FTrie[Index];
          Inc(Index);
          Next := Node and NodePtrMask;
          if Next <> 0 then
          begin
            Word[WordLength] := Node shr SymbolShift;
            DoSearch(Next, SymbolPos, WordLength +1);
          end;
        until Node and EON <> 0;
      end else
      begin
        repeat
          Node := FTrie[Index];
          Inc(Index);
          Word[WordLength] := Node shr SymbolShift;
          if Node and EOW <> 0 then DoFound;
          Next := Node and NodePtrMask;
          if Next <> 0 then
            DoSearch(Next, SymbolPos, WordLength +1);
        until Node and EON <> 0;
      end;
    end else
    begin
      Terminates := (Symbols[SymbolPos +1] = Terminator) or
                   ((Symbols[SymbolPos +1] = Asterisk) and (Symbols[SymbolPos +2] = Terminator));
      if Symbol <> Numeric then
      begin
        repeat
          Node := FTrie[Index];
          Inc(Index);
          Word[WordLength] := Node shr SymbolShift;
          if (Symbol = Wildcard) or (Node and SymbolMask = Symbol) then
          begin
            if Terminates and (Node and EOW <> 0) then DoFound;
            Next := Node and NodePtrMask;
            if Next <> 0 then DoSearch(Next, SymbolPos +1, WordLength +1);
          end;
        until Node and EON <> 0;
      end else
      begin
        repeat
          Node := FTrie[Index];
          Inc(Index);
          Word[WordLength] := Node shr SymbolShift;
          if Char(Word[WordLength]) in ['0'..'9'] then
          begin
            if Terminates and (Node and EOW <> 0) then DoFound;
            Next := Node and NodePtrMask;
            if Next <> 0 then DoSearch(Next, SymbolPos +1, WordLength +1);
          end;
        until Node and EON <> 0;
      end;
    end;
  end;

var
  Pos: Integer;
  Temp: String;
begin
  if Found = nil then
  begin
    Result := False;
    Exit;
  end;
  Dec(WithLength);
  HasFound := False;
  Found.Unpack;
  SetLength(Temp, FMaxLength);
  Word := Pointer(Temp);
  Symbols := CreateSymbols(PByte(Pattern), Length(Pattern));
  Pos := 0;
  while Pos < Length(Symbols) do
  begin
    DoSearch(FRootNode, Pos, 0);
    while Symbols[Pos] <> Terminator do Inc(Pos);
    while Symbols[Pos] = Terminator do Inc(Pos);
  end;
  Result := HasFound;
end;

function TDawg.Exists(const Pattern: String): Boolean;
begin
  Result := Exists(PByte(Pattern), Length(Pattern));
end;

function TDawg.Exists(Pattern: PByte; Length: Integer): Boolean;
// identical to .Search() but only test if any words exists
var
  Symbols: TDawgNodeList;

  function DoFind(Index: TDawgNode; SymbolPos: TDawgNode): Boolean;
  var
    Node,Next,Symbol: TDawgNode;
    Terminates: Boolean;
  begin
    Result := False;
    Symbol := Symbols[SymbolPos];
    if Symbol = Terminator then Exit;
    if Symbol = Asterisk then
    begin
      if Symbols[SymbolPos +1] <> Terminator then
      begin
        Result := DoFind(Index, SymbolPos +1);
        if not Result then
        repeat
          Node := FTrie[Index];
          Inc(Index);
          Next := Node and NodePtrMask;
          if Next <> 0 then
            Result := DoFind(Next, SymbolPos +1) or DoFind(Next, SymbolPos);
        until Result or (Node and EON <> 0);
      end else
      begin
        repeat
          Node := FTrie[Index];
          Inc(Index);
          if Node and EOW <> 0 then
          begin
            Result := True;
            Exit;
          end;
          Next := Node and NodePtrMask;
          if Next <> 0 then
            Result := DoFind(Next, SymbolPos +1) or DoFind(Next, SymbolPos);
        until Result or (Node and EON <> 0);
      end;
    end else
    begin
      Terminates := (Symbols[SymbolPos +1] = Terminator) or
                   ((Symbols[SymbolPos +1] = Asterisk) and (Symbols[SymbolPos +2] = Terminator));
      if Symbol <> Numeric then
      begin
        repeat
          Node := FTrie[Index];
          Inc(Index);
          if (Symbol = Wildcard) or (Node and SymbolMask = Symbol) then
          begin
            if Terminates and (Node and EOW <> 0) then
            begin
              Result := True;
              Exit;
            end;
            Next := Node and NodePtrMask;
            if Next <> 0 then Result := DoFind(Next, SymbolPos +1);
          end;
        until Result or (Node and EON <> 0);
      end else
      begin
        repeat
          Node := FTrie[Index];
          Inc(Index);
          if Char(Node shr SymbolShift) in ['0'..'9'] then
          begin
            if Terminates and (Node and EOW <> 0) then
            begin
              Result := True;
              Exit;
            end;
            Next := Node and NodePtrMask;
            if Next <> 0 then Result := DoFind(Next, SymbolPos +1);
          end;
        until Result or (Node and EON <> 0);
      end;
    end;
  end;


var
  Pos: Integer;
begin
  Symbols := CreateSymbols(Pattern, Length);
  Pos := 0;
  while Pos < Length do
  begin
    Result := DoFind(FRootNode, Pos);
    if Result then Exit;
    while Symbols[Pos] <> Terminator do Inc(Pos);
//    while Symbols[Pos] = Terminator do Inc(Pos);
  end;
  Result := False;
end;

function TDawg.SearchCombinatoric(const Pattern: String; Found: TDawg; MinLength: Integer = 2): Boolean;
// search all words in DAWG that pass to any combination of chars in Pattern and
// insert the words into DAWG Found.
// On 200023 wordlist and random 7 char pattern (scrabble 102 stones) it
// take 0,044 ms in average eg. 66000 cycles to find in average 26 words per call.

  function ParsePattern(const Pattern: String): String;
  // convert pattern to DAWG's mapping
  var
    I,J: Integer;
    Symbol: TDawgNode;
  begin
    SetLength(Result, Length(Pattern));
    J := 1;
    for I := 1 to Length(Pattern) do
    begin
      Symbol := FMap[Ord(Pattern[I])];
      if Symbol and NullSymbol = 0 then
      begin
        Result[J] := Char(Symbol shr SymbolShift);
        Inc(J);
      end;
    end;
    SetLength(Result, J);
  end;

  procedure Sort(List: PChar; Lo, Hi: Integer);
  // sort a List of chars ascending
  var
    L,H: Integer;
    M,T: Char;
  begin
    L := Lo;
    H := Hi;
    M := List[(L + H) div 2];
    repeat
      while List[L] < M do Inc(L);
      while List[H] > M do Dec(H);
      if L <= H then
      begin
        T := List[L];
        List[L] := List[H];
        List[H] := T;
        Inc(L);
        Dec(H);
      end;
    until L > H;
    if H > Lo then Sort(List, Lo, H);
    if L < Hi then Sort(List, L, Hi);
  end;

var
  Symbols,Stop: PChar;
  HasFound: Boolean;

  procedure DoSearch(Index: TDawgNode; Pos: PChar);
  var
    Node: TDawgNode;
    Symbol,Temp: Char;
    Cur: PChar;
    Len: Integer;
  label
    Finish;
  begin
  // some checks
    if (Index = 0) or (Pos > Stop) then Exit;
  // loop trough our node
    Cur := Pos;
    repeat
      Node := FTrie[Index];
      Inc(Index);
      Symbol := Char(Node shr SymbolShift);
      while Symbol > Pos^ do
      begin
      // symbol of current node is greater as patternchar,
      // so we reorder our pattern to next higher char
        Temp := Cur^;
        Cur^ := Pos^;
        Pos^ := Temp;
        Inc(Cur);
        if Cur > Stop then goto Finish; // we finished, use goto here
      end;
      if Symbol = Pos^ then
      begin
      // yes, we found or slot
        if Node and EOW <> 0 then
        begin
          // and we found a passing word
          Len := Pos - Symbols +1;
          if Len >= MinLength then
          begin
            // and word have right length, now insert into result Dawg
            Found.Insert(PByte(Symbols), Len);
            Found.Insert;
            HasFound := True;
          end;
        end;
      // search next deeper chars
        DoSearch(Node and NodePtrMask, Pos +1);
      end;
    until Node and EON <> 0;
  // we have reach the end of the node, now we must resync our pattern,
  // thus reorder it
    while Cur <= Stop do
    begin
      Temp := Cur^;
      Cur^ := Pos^;
      Pos^ := Temp;
      Inc(Cur);
    end;
  Finish:
  // restore old pattern order, we rotate it by one char
    Temp := Pos^;
    while Pos < Stop do
    begin
      Pos^ := Pos[1];
      Inc(Pos);
    end;
    Pos^ := Temp;
  end;


var
  Temp: String;
begin
  HasFound := False;
// first parse the pattern
  Temp := ParsePattern(Pattern);
// initialize working pointers
  Symbols := PChar(Temp);
  Stop := Symbols + Length(Temp) -1;
// sort chars of pattern
  Sort(Symbols, 0, Length(Temp) -2);
// do the search
  DoSearch(FRootNode, Symbols);
  Result := HasFound;
end;

end.
