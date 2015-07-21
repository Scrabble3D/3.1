{ Helper functions
  OnMessage and OnProgress are global variables and connected in main after create

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

unit utypes;

{$mode objfpc}
{$H+}

interface

uses
  Controls, SysUtils, Forms, Graphics, Classes, ComCtrls,
  LCLIntF, FileUtil, Dialogs, StdCtrls, LCLType,
  {$IF defined(Linux)}ctypes
  {$ELSEIF defined(FREEBSD) or defined(DARWIN)}ctypes, sysctl
  {$ELSEIF defined(WINDOWS)}Windows, Dynlibs{$ENDIF}
  ;

type
  TScrabbleMessageType=(smError=0,smWarning=1,smInformation=2,smChat=3,smOwnMove=4,smGameResult=5,
                        smDebug=6,smPanelOnly=7,smPanelOnlyWithoutTime=8,smScrabble_self=9,
                        smScrabble_other=10);
  TOnMessage=procedure(aMsgType:TScrabbleMessageType; aMsg: string; aSender:string='') of object;
  TOnProgress=procedure(Sender:TObject; const Value:byte) of object;
  TOnGetSetting=function(aIdentifier:string):integer of object;

  TOnUpdatePoints=procedure(Who:byte; Value:word) of object;

  EScrabbleError=class(Exception);

  TScrabbleDragCursor=(scInsertLeft,scInsertRight,scExchange,scDrop,scNoDrop);

  { TAbstractScrabbleDragObject }
  TAbstractScrabbleDragObject=class(TDragControlObjectEx) //fmPieces, fm2D, fm3D
   private
   protected
     FCursor : TScrabbleDragCursor;
     function GetData:TObject;virtual;abstract;
   public
     property Data : TObject read GetData;
     property CursorType : TScrabbleDragCursor read FCursor write FCursor;
   end; //TAbstractScrabbleDragObject

   TSizeOrder=record                     //umoveablepanel
     Size  : integer;
     Panel : TControl;
   end;

   THistoryArray=array of integer;

   function LightOrDark(Value : TColor; Inverse:boolean=false):TColor;
   function ColorToHex(const aValue:TColor;aDigits:byte=2):string;
   procedure SortWindowZOrder(aValue:array of TSizeOrder);
   function SecondsToString(const aValue:integer;UseHour:boolean=true):string;
   function MessageDlgNoFocus(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons):integer;
   procedure RemoveBOMFromFile(aFileName:string);
   //MTProcs -> MTCPU
   function GetSystemThreadCount: integer;
   {$IFDEF Linux}
   const _SC_NPROCESSORS_ONLN = 83;
   function sysconf(i: cint): clong; cdecl; external name 'sysconf';
   {$ENDIF}
   function MAC_Address: string;

const
  LineBreak=#13+#10;
  clScrabble=clGreen;

var
  OnMessage  : TOnMessage; //global available
  OnProgress : TOnProgress;
  OnUpdatePoints: TOnUpdatePoints;

implementation

function LightOrDark(Value : TColor;Inverse:boolean=false):TColor;
var
  l:longint;
begin
  l:=ColorToRGB(Value);
  if (l and $00FF0000) shr 16+
     (l and $0000FF00) shr 8+
     (l and $000000FF) > 500 then
    Result:=clBlack else
    Result:=clWhite;
  if Inverse then
  begin
    if Result=clBlack then
      Result:=clWhite else
      Result:=clBlack;
  end;
end;

function ColorToHex(const aValue: TColor;aDigits:byte): string;
begin
  Result:=IntToHex(GetRValue(aValue),aDigits)+
          IntToHex(GetGValue(aValue),aDigits)+
          IntToHex(GetBValue(aValue),aDigits);
end;

function DoSort(Item1:Pointer;Item2: Pointer):Integer;
begin
  if TSizeOrder(Item1^).Size=TSizeOrder(Item2^).Size then
   Result:=0 else
    if TSizeOrder(Item1^).Size>TSizeOrder(Item2^).Size then
     Result:=-1 else
     Result:=1;
end;

procedure SortWindowZOrder(aValue: array of TSizeOrder);
var
  SizeOrder : ^TSizeOrder;
  i         : integer;
begin
  with TList.Create do
  try
    for i:=0 to length(aValue)-1 do
    begin
      New(SizeOrder);
      SizeOrder^.Panel:=aValue[i].Panel;
      SizeOrder^.Size:=aValue[i].Size;
      Add(SizeOrder);
    end;
    Sort(@DoSort);
    while Count>0 do
    begin
      SizeOrder:=Items[0];
      SizeOrder^.Panel.BringToFront;
      Dispose(SizeOrder);
      Delete(0);
    end;
    for i:=0 to Application.Mainform.ControlCount-1 do
     if (Application.Mainform.Controls[i] is TStatusBar) or
        (Application.Mainform.Controls[i] is TToolBar) then
      Application.Mainform.Controls[i].BringToFront;
  finally
    Free;
  end;
end;

function SecondsToString(const aValue: integer;UseHour:boolean): string;
const
  cMinute=60;//s
  cHour=60*cMinute;
  cDay=24*cHour;
var
  i:Longword;
  s:string;
begin
  if aValue>0 then i:=aValue else i:=0;
  if UseHour then
  begin
    if i>cDay then
    begin
      Result:=inttostr(i div cDay)+'d ';
      {$Hints off}
      dec(i,(i div cDay)*cDay);
      {$Hints on}
    end else
      Result:='';
    Result:=Result+inttostr(i div cHour)+':';
    {$Hints off}
    dec(i,(i div cHour)*cHour);
    {$Hints on}
  end else
    Result:='';
  s:=inttostr(i div cMinute);
  {$Hints off}
  dec(i,(i div cMinute)*cMinute); while length(s)<2 do s:='0'+s;
  {$Hints on}
  Result:=Result+s+':';
  s:=inttostr(i);
  while length(s)<2 do s:='0'+s;
  Result:=Result+s;
end;

function MessageDlgNoFocus(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons):integer;
var
  aButton:TButton;
  aForm:TForm;
begin
  aForm:=CreateMessageDialog(Msg,DlgType,Buttons);
  with aForm do
  try
    aButton:=TButton.Create(aForm);
    aButton.Parent:=aForm;
    aButton.SetBounds(1000,0,0,0);
    ActiveControl:=aButton;
    CancelControl:=nil;
    Result:=ShowModal;
  finally
    aButton.free;
    aForm.Free;
  end;
end;

{---------------------------------}
//based on lazsolutions by Silvio Clecio.
//http://code.google.com/p/lazsolutions/source/browse/trunk/Core/LSUtils.pas

function MAC_Address: string;

{$IFDEF MSWINDOWS}
type
  TCreateGUIDFunction = function(AGUID: PGUID): LongInt; stdcall;
{$ENDIF}
{$IFDEF UNIX}
const
  VDevice: array[0..7] of string=('eth', 'wlan', 'ppp', 'ath', 'ra', 'msh', 'venet', 'vet');
{$ENDIF}
var
{$IFDEF UNIX}
  VPath : string;
  i,j   : byte;
{$ENDIF}
{$IFDEF MSWINDOWS}
  VLibHandle: TLibHandle;
  VCreateGUIDFunction: TCreateGUIDFunction;
  VGUID1, VGUID2: TGUID;
{$ENDIF}
begin
  Result := '';

  {$IFDEF UNIX}
  for i:=0 to high(VDevice) do
   for j:=0 to 9 do
   begin
     VPath := Format('/sys/class/net/%s/address', [VDevice[i]+inttostr(j)]);
     if FileExists(VPath) then
      with TFileStream.Create(VPath, fmOpenRead or fmShareDenyWrite) do
      try
        SetLength(Result, 17);
        Read(Pointer(Result)^, 17);
        exit;
      finally
        Free;
      end;
   end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  VLibHandle := LoadLibrary('rpcrt4.dll');
  try
    if VLibHandle <> NilHandle then
    begin
      VCreateGUIDFunction := TCreateGUIDFunction(GetProcedureAddress(VLibHandle,
        'UuidCreateSequential'));
      if Assigned(VCreateGUIDFunction) then
        if (VCreateGUIDFunction(@VGUID1) = 0) and
          (VCreateGUIDFunction(@VGUID2) = 0) and (VGUID1.D4[2] = VGUID2.D4[2]) and
          (VGUID1.D4[3] = VGUID2.D4[3]) and (VGUID1.D4[4] = VGUID2.D4[4]) and
          (VGUID1.D4[5] = VGUID2.D4[5]) and (VGUID1.D4[6] = VGUID2.D4[6]) and
          (VGUID1.D4[7] = VGUID2.D4[7]) then
            Result := Format('%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x', [VGUID1.D4[2], VGUID1.D4[3],
                        VGUID1.D4[4], VGUID1.D4[5], VGUID1.D4[6], VGUID1.D4[7]]);
    end;
  finally
    UnloadLibrary(VLibHandle);
  end;
  {$ENDIF}
end;

//MTProcs -> MTCPU
function GetSystemThreadCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: {$ifdef win64}QWORD{$else}DWORD{$endif};
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(UNTESTEDsolaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  fpsysctl(pchar(@mib), 2, @t, @len, Nil, 0);
  Result:=t;
end;
{$ELSEIF defined(linux)}
  begin
    Result:=sysconf(_SC_NPROCESSORS_ONLN);
  end;

{$ELSE}
  begin
    Result:=1;
  end;
{$ENDIF}

procedure RemoveBOMFromFile(aFileName:string);
const
  UTF8BOM:string=#$EF#$BB#$BF;
var
  Content:string;
begin
  if FileExistsUTF8(aFileName) then
  with TFileStream.Create(aFileName,fmOpenReadWrite) do
  try
    if Size>3 then
    begin
      SetLength(Content,Size);
      Read(Content[1],Size);
      if Copy(Content,1,3)=UTF8BOM then
      begin
        Delete(Content,1,3);
        Seek(0,soBeginning);
        Write(Content[1],length(Content));
      end;
      SetLength(Content,0);
    end;
  finally
    Free;
  end;
end;

end.
