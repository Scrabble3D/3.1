{ TCP Client definition

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

unit utcpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf, LMessages, Forms,
  utcptypes, ugameoptions, uthreadstringlist, utypes,
  blcksock; //synapse


const
  LM_SYNCMESSAGE = LM_USER + 1;

type

  TOnClientSend=procedure(aMsgId:string;aReceiver,aMsg:string) of object;
  TState=(nsSend,nsReceive,nsIdle);

  { TTCPClient }

  TTCPClient=class(TThread)
    private
      {$ifdef CPU32}
      aCriticalSection:LongWord;//TRTLCriticalSection;
      {$else}
      aCriticalSection:QWord;
      {$endif}

      FPlayerData   : TClientInfo; //self info

      FOnClientSend : TOnClientSend;

      FSocket     : TTCPBlockSocket;
      FDataOut    : TThreadStringList;
      FDataIn     : TStringList;
      FPlayers    : TThreadList;
      FTimeToAfk  : longword;

      FUID        : string; //MAC_Address
      FEmail      : string;
      FPassword    : string;
      FServerVersion : string; //nwServerVersion
      FIP          : string; //IP address to connect to
      FPort        : string;
      FState       : TState;

      FProxyIp,FProxyPort,FProxyUser,FProxyPass:string;
      function GetPlayerByIndex(aPlayerIndex: integer): TClientInfo;
      function GetPlayerData(aPlayerName: string): TClientInfo;
      procedure SetPlayerData(aPlayerName: string; const aValue: TClientInfo);
    published
      property OnSend       : TOnClientSend read FOnClientSend write FOnClientSend;
    public
      constructor Create(const aName:string);
      destructor Destroy; override;
      procedure Execute; override;
      procedure SyncError;
      procedure RemovePlayer(aPlayerName:string);
      procedure UpdateMates;
      function GetDataIn:string;
    public
      property PlayerList:TThreadList read FPlayers;

      property ProxyIp:string read FProxyIp write FProxyIp;
      property ProxyPort:string read FProxyPort write FProxyPort;
      property ProxyUser:string read FProxyUser write FProxyUser;
      property ProxyPass:string read FProxyPass write FProxyPass;

      property PlayerData:TClientInfo read FPlayerData write FPlayerData;
      property Players[index:string]:TClientInfo read GetPlayerData write SetPlayerData;
      property PlayersByIndex[index:integer]:TClientInfo read GetPlayerByIndex;

      property UID:string read FUID write FUID;
      property Password : string read FPassword write FPassword;
      property ServerVersion : string read FServerVersion write FServerVersion;
      property IP : string read FIP write FIP;
      property Port : string read FPort write FPort;
      property Email: string write FEmail;
      property TimeToAfk:longword read FTimeToAfk write FTimeToAfk;//milliseconds
      property DataOut:TThreadStringList read FDataOut write FDataOut;
      property Terminated;
    end;

var
  TCPClient : TTCPClient;

implementation

{ TTCPClient }

function TTCPClient.GetPlayerData(aPlayerName: string): TClientInfo;
var
  i:integer;
begin
  if aPlayerName=FPlayerData.PlayerName then
    Result:=FPlayerData else
  with FPlayers.LockList do
  try
    Result:=nil;
    for i:=0 to Count-1 do
    begin
      if TClientInfo(Items[i]).PlayerName=aPlayerName then
      begin
        Result:=TClientInfo(Items[i]);
        break;
      end;
    end;
    if (Result=nil) and (aPlayerName<>'') then
    begin
      Result:=TClientInfo.Create(aPlayerName);
      Add(Result);
    end;
  finally
    FPlayers.UnlockList;
  end;
end;

function TTCPClient.GetPlayerByIndex(aPlayerIndex: integer): TClientInfo;
begin
  with FPlayers.LockList do
  try
    if aPlayerIndex<Count then
      Result:=TClientInfo(Items[aPlayerIndex]) else
      Result:=nil;
  finally
    FPlayers.UnlockList;
  end;
end;

procedure TTCPClient.UpdateMates;
var
  aMateIndex,i:integer;
begin
  with FPlayers.LockList do
  try
    for i:=0 to Count-1 do
    with TClientInfo(Items[i]) do
    begin
      aMateIndex:=FPlayerData.Mates.IndexOf(PlayerName);
      if (GroupID>0) and
         (GroupID=FPlayerData.GroupID) and
         not IsKibitz then
      begin
        if aMateIndex=-1 then
          FPlayerData.Mates.Add(PlayerName)
      end else
      begin
        if aMateIndex>-1 then
          FPlayerData.Mates.Delete(aMateIndex);
      end;
    end;
  finally
    FPlayers.UnlockList;
  end;
end;

function TTCPClient.GetDataIn: string;
begin
  if assigned(FDataIn) and (FDataIn.Count>0) then
  begin
    EnterCriticalSection(aCriticalSection);
    try
      Result:=FDataIn[0];
      FDataIn.Delete(0);
    finally
      LeaveCriticalSection(aCriticalSection);
    end;
  end else
    Result:='';
end;

procedure TTCPClient.SetPlayerData(aPlayerName: string; const aValue: TClientInfo);
var
  {%H-}aClientInfo:TClientInfo;
begin
  with FPlayers.LockList do
  try
    aClientInfo:=GetPlayerData(aPlayerName);
    aClientInfo:=aValue;
  finally
    FPlayers.UnlockList;
  end;
end;

procedure TTCPClient.RemovePlayer(aPlayerName: string);
var
  i:integer;
  aClientInfo:TClientInfo;
begin
  aClientInfo:=GetPlayerData(aPlayerName);
  if (aClientInfo<>FPlayerData) and (aClientinfo<>nil) then
  with FPlayers.LockList do
  try
    //mates
    i:=FPlayerData.Mates.IndexOf(aPlayerName);
    if i>-1 then
      FPlayerData.Mates.Delete(i);
    //list
    i:=IndexOf(aClientInfo);
    if i>-1 then
    begin
      TClientInfo(Items[i]).Free;
      Delete(i);
    end;
  finally
    FPlayers.UnlockList;
  end;
end;

constructor TTCPClient.Create(const aName:string);
begin
  inherited Create(true);
  FreeOnTerminate:=true;
  InitializeCriticalSection(aCriticalSection);

  FPlayers:=TThreadList.Create;

  FPlayerData:=TClientInfo.Create(aName);
  FPlayers.Add(FPlayerData);

  FDataOut:=TThreadStringList.Create;
  FDataIn:=TStringList.Create;
  FDataIn.Delimiter:=Chr(2);
  FDataIn.StrictDelimiter:=true;
end;

destructor TTCPClient.Destroy;
var
  i:integer;
begin
  if assigned(FSocket) then
  begin
    FSocket.CloseSocket;
    FSocket.Free;   //    FreeAndNil(FSocket);
  end;
  with FPlayers.LockList do
  try
    for i:=0 to Count-1 do   //item[0]=FPlayerData
      TClientInfo(Items[i]).Free;
  finally
    FPlayers.UnlockList;
  end;
  DeleteCriticalSection(aCriticalSection);
  FPlayers.Free;
  FDataIn.Free;
  FDataOut.Free;
  inherited Destroy;
end;

procedure TTCPClient.Execute;
var
  s:string;
begin
  FSocket:=TTCPBlockSocket.Create;
  if (FProxyIp<>'') then
  begin
    FSocket.HTTPTunnelIP:=FProxyIP;
    FSocket.HTTPTunnelPort:=FProxyPort;
    FSocket.HTTPTunnelUser:=FProxyUser;
    FSocket.HTTPTunnelPass:=FProxyPass;
  end;
  FSocket.Connect(FIP,FPort);

  with TStringList.Create do
  try
    Delimiter:=nwDelimiter;
    StrictDelimiter:=true;
    Add('Password='+FPassword);
    Add('MenuLang='+FPlayerData.MenuLang);
    Add('Version='+FServerVersion);
    Add('Country='+FPlayerData.Country);
    Add('City='+FPlayerData.City);
    Add('UID='+FUID);
    Add('Email='+FEmail);
    Add('Release='+FPlayerData.Release);
    Add('MessageType=nwConnect');
    Add('Sender='+FPlayerData.PlayerName);
    Add('Receiver='+FPlayerData.PlayerName);
    FDataOut.Add(DelimitedText+CRLF);
  finally
    Free;
  end;

  if not Terminated and (FSocket.LastError=0) then
  begin
    PostMessage(Application.Mainform.Handle, LM_SYNCMESSAGE, 0, 0);
    while not Terminated do
    begin
      sleep(10);
      if FDataOut.Count>0 then
      begin
        FSocket.SendString(FDataOut[0]);
        FDataOut.Delete(0);
      end;
      s:=FSocket.RecvString(10);
      if s<>'' then
      begin
        FState:=nsReceive;
        FDataIn.Add(s);
        PostMessage(Application.Mainform.Handle, LM_SYNCMESSAGE, 0, 0);
      end;
      {$IF defined(WINDOWS)}
      if (FSocket.LastError<>0) and (FSocket.LastError<>10060)
      {$ELSEIF defined(Linux)}
      if (FSocket.LastError<>0) and (FSocket.LastError<>110)
      {$ELSEIF defined(Darwin)}
      if (FSocket.LastError<>0) and (FSocket.LastError<>60)
      {$ENDIF}
      then
        Terminate; //110=WSAETIMEDOUT
    end;//while
  end else //LastError on connect
    Synchronize(@SyncError);

  FSocket.CloseSocket;
end;

procedure TTCPClient.SyncError;
begin
  if not Terminated then
    OnMessage(smError,'Network error: '+FSocket.LastErrorDesc);
end;

end.

