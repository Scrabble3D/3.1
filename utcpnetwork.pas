{ TCPClient and network functions

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

unit utcpnetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  utcptypes,
  synsock, blcksock; //synapse

type
  TOnClientReceived=procedure(aMsgId:Char; aSender, aReceiver:string; aMsg:TStrings) of object;

  { TClientList }

  TOnClientRemove=procedure(aClientName:string) of object;
  TClientList=class(TList)
   private
     FOnClientRemove:TOnClientRemove;
   public
     function RemoveClient(Item: Pointer): Integer;
     property OnClientRemove:TOnClientRemove read FOnClientRemove write FOnClientRemove;
   end;

  { TClient }
  TOnLog=procedure(EventType : TEventType; const Msg : String) of object;

  TClient=class(TClientInfo)
    private
      FClientSocket : TTCPBlockSocket;
      FOnLog        : TOnLog;
      FParentList   : TClientList; //link to server.clients
      procedure DoClientStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
    public
      constructor Create(aSocket:TSocket; aParentList:TClientList);
      destructor Destroy; override;
      property ClientSocket : TTCPBlockSocket read FClientSocket;
      property OnLog        : TOnLog read FOnLog write FOnLog;
    end;

  { TCustomTCPServer }

  TCustomTCPServer=class(TThread)
    private
      FServerSocket  : TTCPBlockSocket;
      FPort     : string;
      FClients  : TClientList;
      FRecvMsg  : TNetworkMessage;
    protected
      procedure CheckMessage(var Sender:TClient; var aRecvMsg:TNetworkMessage); virtual; abstract;
      procedure Log(EventType : TEventType; const Msg : String); virtual; abstract;
    public
      constructor Create(aPort:string;aStart:boolean=true);
      destructor Destroy; override;
      procedure Execute; override;
      property Clients : TClientList read FClients write FClients;
    end;

  function NextGroupId:longword;

implementation

var
  aNextGroupId:longword;

function NextGroupId: longword;
begin
  Result:=aNextGroupId;
  inc(aNextGroupId);
end;

{ TClientList }

function TClientList.RemoveClient(Item: Pointer): Integer;
var
  aClientName:string;
begin
  aClientName:=TClientInfo(Item).PlayerName;
  Result:=Remove(Item);
  if assigned(FOnClientRemove) then
    FOnClientRemove(aClientName);
end;

{ TTCPServer }

constructor TCustomTCPServer.Create(aPort:string;aStart:boolean);
begin
  inherited Create(true);
  FPort:=aPort;
  FServerSocket:=TTCPBlockSocket.Create;
  FServerSocket.EnableReuse(true);//otherwise "address already in use" after crash
  FServerSocket.SetTimeout(1000);
  FreeOnTerminate:=true;
  FClients:=TClientList.Create;
  FRecvMsg:=TNetworkMessage.Create;
  FRecvMsg.Delimiter:=nwDelimiter;
  FRecvMsg.StrictDelimiter:=true;
  if aStart then
    Start;//Resume;
end;

destructor TCustomTCPServer.Destroy;
var
  i:integer;
begin
  //3.0.2.-10
  for i:=FClients.Count-1 downto 0 do
   if assigned(FClients[i]) then
    TClient(FClients[i]).Free;
  FClients.Clear;
  FClients.Free;
  FServerSocket.CloseSocket;
  FServerSocket.Free;
  FRecvMsg.Free;
  inherited Destroy;
end;

procedure TCustomTCPServer.Execute;
var
  i,j:integer;
  aSender,aReceiver:TClient;
  aSocket:TSocket;
begin
  FServerSocket.Bind('0.0.0.0', FPort);
  //FServerSocket.SetLinger(true, 10);
  if FServerSocket.LastError=0 then
  begin
    FServerSocket.Listen;
    FRecvMsg.Clear;
    while not Terminated do
    begin
      if FServerSocket.CanRead(100) then
      begin
        aSocket:=FServerSocket.Accept;
        aSender:=TClient.Create(aSocket, FClients);
        aSender.GroupID:=NextGroupId;
        aSender.OnLog:=@Log;
        FClients.Add(aSender);
      end;
      for i:=FClients.Count-1 downto 0 do
      begin
        aSender:=TClient(FClients[i]);
        if aSender.ClientSocket.CanRead(10) then
        begin
          //msg
          FRecvMsg.DelimitedText:=aSender.ClientSocket.RecvString(1000);
          //client has been disconnected
          if (aSender.ClientSocket.LastError<>0) then //=WSAECONNRESET) then
            aSender.Free;
          //modify message
          if FRecvMsg.Receiver<>'' then
            CheckMessage(aSender, FRecvMsg);
          //transmit message
          if FRecvMsg.Receiver<>'' then //empty msg
          begin
            if (FRecvMsg.Sender=FRecvMsg.Receiver) then
            begin
              aSender.ClientSocket.SendString(FRecvMsg.DelimitedText+CRLF);
              if (aSender.ClientSocket.LastError<>0) then //=WSAECONNRESET) then
                aSender.Free;
            end else
            for j:=FClients.Count-1 downto 0 do
            begin
              aReceiver:=TClient(FClients[j]);
              if (FRecvMsg.Receiver='all') or
                 (FRecvMsg.Receiver=aReceiver.PlayerName) or
                 ((FRecvMsg.Receiver='group') and (aSender.GroupID=aReceiver.GroupID)) or
                 ((FRecvMsg.Receiver='kibitzes') and (aSender.GroupID=aReceiver.GroupID) and aReceiver.IsKibitz) then
               aReceiver.ClientSocket.SendString(FRecvMsg.DelimitedText+CRLF);
              if (aReceiver.ClientSocket.LastError<>0) then //=WSAECONNRESET) then
                aReceiver.Free;
            end;//for to j
          end;//not empty
          FRecvMsg.Clear;
        end;
      end; //for to i

      //Disconnect
      for i:=FClients.Count-1 downto 0 do
      begin
        aSender:=TClient(FClients[i]);
        if (aSender.ClientSocket.LastError<>0) then //=WSAECONNRESET) then
          aSender.Free;
      end;
      {$IFDEF WINDOWS}
      if (FServerSocket.LastError<>0) and (FServerSocket.LastError<>10060)
      {$ELSE}
      if (FServerSocket.LastError<>0) and (FServerSocket.LastError<>110)
      {$ENDIF}
      then Terminate; //110=WSAETIMEDOUT
    end;//Terminated
  end else
  begin
    writeln('Fatal error: '+FServerSocket.LastErrorDesc);
    Terminate;//FSocket.LastError<>0
  end;
end;

{ TCustomTCPServerThread }

procedure TClient.DoClientStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
begin
  if assigned(FOnLog) then
  try
    case Reason of
     HR_WAIT: FOnLog(etDebug,'HR_WAIT '+Value+' ('+PlayerName+')');
     HR_Error: FOnLog(etDebug,'HR_ERROR '+Value+' ('+PlayerName+')');
     HR_ResolvingBegin: FOnLog(etDebug,'HR_ResolvingBegin '+Value+' ('+PlayerName+')');
     HR_ResolvingEnd: FOnLog(etDebug,'HR_ResolvingEnd '+Value+' ('+PlayerName+')');
     HR_SocketCreate: FOnLog(etDebug,'HR_SocketCreate '+Value+' ('+PlayerName+')');
     HR_SocketClose: FOnLog(etDebug,'HR_SocketClose '+Value+' ('+PlayerName+')');
     HR_Bind: FOnLog(etDebug,'HR_Bind '+Value+' ('+PlayerName+')');
     HR_Connect: FOnLog(etDebug,'HR_Connect '+Value+' ('+PlayerName+')');
     HR_Listen: FOnLog(etDebug,'HR_Listen '+Value+' ('+PlayerName+')');
     HR_Accept: FOnLog(etDebug,'HR_Accept '+Value+' ('+PlayerName+')');
    end;//case
  except
    on E:Exception do FOnLog(etError,E.Message);
  end;
end;

constructor TClient.Create(aSocket: TSocket; aParentList:TClientList);
begin
  inherited Create('');
  Release:='0.0';
  FClientSocket:=TTCPBlockSocket.Create;
  FClientSocket.SetTimeout(1000);
  FClientSocket.Socket:=aSocket;
  FClientSocket.OnStatus:=@DoClientStatus;
  FParentList:=aParentList;
end;

destructor TClient.Destroy;
begin
  //3.0.2.-10
  try
    if assigned(FClientSocket) then
      FClientSocket.Free;
  except
    //access violation
  end;
  if assigned(FParentList) then
    FParentList.RemoveClient(self);
  inherited Destroy;
end;

initialization
  aNextGroupId:=0;
end.

