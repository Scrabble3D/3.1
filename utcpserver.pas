{ Local TCP Server

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

unit utcpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  utcpnetwork, utcptypes, utypes;

type

  { TLanTCPServer }

  TLanTCPServer=class(TCustomTCPServer)
     aLog:string;
     procedure CheckMessage(var aSender:TClient; var aRecvMsg:TNetworkMessage); override;
     procedure Log(EventType : TEventType; const Msg : String); override;
     procedure SyncMethod;
   end;

var
  TCPServer : TLanTCPServer;

implementation

procedure TLanTCPServer.CheckMessage(var aSender:TClient; var aRecvMsg:TNetworkMessage);
var
  i:integer;
  aClient:TClient;
begin
  case aRecvMsg.MessageType of
   'nwConnect':
    begin
      aSender.PlayerName:=aRecvMsg.Sender;
      if (aRecvMsg.Value('Version',0)<nwServerVersion) then
      begin
        aRecvMsg.Clear;
        aRecvMsg.Values['MessageType']:='nwDisconnect';
        aRecvMsg.Values['Sender']:=aSender.PlayerName;
        aRecvMsg.Values['Receiver']:=aSender.PlayerName;
        aRecvMsg.Values['Message']:='Error_Release';
        exit;
      end;
      if Clients.Count>4 then    //too much players
      begin
        aRecvMsg.Clear;
        aRecvMsg.Values['MessageType']:='nwDisconnect';
        aRecvMsg.Values['Sender']:=aSender.PlayerName;
        aRecvMsg.Values['Receiver']:=aSender.PlayerName;
        aRecvMsg.Values['Message']:='Error_Connections';
        exit;
      end;
      for i:=0 to Clients.Count-1 do //unique name for LAN
      begin
        aClient:=TClient(Clients[i]);
        if (aClient.PlayerName=aSender.PlayerName) and
           (aClient<>aSender) then
        begin
          aRecvMsg.Clear;
          aRecvMsg.Values['MessageType']:='nwDisconnect';
          aRecvMsg.Values['Sender']:=aSender.PlayerName;
          aRecvMsg.Values['Receiver']:=aSender.PlayerName;
          aRecvMsg.Values['Message']:='Error_Name';
          exit;
        end;
      end;
      aSender.GroupID:=1;
      aSender.Release:='0.1';
      aRecvMsg.Clear;
      aRecvMsg.Values['MessageType']:='nwLogin'; //handshake -> call nwRefresh from client
      aRecvMsg.Values['Sender']:=aSender.PlayerName;
      aRecvMsg.Values['Receiver']:='all';
      aRecvMsg.Values['Count']:=inttostr(Clients.Count);
      for i:=0 to Clients.Count-1 do
      begin
        aClient:=TClient(Clients[i]);
        aRecvMsg.Values[inttostr(i)+'_'+'Name']:=aClient.PlayerName;
      end;
    end;
   'nwRefresh':
    begin
      aRecvMsg.Values['Count']:=inttostr(Clients.Count);
      for i:=0 to Clients.Count-1 do
      begin
        aClient:=TClient(Clients[i]);
        aRecvMsg.Values[inttostr(i)+'_'+'Name']:=aClient.PlayerName;
        aRecvMsg.Values[inttostr(i)+'_'+'Group']:='1';
      end;
    end;
   'nwSyncNewGame':
    begin
      aRecvMsg.Add('PlayerCount='+inttostr(Clients.Count));
      for i:=0 to Clients.Count-1 do
      begin
        aClient:=TClient(Clients[i]);
        aRecvMsg.Add('Player'+inttostr(i+1)+'Name='+aClient.PlayerName);
      end;
    end;
   'nwServerInfo','nwGameResult','nwHighscore':
    begin
      aRecvMsg.Clear;
    end;
  end;//case
end;

procedure TLanTCPServer.Log(EventType: TEventType; const Msg: String);
begin
//  exit;
{  aLog:=Msg;
  Synchronize(@SyncMethod);}
  //nothing to log in client/server mode
end;

procedure TLanTCPServer.SyncMethod;
begin
  OnMessage(smDebug,aLog);
end;

end.

