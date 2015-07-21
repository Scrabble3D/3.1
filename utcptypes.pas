{ Functions for network messages

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

unit utcptypes;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

const

  nwDelimiter=#7;
  nwServerVersion=4;

  CR = #$0d; //blcksock
  LF = #$0a;
  CRLF = CR + LF;

  {.$define TestServer}
  {$ifdef TestServer}
   cPort='5002';{$Warning Debug: TestServer}
  {$else}
   cPort='5001';
  {$endif}


type

  { TClientInfo }
  TClientState=(csOpenForGames,csNotOpen,csAfk,csBusy);
  TClientInfo=class
    PlayerName  : string;
    Country     : string;
    City        : string;
    GameStarted : TDateTime;
    MoveNumber  : integer;
    Release     : string;
    Mates       : TStringList;
    Registered  : TDateTime;
    Rating      : word;
    GamesPlayed : word;
    MenuLang    : string;
    AllowKibitz : boolean;
    IsKibitz    : boolean;
    GroupID, GameID : longword;
    ClientState : TClientState;
    constructor Create(aName:string);
    destructor Destroy; override;
  end;

  { TNetworkMessage
  TNetworkMessageType=(nwConnect,     //send by client itself after as handshake / returns nwLogin or nwDisconnect from server
                       nwLogin,       //fired just after login of any player, returns static data of all players
                       nwRefresh,     //mainly pulled from client to retrieve dynamic data of all players
                       nwDisconnect,  //sent by sever if connection has failed
                       nwLogout,      //sent if a player disconnects
                       nwChat,        //simple chat message
                       nwInformation, //static information with translation, e.g. chat receiver not online
                       nwServerInfo,  //info sent to server, e.g. sequence of mates
                       nwInvite,      //invitation to a group
                       nwJoin,        //join a group either as mate (before game start or if names match) or as kibitz
                       nwLeave,       //group is informed (recipient is set to group by server); leaver pushes nwRefresh to 'all'
                       nwSyncNewGame, //synchronize and apply game settings and ask to start
                       nwStartNewGame,//start the game
                       nwAnswer,      //decisions are made in group, answer yes/no to a poll
                       nwPoll,        //question to ask for in group
                       nwPause,       //pause the game (hide pieces)
                       nwKibitz,      //set kibitz mode to private or public
                       nwCheckWord,   //check if words are in dictionary
                       nwCambioSecco, //do a cambio secco
                       nwRemoteGames, //loads a list of stored games on server
                       nwLoadGame,    //message sent to server that responds with nwSync, nwStart and nwNext etc.
                       nwHighscore,   //game server lists all registered players with at least 1 played game, sorts the list and reports the first 10
                       nwNextPlayer,  //includes all placed or exchanged letters or message if move was invalid
                       nwGameResult,  //sent to server on game end to calculate rating
                       nwAddTime,     //add 60s to current player's time
                       nwChallenge,   //challenge last move's word
                       nwBestValues,  //caluculation results to store at gameserver after game analysis
                       nwEndGame      //in challenge mode the move that ends game has to be checked by others; if none objects the game end with this message
                       );
  }
  TNetworkMessage=class(TStringList)
    private
      function GetMessageType: string;
      function GetReceiver: string;
      function GetSender: string;
    published
      function Value(const Identifier:string;def:string):string;overload;
      function Value(const Identifier:string;def:integer):integer;overload;
      function Value(const Identifier:string;def:double):double;overload;
      function Value(const Identifier:string;def:boolean):boolean;overload;
//      function MsgTypeAsString(const aMsgType:TNetworkMessageType):string;
    public
      constructor Create;
      destructor Destroy; override;
      property Receiver:string read GetReceiver;
      property Sender:string read GetSender;
      property MessageType:string read GetMessageType;
    end;

  function ClientStateToString(const aValue:TClientState): string;
  function StringToClientState(const aValue: string):TClientState;

implementation

{ TNetworkMessage }

function TNetworkMessage.GetMessageType: string;
//var s:string;
begin
  Result:=Value('MessageType','');
{  s:=Value('MessageType','nwUnknown');
  case s of
   'nwConnect'     : Result:=nwConnect;
   'nwLogin'       : Result:=nwLogin;
   'nwRefresh'     : Result:=nwRefresh;
   'nwDisconnect'  : Result:=nwDisconnect;
   'nwLogout'      : Result:=nwLogout;
   'nwChat'        : Result:=nwChat;
   'nwInformation' : Result:=nwInformation;
   'nwInvite'      : Result:=nwInvite;
   'nwLeave'       : Result:=nwLeave;
   'nwJoin'        : Result:=nwJoin;
   'nwSyncNewGame' : Result:=nwSyncNewGame;
   'nwStartNewGame': Result:=nwStartNewGame;
   'nwPoll'        : Result:=nwPoll;
   'nwAnswer'      : Result:=nwAnswer;
   'nwServerInfo'  : Result:=nwServerInfo;
   'nwPause'       : Result:=nwPause;
   'nwKibitz'      : Result:=nwKibitz;
   'nwCambioSecco' : Result:=nwCambioSecco;
   'nwCheckWord'   : Result:=nwCheckWord;
   'nwRemoteGames' : Result:=nwRemoteGames;
   'nwLoadGame'    : Result:=nwLoadGame;
   'nwHighscore'   : Result:=nwHighscore;
   'nwNextPlayer'  : Result:=nwNextPlayer;
   'nwGameResult'  : Result:=nwGameResult;
   'nwAddTime'     : Result:=nwAddTime;
   'nwChallenge'   : Result:=nwChallenge;
   'nwBestValues'  : Result:=nwBestValues;
   'nwEndGame'     : Result:=nwEndGame;
   else raise EInOutError.Create('Unknown message received: '+s);
  end;  }
end;

function TNetworkMessage.GetReceiver: string;
begin
  Result:=Value('Receiver','');
end;

function TNetworkMessage.GetSender: string;
begin
  Result:=Value('Sender','');
end;

function TNetworkMessage.Value(const Identifier: string;def:string): string;
begin
  if IndexOfName(Identifier)=-1 then
    Result:=def else
    Result:=Values[Identifier];
end;

function TNetworkMessage.Value(const Identifier: string;def:integer): integer;
begin
  if IndexOfName(Identifier)=-1 then
    Result:=def else
    Result:=StrToIntDef(Values[Identifier],def);
end;

function TNetworkMessage.Value(const Identifier: string; def: double): double;
begin
  if IndexOfName(Identifier)=-1 then
    Result:=def else
    Result:=StrToFloatDef(StringReplace(Values[Identifier],'.',DefaultFormatsettings.DecimalSeparator,[rfReplaceAll]),def);
end;

function TNetworkMessage.Value(const Identifier: string; def: boolean): boolean;
begin
  if IndexOfName(Identifier)=-1 then
    Result:=def else
    Result:=StrToBoolDef(Values[Identifier],def);
end;

constructor TNetworkMessage.Create;
begin
  inherited Create;
  Delimiter:=nwDelimiter;
  StrictDelimiter:=true;
end;

destructor TNetworkMessage.Destroy;
begin
  inherited Destroy;
end;

{ TClientInfo }

function ClientStateToString(const aValue:TClientState): string;
begin
  case aValue of
   csOpenForGames:Result:='csOpenForGames';
   csNotOpen:Result:='csNotOpen';
   csAfk:Result:='csAfk';
   csBusy:Result:='csBusy';
   else Result:='csOpenForGames';
  end;
end;

function StringToClientState(const aValue: string):TClientState;
begin
  case aValue of
   'csOpenForGames':Result:=csOpenForGames;
   'csNotOpen':Result:=csNotOpen;
   'csAfk':Result:=csAfk;
   'csBusy':Result:=csBusy;
    else Result:=csOpenForGames;
  end;
end;

constructor TClientInfo.Create(aName:string);
begin
  inherited Create;
  Rating:=1000;
  GamesPlayed:=0;
  MenuLang:='gb';
  Country:='gb';
  IsKibitz:=false;
  AllowKibitz:=false;
  Mates:=TStringList.Create;
  PlayerName:=aName;
  ClientState:=csOpenForGames;
end;

destructor TClientInfo.Destroy;
begin
  Mates.Free;
  inherited Destroy;
end;

end.

{
function TNetworkMessage.MsgTypeAsString(const aMsgType: TNetworkMessageType): string;
begin
  case aMsgType of
   nwConnect      : Result:='nwConnect';
   nwLogin        : Result:='nwLogin';
   nwRefresh      : Result:='nwRefresh';
   nwDisconnect   : Result:='nwDisconnect';
   nwLogout       : Result:='nwLogout';
   nwChat         : Result:='nwChat';
   nwInformation  : Result:='nwInformation';
   nwInvite       : Result:='nwInvite';
   nwLeave        : Result:='nwLeave';
   nwJoin         : Result:='nwJoin';
   nwSyncNewGame  : Result:='nwSyncNewGame';
   nwStartNewGame : Result:='nwStartNewGame';
   nwServerInfo   : Result:='nwServerInfo';
   nwAnswer       : Result:='nwAnswer';
   nwPoll         : Result:='nwPoll';
   nwPause        : Result:='nwPause';
   nwKibitz       : Result:='nwKibitz';
   nwCambioSecco  : Result:='nwCambioSecco';
   nwCheckWord    : Result:='nwCheckWord';
   nwRemoteGames  : Result:='nwRemoteGames';
   nwLoadGame     : Result:='nwLoadGame';
   nwHighscore    : Result:='nwHighscore';
   nwNextPlayer   : Result:='nwNextPlayer';
   nwGameResult   : Result:='nwGameResult';
   nwAddTime      : Result:='nwAddTime';
   nwChallenge    : Result:='nwChallenge';
   nwBestValues   : Result:='nwBestValues';
   nwEndGame      : Result:='nwEndGame';
   else
     raise EInOutError.Create('Unknown message to send');
  end;
end;    }
