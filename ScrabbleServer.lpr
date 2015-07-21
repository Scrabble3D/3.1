program ScrabbleServer;

{$mode objfpc}{$H+}

{$define sql56} //define sql55 to use Mysql5.5 connector
//{$define sql50}{$define sql55}{$define sql56}

uses
  {$ifdef Linux}
  cthreads, //cmem,
  {$endif}
  math, eventlog,
  Classes, Keyboard, SysUtils, IniFiles, Dateutils, strutils, Process,
  {$ifdef sql50}Mysql50conn, {$endif}
  {$ifdef sql55}Mysql55conn, {$endif}
  {$ifdef sql56}Mysql56conn, {$endif}
  sqldb, db,
  synautil, smtpsend,
  utcpnetwork, utcptypes;

type

  { TWanTCPServer }

  TWanTCPServer=class(TCustomTCPServer)
    procedure DoClientRemove(aClientName: string);
  private
    function IsKibitzInGame(const aGameID:longword; const aPlayerName:string):boolean;
  private
    procedure DoConnect(var aSender:TClient; var aRecvMsg:TNetworkMessage);
    procedure DoRefresh(var aSender:TClient; var aRecvMsg:TNetworkMessage);
    procedure DoHighscore(var aRecvMsg:TNetworkMessage);                     //get a list of players sorted by rating; remove "test player" accounts
    procedure DoChat(var aRecvMsg:TNetworkMessage);                          //send a message to a player that is online or store it if offline
    procedure DoInformation(var aRecvMsg:TNetworkMessage);                   //get information about registered players
    procedure DoInvite(var aRecvMsg:TNetworkMessage);
    procedure DoJoin(var aSender:TClient; var aRecvMsg:TNetworkMessage);     //join a group of players as mate or kibitz
    procedure DoLeave(var aSender:TClient; var aRecvMsg:TNetworkMessage);    //leave a group; push refresh from sender
    procedure DoRemoteGames(var aRecvMsg:TNetworkMessage);     //read games saved on server
    procedure DoBestValues(var aRecvMsg:TNetworkMessage);      //add post-hoc calculated bestvalues per move to game state
    procedure DoKibitz(const aSender:TClient; var aRecvMsg:TNetworkMessage);          //sets game public or private (kibitzes will be "self-kicked")
    procedure DoSyncNewGame(const aSender:TClient; var aRecvMsg:TNetworkMessage);     //add player names to settings; set game number
    procedure DoServerInfo(const aSender:TClient; var aRecvMsg:TNetworkMessage);      //stores correct sequence of players to know the player that is on turn at loadgame
    procedure DoAddMove(const aSender:TClient; var aRecvMsg:TNetworkMessage);         //add move to ssg
    procedure DoLoadGame(const aSender:TClient; var aRecvMsg:TNetworkMessage);
    procedure DoGameResult(var aSender:TClient; var aRecvMsg:TNetworkMessage);
  protected
    MySQL                 : {$ifdef sql50}TMySQL50Connection;{$endif}
                            {$ifdef sql55}TMySQL55Connection;{$endif}
                            {$ifdef sql56}TMySQL56Connection;{$endif}
    Trans                 : TSQLTransaction;
    Query                 : TSQLQuery;
    SysLog                : TEventLog;
    GameNumber            : longword;
    DebugMsg              : string;
    Best                  : integer; //best rating value
    MaxDaysForStoredGames : byte;
    MaxNumberOfAccounts   : byte;
    MinGamesPlayed        : byte;
    AdminEmail            : string;
    ServerNews            : string;
    TimeToAfk             : string;
    procedure CheckMessage(var Sender:TClient; var aRecvMsg:TNetworkMessage); override;
    procedure Log(EventType : TEventType; const Msg : String); override;
  public
    constructor Create(aPort:string;aUser,aPasswd,aDatabase:string); reintroduce;
    destructor Destroy; override;
  end;

const
  CR = #$0d; //blcksock
  LF = #$0a;
  CRLF = CR + LF;

procedure TWanTCPServer.DoClientRemove(aClientName: string);
var
  i:integer;
  aClient:TClient;
begin
  with TNetworkMessage.Create do
  try
    Values['MessageType']:='nwLogout';
    Values['Sender']:=aClientName;
    Values['Receiver']:='all';
    Values['Message']:=aClientName+' '+'disconnected';

    Log(etInfo,aClientName+'->all: nwLogout');
    for i:=Clients.Count-1 downto 0 do
    begin
      aClient:=TClient(Clients[i]);
      aClient.ClientSocket.SendString(DelimitedText+CRLF);
    end;
  finally
    Free;
  end;
end;

function TWanTCPServer.IsKibitzInGame(const aGameID: longword; const aPlayerName: string): boolean;
begin
  Result:=false;
  if aGameId>0 then
  with TStringList.Create do
  try
    StrictDelimiter:=true;
    with TIniFile.Create(inttostr(aGameID)+'.ssv') do
    try
      CommaText:=ReadString('Info','Player','');
    finally
      Free;
    end;
    Result:=(IndexOf(aPlayerName)=-1);
  finally
    Free;
  end;
end;

procedure TWanTCPServer.Log(EventType: TEventType; const Msg: String);
var
  s:string;
  aSL:TStringList;
begin
  case EventType of
   etInfo : s:='Info';
   etWarning : s:='Warning';
   etError: s:='Error';
   etDebug: s:='Debug';
   else s:='';
  end;
  s:=Format('%s [%s %s] %s',[s,FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),'Scrabble3D',Msg]);
  writeln(s);

  if EventType in [etWarning,etError] then
  begin
    //add error msg to log
    Syslog.Log(EventType,Msg);

    //send email to admin
    if AdminEmail<>'' then
    begin
      aSL:=TStringList.Create;
      try
        aSL.Add('From: '+AdminEmail);
        aSL.Add('To: '+AdminEmail);
        aSL.Add('Date: ' + Rfc822DateTime(now));
        aSL.Add('Subject: Error');
        aSL.Add('X-mailer: Scrabble3D - powered by Synapse');
        aSL.Add('');
        aSL.Add(s);
        SendToRaw(AdminEmail, AdminEmail, 'localhost', aSL, '', '');
      finally
        aSL.Free;
      end;
    end;
  end;
end;

procedure TWanTCPServer.DoConnect(var aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  s,aEmail:string;
  i:integer;
  aClient:TClient;
begin
  try
    aSender.PlayerName:=aRecvMsg.Sender;
    aSender.MenuLang:=aRecvMsg.Value('MenuLang','gb');
    DebugMsg:=DebugMsg+' ('+aSender.ClientSocket.GetRemoteSinIP+','+aRecvMsg.Value('UID','')+')';

    Query.Active:=false;
    Query.SQL.Text := 'SELECT * FROM Users WHERE Name="'+aSender.PlayerName+'"';
    Query.Active:=true;

    //update needed?
    if (aRecvMsg.Value('Version',0)<nwServerVersion) then
    begin
      aRecvMsg.Clear;
      aRecvMsg.Values['MessageType']:='nwDisconnect';
      aRecvMsg.Values['Sender']:=aSender.PlayerName;
      aRecvMsg.Values['Receiver']:=aSender.PlayerName;
      aRecvMsg.Values['Message']:='Error_Release';
      DebugMsg:=DebugMsg+' but needs to update.';
      exit;
    end;

    //banned?
    s := aRecvMsg.Value('UID','ABC');
    if (s<>'') and (s<>'ABC') then //MacOS doesn't send MacAddr
    begin
      with TIniFile.Create('ScrabbleServer.ini') do
      try
        if ReadBool('Banned_UID',s,false) then
        begin
          aRecvMsg.Clear;
          aRecvMsg.Values['MessageType']:='nwDisconnect';
          aRecvMsg.Values['Sender']:=aSender.PlayerName;
          aRecvMsg.Values['Receiver']:=aSender.PlayerName;
          aRecvMsg.Values['Message']:='Error_Banned';
          DebugMsg:=DebugMsg+' but is banned';
          exit;
        end;
      finally
        Free;
      end
    end;

    //register user
    if (aRecvMsg.Value('Password','')<>'') and (Query.RecordCount=0) then
    begin
      //number of accounts by this user
      if (s<>'') and (s<>'ABC') then
      begin
        Query.Active:=false;
        Query.SQL.Text := 'SELECT * FROM Users WHERE lower(LastMac)=lower("'+s+'")';
        Query.Active:=true;
        if Query.RecordCount>MaxNumberOfAccounts then
        begin
          aRecvMsg.Clear;
          aRecvMsg.Values['MessageType']:='nwDisconnect';
          aRecvMsg.Values['Sender']:=aSender.PlayerName;
          aRecvMsg.Values['Receiver']:=aSender.PlayerName;
          aRecvMsg.Values['Message']:='Error_Accounts';
          DebugMsg:=DebugMsg+' but has too much accounts';
          exit;
        end;
      end;

      //create new user
      Query.Cancel;
      Query.Append;
      Query.FieldByName('Name').AsString := aSender.PlayerName;
      Query.FieldByName('Passwd').AsString := aRecvMsg.Value('Password','');
      Query.FieldByName('GamesPlayed').AsInteger := 0;
      Query.FieldByName('Rating').AsInteger := 1000;
      Query.FieldByName('Registered').AsDateTime := Now;
      Query.FieldByName('LastConnect').AsDateTime := Now;
      Query.FieldByName('LastIP').AsString := aSender.ClientSocket.GetRemoteSinIP;
      Query.FieldByName('LastMac').AsString :=aRecvMsg.Value('UID','');
      Query.FieldByName('Messages').AsString :='';
      Query.FieldByName('AllowKibitz').AsBoolean :=true;
      Query.FieldByName('Email').AsString := '';//aRecvMsg.Value('Email','');
      Query.Post;
      Query.ApplyUpdates; //to apply update
    end;


    //once again because player might have been added
    Query.Active:=false;
    Query.SQL.Text:='SELECT * FROM Users WHERE Name="'+aSender.PlayerName+'"';
    Query.Active:=true;
    Query.First;
    if Query.RecordCount<>1 then
      Log(etWarning,Format('DoConnectCnt: '+Query.SQL.Text+':%d',[Query.RecordCount]));

    //empty or wrong password
    s:=aRecvMsg.Value('Password','');
    if (s='') or (s<>Query.FieldByName('Passwd').AsString) then
    begin
      aRecvMsg.Clear;
      aRecvMsg.Values['MessageType']:='nwDisconnect';
      aRecvMsg.Values['Sender']:=aSender.PlayerName;
      aRecvMsg.Values['Receiver']:=aSender.PlayerName;
      aRecvMsg.Values['Message']:='Error_Password';
      DebugMsg:=DebugMsg+' but uses wrong password';
      exit;
    end;

    //read player info
    aSender.Country:=aRecvMsg.Value('Country','gb');
    aSender.Release:=aRecvMsg.Value('Release','0.0');
    aSender.City:=aRecvMsg.Value('City','');
    aSender.GamesPlayed:=Query.FieldByName('GamesPlayed').AsInteger;
    aSender.Rating:=Query.FieldByName('Rating').AsInteger;
    aSender.Registered:=Query.FieldByName('Registered').AsDateTime;
    aSender.AllowKibitz:=Query.FieldByName('AllowKibitz').AsBoolean;
    aEmail:=aRecvMsg.Value('Email','');

    //unique name for LAN (kick other player)
    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      //kick yourself
      if (aClient.PlayerName=aSender.PlayerName) and
         (aClient<>aSender) then
      begin
        aRecvMsg.Clear;
        aRecvMsg.Values['MessageType']:='nwInformation';
        aRecvMsg.Values['Sender']:=aClient.PlayerName;
        aRecvMsg.Values['Receiver']:=aClient.PlayerName;
        aRecvMsg.Values['Info']:='KickedByAnother';
        aClient.ClientSocket.SendString(aRecvMsg.DelimitedText+CRLF);
        aClient.Free;
        DebugMsg:=DebugMsg+' (same name/pw -> kicked)';
        break;
      end;
    end;

    //inform all players
    s:=aRecvMsg.Value('UID','');  //so ugly!
    aRecvMsg.Clear;
    aRecvMsg.Values['MessageType']:='nwLogin'; //handshake -> call from client
    aRecvMsg.Values['Sender']:=aSender.PlayerName;
    aRecvMsg.Values['Receiver']:='all';  {$Note Information to all somewhere else!}
    aRecvMsg.Values['Best']:=booltostr(aSender.Rating>=Best,true);
    aRecvMsg.Values['UID']:=s;

    //static player info
    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      s:=inttostr(i)+'_';
      aRecvMsg.Values[s+'Name']:=aClient.PlayerName;
      aRecvMsg.Values[s+'MenuLang']:=aClient.MenuLang;
      aRecvMsg.Values[s+'Country']:=aClient.Country;
      aRecvMsg.Values[s+'City']:=aClient.City;
      aRecvMsg.Values[s+'Release']:=aClient.Release;
      aRecvMsg.Values[s+'Registered']:=floattostr(aClient.Registered);
    end;
    aRecvMsg.Values['Count']:=inttostr(Clients.Count);

    //server info
    aRecvMsg.Values['News']:=ServerNews;
    aRecvMsg.Values['TimeToAfk']:=TimeToAfk;

    //user messages
    with TStringList.Create do
    try
      Delimiter:=#7;
      StrictDelimiter:=true;
      DelimitedText:=Query.FieldByName('Messages').AsString;
      for i:=0 to Count-1 do
        aRecvMsg.Values['Chat'+inttostr(i)]:=Strings[i];
    finally
      Free;
    end;

    //update database info
    Query.Edit;
    Query.FieldByName('Messages').AsString := '';
    Query.FieldByName('LastConnect').AsDateTime := Now;
    Query.FieldByName('LastIP').AsString := aSender.ClientSocket.GetRemoteSinIP;
    Query.FieldByName('LastMac').AsString :=aRecvMsg.Value('UID','');
    Query.FieldByName('Email').AsString :=aEmail;
    Query.Post;
    Query.ApplyUpdates; //to apply update

    DebugMsg:=DebugMsg+' successfully.';
  except
    on E:Exception do Log(etError,'DoConnect '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoRefresh(var aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  s:string;
  i:integer;
  aClient:TClient;
begin
  try
    s:=aRecvMsg.Value('SetState',''); //state has been change to e.g. afk
    if s<>'' then aSender.ClientState:=StringToClientState(s);

    aRecvMsg.Values['Count']:=inttostr(Clients.Count);
    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      s:=inttostr(i)+'_';
      aRecvMsg.Values[s+'Name']:=aClient.PlayerName;
      aRecvMsg.Values[s+'Rating']:=inttostr(aClient.Rating);
      aRecvMsg.Values[s+'Group']:=inttostr(aClient.GroupID);
      aRecvMsg.Values[s+'GameID']:=inttostr(aClient.GameID);
      aRecvMsg.Values[s+'GamesPlayed']:=inttostr(aClient.GamesPlayed);
      aRecvMsg.Values[s+'IsKibitz']:=booltostr(aClient.IsKibitz,true);
      aRecvMsg.Values[s+'AllowKibitz']:=booltostr(aClient.AllowKibitz,true);
      aRecvMsg.Values[s+'ClientState']:=ClientStateToString(aClient.ClientState);
      if aClient.GameID>0 then  //is a game is running
      begin
        with TIniFile.Create(inttostr(aClient.GameID)+'.ssv') do
        try
          aRecvMsg.Values[s+'GameStarted']:=ReadString('Info','Date','0.01');
          aRecvMsg.Values[s+'MoveNumber']:=inttostr(ReadInteger('Info','NumberOfMoves',0));
        finally
          Free;
        end;
      end else
      begin
        aRecvMsg.Values[s+'GameStarted']:='';
        aRecvMsg.Values[s+'MoveNumber']:='-1';
      end;
    end;
  except
    on E:Exception do Log(etError,'DoRefresh '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoHighscore(var aRecvMsg: TNetworkMessage);
var
  i:integer;
begin
  try
    Query.Active:=false;
    //http://stackoverflow.com/questions/1764881/mysql-getting-data-for-histogram-plot
    Query.SQL.Text:='SELECT floor(Rating/50)*50 AS bin,count(*) AS cnt'+
                    ' FROM Users WHERE GamesPlayed>'+inttostr(MinGamesPlayed)+
                    ' GROUP BY floor(Rating/50)*50 ORDER BY floor(Rating/50)*50';
    Query.Active:=true;
    Query.First;
    i:=0;
    while not Query.Eof do
    begin
      aRecvMsg.Values['Bin'+inttostr(i)]:=Query.FieldValues['bin'];
      aRecvMsg.Values['Cnt'+inttostr(i)]:=Query.FieldValues['cnt'];
      inc(i);
      Query.Next;
    end;
    aRecvMsg.Values['Count']:=inttostr(i);

    Query.Active:=false;
    //http://thinkdiff.net/mysql/how-to-get-rank-using-mysql-FQuery/
    Query.SQL.Text:='SET @rank=0';
    Query.ExecSQL;
    Query.SQL.Text:='SELECT rank, name, Rating FROM (SELECT @rank:=@rank+1 AS rank,Name, Rating'+
                    ' FROM Users WHERE GamesPlayed>'+inttostr(MinGamesPlayed)+
                    ' ORDER BY Rating DESC) as result Where Name="'+aRecvMsg.Sender+'"';
    Query.Active:=true;

    if Query.RecordCount=1 then
    begin
      aRecvMsg.Values['Own']:=Query.FieldValues['rank'];
      Query.Active:=false;
      Query.SQL.Text := 'SELECT Rating FROM Users WHERE GamesPlayed>'+inttostr(MinGamesPlayed);
      Query.Active:=true;
      Query.Last;
      aRecvMsg.Values['Overall']:=inttostr(Query.RecordCount);
    end;
  except
    on E:Exception do Log(etError,'DoHighscore '+aRecvMsg.Sender+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoChat(var aRecvMsg: TNetworkMessage);
var
  s,aMailTo:string;
  aSL:TStringList;
  i:integer;
  aClient:TClient;
begin
  try
    if (aRecvMsg.Receiver<>'group') and
       (aRecvMsg.Receiver<>'all') and
       (aRecvMsg.Receiver<>'kibitzes') then
    begin
       //online?
      for i:=0 to Clients.Count-1 do
      begin
        aClient:=TClient(Clients[i]);
        if aClient.PlayerName=aRecvMsg.Receiver then
          exit;
      end;
      //offline!
      Query.Active:=false;
      Query.SQL.Text := 'SELECT * FROM Users WHERE Name="'+aRecvMsg.Receiver+'"';
      Query.Active:=true;
      Query.First;
      if Query.RecordCount>0 then //FieldValues['LastConnect']<>'' then
      begin
        aMailTo:=Query.FieldByName('Email').AsString;
        s:=Query.FieldByName('Messages').AsString;
        if s<>'' then s:=s+#7;
        s:=s+aRecvMsg.Sender+' ('+DateTimeToStr(Now)+'): '+aRecvMsg.Value('Chat','');

        Query.Edit;
        Query.FieldByName('Messages').AsString := s;
        Query.Post;
        Query.ApplyUpdates;

        if aMailTo<>'' then
        begin
          aSL:=TStringList.Create;
          try
            aSL.Add('From: Scrabble3D <heiko_tietze@web.de>');
            aSL.Add('To: ' + aMailTo);
            aSL.Add('Date: ' + Rfc822DateTime(now));
            aSL.Add('Subject: New Message Received');
            aSL.Add('X-mailer: Scrabble3D - powered by Synapse');
            aSL.Add('');

            aSL.Add('You received a new message on Scrabble3D-Server:');
            aSL.Add('');
            aSL.Add('Sender: '+aRecvMsg.Sender);
            aSL.Add('Time: '+DateTimeToStr(Now));
//            aSL.Add('Message: '+aRecvMsg.Value('Chat','')); //no unicode characters in plain text mails
            aSL.Add('');
            aSL.Add('Please don''t reply to this message');
            if not SendToRaw('Scrabble3D <heiko_tietze@web.de>', aMailTo, 'localhost', aSL, '', '') then
              Log(etWarning,'DoChat: Send mail failed');
          finally
            aSL.Free;
          end;
        end;

        aRecvMsg.Values['Receiver']:=aRecvMsg.Sender;
        aRecvMsg.Values['MessageType']:='nwInformation';
        aRecvMsg.Values['Info']:='ChatStored';
      end else
      begin
        //unknown
        aRecvMsg.Values['Receiver']:=aRecvMsg.Sender;
        aRecvMsg.Values['MessageType']:='nwInformation';
        aRecvMsg.Values['Info']:='ChatUnknownReceiver';
      end;
    end;
  except
    on E:Exception do Log(etError,'DoChat '+aRecvMsg.Sender+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoInformation(var aRecvMsg: TNetworkMessage);
const
  MaxBest=20;
var
  i:integer;
  s:string;
begin
  try
    aRecvMsg.Values['Result']:='';
    case aRecvMsg.Value('Info','') of
     'finger':
     begin
       //ranking
       Query.Active:=false;
       //http://thinkdiff.net/mysql/how-to-get-rank-using-mysql-FQuery/
       Query.SQL.Text:='SET @rank=0';
       Query.ExecSQL;
       Query.SQL.Text:='SELECT rank, name, Rating FROM (SELECT @rank:=@rank+1 AS rank,Name, Rating'+
                       ' FROM Users WHERE GamesPlayed>'+inttostr(MinGamesPlayed)+
                       ' ORDER BY Rating DESC) as result Where Name="'+aRecvMsg.Value('Name','')+'"';
       Query.Active:=true;
       if Query.RecordCount=1 then
       begin
         s:=Query.FieldValues['rank'];
         Query.Active:=false;
         Query.SQL.Text := 'SELECT Rating FROM Users WHERE GamesPlayed>'+inttostr(MinGamesPlayed);
         Query.Active:=true;
         Query.Last;
         s:=s+' of '+inttostr(Query.RecordCount);
       end;

       //personal data
       Query.Active:=false;
       Query.SQL.Text := 'SELECT * FROM Users WHERE Name="'+aRecvMsg.Value('Name','')+'"';
       Query.Active:=true;
       Query.First;
       if Query.EOF then
         aRecvMsg.Values['Info']:='ChatUnknownReceiver' else
         aRecvMsg.Values['Result']:='Name: '+Query.FieldByName('Name').AsString+#13+
                                    '#Games: '+IntToStr(Query.FieldByName('GamesPlayed').AsInteger)+#13+
                          	    'Rating: '+IntToStr(Query.FieldByName('Rating').AsInteger)+#13+
                                    'Rank: '+s+#13+
                                    'Last connect: '+DateTimeToStr(Query.FieldByName('LastConnect').AsDateTime);
     end ;
     'best':
     begin
       Query.Active:=false;
       Query.SQL.Text := 'SELECT Name, Rating FROM Users'+
                         ' WHERE GamesPlayed>'+inttostr(MinGamesPlayed)+
                         ' ORDER BY Rating DESC';
       Query.Active:=true;
       Query.First;
       for i:=0 to MaxBest-1 do
       begin
         if Query.Eof then exit;
         aRecvMsg.Values['Result']:=aRecvMsg.Values['Result']+
                                    inttostr(i+1)+'. '+
                                    Query.FieldByName('Name').AsString+#9+
                                    inttostr(Query.FieldByName('Rating').AsInteger)+#13;
         Query.Next;
       end;
     end;
    end;//case
  except
    on E:Exception do Log(etError,'DoInformation '+aRecvMsg.Sender+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoInvite(var aRecvMsg: TNetworkMessage);
var
  i:integer;
begin
  try
    i:=aRecvMsg.Value('GameID',0);
    if (i>0) then
    begin
      if IsKibitzInGame(i,aRecvMsg.Receiver) then
        aRecvMsg.Add('JoinMode=1') else //as kibitz
        aRecvMsg.Add('JoinMode=2');     //as reinvitation to running game
    end else
      aRecvMsg.Add('JoinMode=0');       //as new group
  except
    on E:Exception do Log(etError,'DoInvite '+aRecvMsg.Sender+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoJoin(var aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  i:integer;
  aClient:TClient;
begin
  try
    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      if aClient.PlayerName=aRecvMsg.Value('Target','') then
      begin
        aSender.GroupID:=aClient.GroupID;
        aSender.IsKibitz:=IsKibitzInGame(aClient.GameID,aSender.PlayerName);
        aRecvMsg.Values['Receiver']:='group';//'all';//aRecvMsg.Sender;
        aRecvMsg.Values['Game']:=inttostr(aClient.GameID);//inttostr(Info.GameID);
        break;
      end;
    end;//for to Count
  except
    on E:Exception do Log(etError,'DoJoin '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoLeave(var aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  i,cnt:integer;
  StoredGroupID:longword;
  aClient:TClient;
begin
  try
    StoredGroupID:=aSender.GroupID;

    aSender.GroupID:=NextGroupId;
    aSender.IsKibitz:=false;
    aSender.GameID:=0;

    //kibitz must not be the last player within a group
    cnt:=0;
    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      if (aClient.GroupID=StoredGroupID) and
          not aClient.IsKibitz then
        inc(cnt);
    end;
    aRecvMsg.Values['GroupMembers']:=inttostr(cnt);

    //send here to (old) group because GroupID has been changed
    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      if (aClient.GroupID=StoredGroupID) or
         (aClient.PlayerName=aRecvMsg.Values['Sender']) then
       aClient.ClientSocket.SendString(aRecvMsg.DelimitedText+CRLF);
    end;

    aRecvMsg.Clear;
  except
    on E:Exception do Log(etError,'DoLeave '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoRemoteGames(var aRecvMsg: TNetworkMessage);
var
  cnt : integer;
  sr  : TSearchRec;
  b   : boolean;
begin
  try
    with TStringList.Create do
    try
      StrictDelimiter:=true;
      cnt:=0;
      if FindFirst('*.ssv',faAnyFile,sr)=0 then
      repeat
        with TIniFile.Create(sr.Name) do
        try
          b:=(DaysBetween(Now,ReadFloat('Info','LastAccess',0))<MaxDaysForStoredGames);{ and (ReadInteger('Info','NumberOfMoves',-1)>-1);}
          if b then
          begin
            CommaText:=ReadString('Info','Player','');
            if (IndexOf(aRecvMsg.Sender)>-1) and (ReadInteger('Info','NumberOfMoves',0)>0) then
            begin
              aRecvMsg.Values['Game'+inttostr(cnt)+'.Player']:=CommaText;
              aRecvMsg.Values['Game'+inttostr(cnt)+'.Sequence']:=ReadString('Info','Sequence','');
              aRecvMsg.Values['Game'+inttostr(cnt)+'.Date']:=ReadString('Info','Date',FloatToStr(Now));
              aRecvMsg.Values['Game'+inttostr(cnt)+'.Name']:=sr.Name;
              aRecvMsg.Values['Game'+inttostr(cnt)+'.LastAccess']:=ReadString('Info','LastAccess','0');
              aRecvMsg.Values['Game'+inttostr(cnt)+'.Moves']:=inttostr(ReadInteger('Info','NumberOfMoves',0));
              aRecvMsg.Values['Game'+inttostr(cnt)+'.GameEnd']:=BoolToStr(ReadBool('GameEnd',aRecvMsg.Sender,false),true);
              inc(cnt);
            end;
          end;
        finally
          Free;
        end; //with Tinifile
        if not b then
          ExecuteProcess('/bin/tar','--remove-files -rf backup.tar '+sr.Name, []);
//          DeleteFile(sr.Name);
      until FindNext(sr)<>0;
      aRecvMsg.Values['NumberOfGames']:=inttostr(cnt);
    finally
      Free;
    end;
  except
    on E:Exception do Log(etError,'DoRemoteGames '+aRecvMsg.Sender+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoBestValues(var aRecvMsg: TNetworkMessage);
var
  GameID:integer;
begin
  try
    //write post-hoc calculated values to save game and transmit values to mates
    GameID:=aRecvMsg.Value('GameID',0);
    if (GameID>0) and (FileExists(inttostr(GameID)+'.ssv')) then
    with TIniFile.Create(inttostr(GameID)+'.ssv') do
    try
      WriteString('Info','BestValues',aRecvMsg.Value('BestValues',''));
    finally
      Free;
    end;
  except
    on E:Exception do Log(etError,'DoBestValues '+aRecvMsg.Sender+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoKibitz(const aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  i:integer;
  b:boolean;
  aClient:TClient;
begin
  try
    b:=aRecvMsg.Value('SetKibitz',true);
    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      if (aClient.GroupID=aSender.GroupID) and
         not aClient.IsKibitz then  {$Note Kibitz does not set allowkibitz?}
      begin
        aClient.AllowKibitz:=b;
        //persistent to player
        Query.Active:=false;
        Query.SQL.Text := 'SELECT * FROM Users WHERE Name="'+aClient.PlayerName+'"';
        Query.Active:=true;
        Query.First;
        if Query.RecordCount=1 then
        begin
          Query.Edit;
          Query.FieldByName('AllowKibitz').AsBoolean := b;
          Query.Post;
          Query.ApplyUpdates; //to apply update
        end else
          Log(etWarning,Format('DoKibitz.RecCnt: '+Query.SQL.Text+':%d',[Query.RecordCount]));
      end;
    end;

    //persistent to game
    if aSender.GameID>0 then
    with TIniFile.Create(inttostr(aSender.GameID)+'.ssv') do
    try
      WriteBool('Info','AllowKibitz',b);
    finally
      Free;
    end;

  except
    on E:Exception do Log(etError,'DoKibitz '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoSyncNewGame(const aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  i,PlayerCnt:integer;
  s:string;
  aClient:TClient;
begin
  try
    inc(GameNumber);
    PlayerCnt:=0;
    s:='';

    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      if (aClient.GroupID=aSender.GroupID) and
        not aClient.IsKibitz then
     begin
       aRecvMsg.Add('Player'+inttostr(PlayerCnt+1)+'Name='+aClient.PlayerName);
       s:=s+aClient.PlayerName+',';
       aClient.GameID:=GameNumber;
       aClient.IsKibitz:=(PlayerCnt>3);
       aClient.AllowKibitz:=aRecvMsg.Value('AllowKibitz',false);
       inc(PlayerCnt);
     end;
    end;
    if PlayerCnt>4 then PlayerCnt:=4;
    aRecvMsg.Add('PlayerCount='+inttostr(PlayerCnt));
    aRecvMsg.Add('GameNumber='+inttostr(GameNumber));
    System.Delete(s,length(s),1);

    //create new server save game
    with TStringList.Create do
    try
      Add('[Info]');
      Add('Date='+FloatToStr(Now));
      Add('Player='+s);
      Add('LastAccess='+FloatToStr(Now));
      Add('SyncNewGame='+aRecvMsg.DelimitedText);
      SaveToFile(inttostr(aSender.GameID)+'.ssv');
    finally
      Free;
    end;

  except
    on E:Exception do Log(etError,'DoSyncNewGame '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoServerInfo(const aSender:TClient; var aRecvMsg: TNetworkMessage);
begin
  try
    with TIniFile.Create(inttostr(aSender.GameID)+'.ssv') do
    try
      WriteString('Info','Sequence',aRecvMsg.Value('Sequence',''));
    finally
      Free;
    end;
    aRecvMsg.Clear;
  except
    on E:Exception do Log(etError,'DoServerInfo '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoAddMove(const aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  i,aMoveNumber,aMNDummy,aCnt:integer;
  aNextPlayer,aMailTo,aMates,aStarted,aSeqDummy:string;
  aSL:TStringList;
  aClient:TClient;
begin
  try
    with TIniFile.Create(inttostr(aSender.GameID)+'.ssv') do
    try
      WriteFloat('Info','LastAccess',Now);
      //for remote game's move count
      if aRecvMsg.MessageType='nwNextPlayer' then
      begin
        aMNDummy:=ReadInteger('Info','NumberOfMoves',-1); //,0!
        if aMNDummy=-1 then
          aMoveNumber:=0 else
          aMoveNumber:=aMNDummy;
        inc(aMoveNumber);
        WriteInteger('Info','NumberOfMoves',aMoveNumber);
      end;
      //for loadgame
      i:=ReadInteger('Info','NumberOfActions',0);
      WriteInteger('Info','NumberOfActions',i+1);
      //content
      WriteString('Game',inttostr(i),aRecvMsg.DelimitedText);
      //next player
      aSeqDummy:='';
      with TStringList.Create do
      try
        Delimiter:=',';
        StrictDelimiter:=true;
        aSeqDummy:=ReadString('Info','Sequence','');
        CommaText:=aSeqDummy;
        aCnt:=Count;
{.$error hier}
        if Count>0 then
          aNextPlayer:=Strings[aMoveNumber mod Count] else
          exit;
        aStarted:=DateTimeToStr(ReadFloat('Info','Date',Now));
        aMates:=ReadString('Info','Player','');
      finally
        Free; //TStringList
      end;
    finally
      Free; //TIniFile
    end;

    //online?
    for i:=0 to Clients.Count-1 do
    begin
      aClient:=TClient(Clients[i]);
      if aClient.PlayerName=aNextPlayer then
        exit;
{#todo: send info to player that are online but not in group; current msg must not be modified}
{      aClient:=TClient(Clients[i]);
      if aClient.PlayerName=aNextPlayer then
      begin
        if aClient.GroupID<>aSender.GroupID then
        begin
          aRecvMsg.Values['Receiver']:=aClient.PlayerName;
          aRecvMsg.Values['MessageType']:='nwInformation';
          aRecvMsg.Values['Info']:='DoMove';
          aRecvMsg.Values['Game']:=inttostr(aSender.GameID);
          aRecvMsg.Values['Mates']:=aMates;
        end;
        exit;
      end;
}
    end;
    //offline!
    Query.Active:=false;
    Query.SQL.Text := 'SELECT * FROM Users WHERE Name="'+aNextPlayer+'"';
    Query.Active:=true;
    Query.First;
    if Query.RecordCount>0 then //FieldValues['LastConnect']<>'' then
    begin
      aMailTo:=Query.FieldByName('Email').AsString;

      if aMailTo<>'' then
      begin
        aSL:=TStringList.Create;
        try
          aSL.Add('From: Scrabble3D <heiko_tietze@web.de>');
          aSL.Add('To: ' + aMailTo);
          aSL.Add('Date: ' + Rfc822DateTime(now));
          aSL.Add('Subject: It''s your turn');
          aSL.Add('X-mailer: Scrabble3D - powered by Synapse');
          aSL.Add('');

          aSL.Add('It''s your turn in a game on Scrabble3D-Server:');
          aSL.Add('');
          aSL.Add('#Game: '+inttostr(aSender.GameID));
          aSL.Add('Players: '+aMates);
          aSL.Add('Started: '+aStarted);
          aSL.Add('');
          aSL.Add('Please don''t reply to this message.');
          if not SendToRaw('Scrabble3D <heiko_tietze@web.de>', aMailTo, 'localhost', aSL, '', '') then
            Log(etWarning,'DoAddMove: Send mail failed');
        finally
          aSL.Free;
        end;
      end;

    end;
  except
    on E:Exception do Log(etError,'DoAddMove '+aSender.PlayerName+': '+E.Message+' | '+
                                  Query.SQL.Text+' | '+
                                  'MoveNumber='+inttostr(aMNDummy)+',GameID='+inttostr(aSender.GameID)+',Seq='+aSeqDummy+',Cnt='+inttostr(aCnt));
  end;
end;

procedure TWanTCPServer.DoLoadGame(const aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  i,j,z:integer;
  aReceiver:string;
  sl:TStringList;
  aClient:TClient;
begin
  try
    if FileExists(aRecvMsg.Values['Name']) then
    begin
      aReceiver:=aRecvMsg.Receiver;

      sl:=TStringList.Create;
      sl.StrictDelimiter:=true;

      with TIniFile.Create(aRecvMsg.Values['Name']) do
      try
        sl.CommaText:=ReadString('Info','Player','');
        //game settings
        aRecvMsg.Clear;
        aRecvMsg.DelimitedText:=ReadString('Info','SyncNewGame','');
        aRecvMsg.Values['IsLoading']:=BoolToStr(true,true);
        if ValueExists('Info','AllowKibitz') then
          aRecvMsg.Values['AllowKibitz']:=ReadString('Info','AllowKibitz',aRecvMsg.Values['AllowKibitz']);

        //kibitz or not
        for i:=0 to Clients.Count-1 do
        begin
          aClient:=TClient(Clients[i]);
          if ((aClient.GroupID=aSender.GroupID) and (aReceiver='group')) or
             (aClient.PlayerName=aReceiver) then
          begin
            aClient.AllowKibitz:=aRecvMsg.Value('AllowKibitz',false);
            if sl.IndexOf(aClient.PlayerName)>-1 then
            begin
              aClient.GameID:=aRecvMsg.Value('GameNumber',0);
              aClient.IsKibitz:=false;
              aRecvMsg.Values['IsKibitz']:='false';
            end else
            begin
              aClient.GameID:=0;
              aClient.IsKibitz:=true;
              aRecvMsg.Values['IsKibitz']:='true';
            end;
            aClient.ClientSocket.SendString(aRecvMsg.DelimitedText+CRLF);
          end;//aClient
        end; //list

        //Actions
        z:=ReadInteger('Info','NumberOfActions',0);
        for i:=0 to z-1 do
        begin
          aRecvMsg.Clear;
          aRecvMsg.DelimitedText:=ReadString('Game',inttostr(i),'');
          aRecvMsg.Values['IsLoading']:=BoolToStr(true,true);
          aRecvMsg.Values['LoadingProgress']:=inttostr(round(100*((i+1)/z)));

          for j:=0 to Clients.Count-1 do
          begin
            aClient:=TClient(Clients[j]);
            if ((aClient.GroupID=aSender.GroupID) and (aReceiver='group')) or
               (aClient.PlayerName=aReceiver) then
             aClient.ClientSocket.SendString(aRecvMsg.DelimitedText+CRLF);
          end;
        end;//actions

        //best values
        aRecvMsg.Clear;
        aRecvMsg.Add('Sender=Server');
        aRecvMsg.Add('Receiver=group');
        aRecvMsg.Add('MessageType=nwBestValues');
        aRecvMsg.Add('BestValues='+ReadString('Info','BestValues',''));
        for i:=0 to Clients.Count-1 do
        begin
          aClient:=TClient(Clients[i]);
          if ((aClient.GroupID=aSender.GroupID) and (aReceiver='group')) or
             (aClient.PlayerName=aReceiver) then
           aClient.ClientSocket.SendString(aRecvMsg.DelimitedText+CRLF);
        end;//best

        //update client
        aRecvMsg.Clear;
        aRecvMsg.Add('Sender=Server');
        aRecvMsg.Add('Receiver=group');
        aRecvMsg.Add('MessageType=nwInformation');
        aRecvMsg.Add('Info=LoadingFinished');
        for i:=0 to Clients.Count-1 do
        begin
          aClient:=TClient(Clients[i]);
          if ((aClient.GroupID=aSender.GroupID) and (aReceiver='group')) or
            (aClient.PlayerName=aReceiver) then
          begin
            aRecvMsg.Values['IsGameEnd']:=BoolToStr(ReadBool('GameEnd',aClient.PlayerName,false),true);
            //client is not yet refreshed
            aRecvMsg.Values['IsKibitz']:=BoolToStr(aClient.IsKibitz,true);
            aClient.ClientSocket.SendString(aRecvMsg.DelimitedText+CRLF);
          end;
        end;//update

        aRecvMsg.Clear;
      finally
        sl.Free;
        Free;
      end;
    end else
      aRecvMsg.Clear;
  except
    on E:Exception do Log(etError,'DoLoadGame '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.DoGameResult(var aSender:TClient; var aRecvMsg: TNetworkMessage);
var
  i: integer;
  s:string;
  MyResult,OppResult : integer;
  p: double;
begin
  try

    with TIniFile.Create(inttostr(aSender.GameID)+'.ssv') do
    try
      WriteBool('GameEnd',aSender.PlayerName,true);
    finally
      Free;
    end;
//    aSender.GameID:=0;

    //reset vars
    MyResult:=aRecvMsg.Value('MyResult',-1);
    inc(aSender.GamesPlayed);
    aRecvMsg.Values['OldRating']:=inttostr(aSender.Rating);

    //if found
    if MyResult>-1 then
    for i:=0 to aRecvMsg.Value('NumberOfPlayers',0)-1 do
    begin
      s:=aRecvMsg.Value('Player'+inttostr(i)+'Name','');
      OppResult:=aRecvMsg.Value('Player'+inttostr(i)+'Value',1000);
      if (s=aSender.PlayerName) then Continue;
      Query.Active:=false;
      Query.SQL.Text := 'SELECT * FROM Users WHERE Name="'+s+'"';
      Query.Active:=true;
      Query.First;
      if Query.RecordCount=1 then
      begin
        p:=1/(1+Exp(0.0031879*(Query.FieldByName('Rating').AsInteger-aSender.Rating)));
        if MyResult<OppResult then
          p:=0-p else
        if MyResult=OppResult then
          p:=0.5-p else
          p:=1-p;
        p:=p/(aRecvMsg.Value('NumberOfPlayers',0)-1);
        if aSender.GamesPlayed<50 then
        begin
          if aSender.Rating<1800 then inc(aSender.Rating,round(p*30)) else
          if aSender.Rating<2000 then inc(aSender.Rating,round(p*24)) else
                                      inc(aSender.Rating,round(p*15));
        end else
        begin
          if aSender.Rating<1800 then inc(aSender.Rating,round(p*20)) else
          if aSender.Rating<2000 then inc(aSender.Rating,round(p*16)) else
                                      inc(aSender.Rating,round(p*10));
        end;
      end else
        Log(etWarning,Format('DoGameResultCnt1: '+Query.SQL.Text+' (a): %d',[Query.RecordCount]));
    end;

    aRecvMsg.Values['NewRating']:=inttostr(aSender.Rating);

    //update new rating in db
    Query.Active:=false;
    Query.SQL.Text := 'SELECT * FROM Users WHERE Name="'+aSender.PlayerName+'"';
    Query.Active:=true;
    Query.First;
    if Query.RecordCount=1 then
    begin
      Query.Edit;
      Query.FieldByName('GamesPlayed').AsInteger:=aSender.GamesPlayed;
      Query.FieldByName('Rating').AsInteger:=aSender.Rating;
      Query.Post;
      Query.ApplyUpdates; //to apply update
    end else
      Log(etWarning,Format('DoGameResultCnt2: '+Query.SQL.Text+' (b): %d',[Query.RecordCount]));

    if aSender.Rating>best then
      best:=aSender.Rating;
  except
    on E:Exception do Log(etError,'DoGameResult '+aSender.PlayerName+': '+E.Message+' ('+Query.SQL.Text+')');
  end;
end;

procedure TWanTCPServer.CheckMessage(var Sender:TClient; var aRecvMsg: TNetworkMessage);
var
  debugstart : TDateTime;
begin
  DebugMsg:=': '+aRecvMsg.Sender+'->'+aRecvMsg.Receiver+': '+aRecvMsg.MessageType;
  debugstart:=now;
  try
    case aRecvMsg.MessageType of
     'nwConnect'    : DoConnect(Sender, aRecvMsg);
     'nwRefresh'    : DoRefresh(Sender, aRecvMsg);
     'nwHighscore'  : DoHighscore(aRecvMsg);
     'nwChat'       : DoChat(aRecvMsg);
     'nwInformation': DoInformation(aRecvMsg);
     'nwInvite'     : DoInvite(aRecvMsg);
     'nwJoin'       : DoJoin(Sender, aRecvMsg);
     'nwLeave'      : DoLeave(Sender, aRecvMsg); //kibitz
     'nwRemoteGames': DoRemoteGames(aRecvMsg);
     'nwBestValues' : DoBestValues(aRecvMsg);
     'nwKibitz'     : DoKibitz(Sender, aRecvMsg);//kibitz
     'nwSyncNewGame': DoSyncNewGame(Sender, aRecvMsg);
     'nwServerInfo' : DoServerInfo(Sender, aRecvMsg);
     'nwLoadGame'   : DoLoadGame(Sender, aRecvMsg);
     'nwGameResult' : DoGameResult(Sender, aRecvMsg);
     'nwNextPlayer','nwCambioSecco','nwChallenge','nwStartNewGame','nwEndGame','nwAddTime': DoAddMove(Sender, aRecvMsg);
     'nwLogout': Log(etInfo,aRecvMsg.Sender+' disconnected ('+inttostr(Clients.Count)+')');{$Note Legacy stuff}
     else exit;
    end;//case
  finally
    Log(etInfo,DebugMsg+' ('+inttostr(Trunc((Now-debugstart)*1000*60*60*24))+'ms)');
  end;
end;

constructor TWanTCPServer.Create(aPort: string; aUser,aPasswd,aDatabase:string);
var
  sl:TStringList;
  aTimeout:integer;
begin
  inherited Create(aPort,false);
  Clients.OnClientRemove:=@DoClientRemove;
  //read last game number
  with TIniFile.Create('ScrabbleServer.ini') do
  try
    GameNumber:=ReadInteger('General','GameNumber',0);

    MaxDaysForStoredGames:=ReadInteger('General','MaxDaysForStoredGames',7);
    MaxNumberOfAccounts:=ReadInteger('General','MaxNumberOfAccounts',3);
    MinGamesPlayed:=ReadInteger('General','MinGamesPlayed',9);
    AdminEmail:=ReadString('General','Email','');
    aTimeOut:=ReadInteger('General','TimeOut',-1);

    ServerNews:=ReadString('News','Current','');
    TimeToAfk:=ReadString('General','TimeToAfk','600000');
  finally
    Free;
  end;

  {$ifdef sql50}MySQL:=TMySQL50Connection.Create(nil);{$endif}
  {$ifdef sql55}MySQL:=TMySQL55Connection.Create(nil);{$endif}
  {$ifdef sql56}MySQL:=TMySQL56Connection.Create(nil);{$endif}
  Trans := TSQLTransaction.Create(nil);
  try
    with MySQL do
    begin
      HostName := '127.0.0.1';
      UserName := aUser;
      Password := aPasswd;
      DatabaseName := aDatabase;
      Connected:=true;
      Transaction := Trans;
    end;

    Query:=TSQLQuery.Create(nil);
    with Query do
    begin
      DataBase := MySQL;
      UpdateMode := upWhereKeyOnly; //upWhereChanged;
      SQL.Text := 'SET CHARACTER SET `utf8`';
      ExecSQL;
      try
      if aTimeOut>-1 then
      begin
        SQL.Text := 'SET WAIT_TIMEOUT='+inttostr(aTimeOut);
        ExecSQL;
      end;

      except
        on E:Exception do Log(etWarning,'Start: '+E.Message+' ('+Query.SQL.Text+')');
      end;
    end;

    sl:=TStringList.Create;
    try
      MySQL.GetFieldNames('Users',sl);
      if sl.CommaText<>'Name,Passwd,Key,GamesPlayed,Rating,Registered,LastConnect,LastIP,LastMac,Messages,AllowKibitz,Email' then
        raise EDatabaseError.Create('Table does not contain right fields: Name,Passwd (Text),Key,GamesPlayed,Rating (int), Registered,LastConnect (datetime), LastIP,LastMac,Messages (text), AllowKibitz (tinyint), Email (varchar255)');
    finally
      sl.Free;
    end;

  except
    on E:Exception do
     begin
       writeln('Fatal error: '+E.Message);
       halt;
     end;
  end;

  SysLog:=TEventLog.Create(nil);
  SysLog.LogType:=ltFile;
  Syslog.FileName:='/var/log/Scrabble3D.log';
  SysLog.Identification := 'Scrabble3D';
  SysLog.AppendContent:=true;
  Log(etInfo,DateTimeToStr(Now)+': Server started');
end;

destructor TWanTCPServer.Destroy;
begin
  Log(etInfo,DateTimeToStr(Now)+': Server destroyed');
  Clients.OnClientRemove:=nil;//no more logout info to clients

  //save last game number
  with TIniFile.Create('ScrabbleServer.ini') do
  try
    WriteInteger('General','GameNumber',GameNumber);
  finally
    Free;
  end;

  if assigned(Syslog) then
  begin
    Syslog.Active:=false;
    Syslog.Free;
  end;

  Query.Free;
  Trans.Commit;
  Trans.Free;
  MySQL.Free;

  inherited Destroy;
end;

{$R *.res}

function CheckParameter(out _user,_passwd,_database:string):boolean;
var
  i:integer;
begin
  Write('ScrabbleServer v0.2: Compiled at ',{$I %TIME%});
  Writeln(' on ',{$I %DATE%});
  Write('Compiler: fpc ',{$I %FPCVERSION%});
  Writeln(' for ',{$I %FPCTARGET%});
  {$ifdef sql50}Writeln('MySQL target: 5.0');{$endif}
  {$ifdef sql55}Writeln('MySQL target: 5.5');{$endif}
  {$ifdef sql56}Writeln('MySQL target: 5.6');{$endif}
  for i:=1 to ParamCount do
   case ParamStr(i) of
    '-u': _user:=ParamStr(i+1);
    '-p': _passwd:=ParamStr(i+1);
    '-d': _database:=ParamStr(i+1);
    '-h','-?': begin
                 writeln('Parameters:');
                 writeln(' -u <user>');
                 writeln(' -p <password>');
                 writeln(' -d <database>');
                 writeln(' -h (Shows this help)');
                 Result:=false;
                 exit;
               end;
   end;
  if _user='' then
  begin
    write('User: ');
    readln(_user);
  end;
  if _passwd='' then
  begin
    write('Password: ');
    _passwd:='';
    readln(_passwd);
  end;
  if _database='' then
  begin
    write('Database: ');
    readln(_database);
  end;
  Result:=true;
end;

var
  _user,_passwd,_database:string;

begin
  if not CheckParameter(_user,_passwd,_database) then
    exit;

  with TWanTCPServer.Create(cPort,_user,_passwd,_database) do
  try
    //find highest value
    Query.Active:=false;
    Query.SQL.Text := 'SELECT * FROM Users ORDER BY Rating DESC';
    Query.Active:=true;
    Query.First;
    Best:=Query.FieldByName('Rating').AsInteger;

    Start;

    repeat
      sleep(10);
    until Keypressed or Terminated;
  finally
    Free;
  end;
end.
