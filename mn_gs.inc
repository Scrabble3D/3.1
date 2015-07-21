{ game server }

procedure TfmMain.miRefreshClick(Sender: TObject);
begin
  TCPClient.OnSend('nwRefresh',TCPClient.PlayerData.PlayerName,'');
end;

procedure TfmMain.miStateClick(Sender: TObject);
begin
  if miState.Checked then
    TCPClient.OnSend('nwRefresh','all','SetState='+ClientStateToString(csOpenForGames)) else
    TCPClient.OnSend('nwRefresh','all','SetState='+ClientStateToString(csNotOpen));
end;

procedure TfmMain.miLeaveClick(Sender: TObject);
begin
  TCPClient.OnSend('nwLeave','group','');
end;

procedure TfmMain.miInviteClick(Sender: TObject);
var
  aClientInfo:TClientInfo;
  s1,s2:string;
begin
  aClientInfo:=TClientInfo(tvGameServer.Selected.Data);

  s1:=Copy(aClientInfo.Release,1,Pos('_',aClientInfo.Release)-1);
  s2:=TCPClient.PlayerData.Release;
  s2:=Copy(s2,1,Pos('_',s2)-1);
  if s1<>s2 then
    OnMessage(smWarning,Language.Format(rMain_WarnRelease,[s1,s2]));

  TCPClient.OnSend('nwInvite',aClientInfo.PlayerName,
                 'GameID='+inttostr(TCPClient.PlayerData.GameID)+nwDelimiter+
                 'Language='+ExtractFileName(Language.LangFile)+nwDelimiter+
                 'Country='+TCPClient.PlayerData.Country+nwDelimiter+
                 'Dictionary='+Dictionary.Info[itFileName]);
end;

procedure TfmMain.miBestClick(Sender: TObject);
begin
  TCPClient.OnSend('nwInformation',TCPClient.PlayerData.PlayerName,'Info=best');
end;

procedure TfmMain.miKibitzClick(Sender: TObject);
begin
//  3.1.1-12
  TCPClient.PlayerData.IsKibitz:=true;
  Scrabble.GameState:=Scrabble.GameState+[gsKibitz];

  TCPClient.OnSend('nwJoin',TCPClient.PlayerData.PlayerName,'Target='+TClientInfo(tvGameServer.Selected.GetFirstChild.Data).PlayerName);
end;

procedure TfmMain.miMessageClick(Sender: TObject);
var
  aMsg,aRec:string;
begin
  if assigned(tvGameServer.Selected.Data) then
  begin
    aRec:=TClientInfo(tvGameServer.Selected.Data).PlayerName;
    if InputQuery(rMain_Chat+' '+aRec, '',aMsg) then
    begin
      TCPClient.OnSend('nwChat',aRec,'Chat='+aMsg);
      OnMessage(smChat,rMain_Whisper+' '+aRec+': '+aMsg);
    end;
  end;
end;

procedure TfmMain.tvGameServerAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  r: TRect;
  bmp: TBitmap;
  png: TPortableNetworkGraphic;
begin
  if not Node.HasChildren then
  begin
    r:=Node.DisplayRect(false);
    bmp:=TBitmap.Create;
    bmp.Width:=22;bmp.Height:=22;
    if cdsSelected in State then
      bmp.Canvas.Brush.Color:=tvGameServer.SelectionColor else
      bmp.Canvas.Brush.Color:=tvGameServer.BackgroundColor;
    bmp.Canvas.Brush.Style:=bsSolid;
    bmp.Canvas.FillRect(bmp.Canvas.ClipRect);
    png:=TPortableNetworkGraphic.Create;
    try
      if TClientInfo(Node.Data).IsKibitz then
        ilMain.Draw(bmp.Canvas,0,0,21) else
        ilMain.Draw(bmp.Canvas,0,0,20);
      if LoadFlagFromResource(TClientInfo(Node.Data).Country,png) then
        bmp.Canvas.StretchDraw(Bounds(9,13,9,6),png);
      case TClientInfo(Node.Data).ClientState of
       csOpenForGames:;
       csAfk     : ilSmallImages.StretchDraw(bmp.Canvas,4,Bounds(11,0,12,12));
       csNotOpen : ilSmallImages.StretchDraw(bmp.Canvas,5,Bounds(13,1,8,8));
       csBusy    : ilSmallImages.StretchDraw(bmp.Canvas,6,Bounds(11,1,10,10));
      end;
      if (Node.Level=0) then
        Sender.Canvas.Draw(R.Left, R.Top, bmp) else
        Sender.Canvas.Draw(R.Left+18, R.Top, bmp);
    finally
      bmp.Free;
      png.Free;
    end;
  end;
  PaintImages:=true;
end;

procedure TfmMain.tvGameServerGetImageIndex(Sender: TObject; Node: TTreeNode);
var i:integer;
begin
  if Node.HasChildren then
  begin
    if TClientInfo(Node.GetFirstChild.Data).AllowKibitz then
      i:=7 else i:=23;
  end else
  begin
    i:=-1;
    if TClientInfo(Node.Data).IsKibitz then
      i:=21 else i:=20;
  end;
  Node.ImageIndex:=i;
  Node.SelectedIndex:=i;
end;

procedure TfmMain.tvGameServerShowHint(Sender: TObject; HintInfo: PHintInfo);
 procedure CanvasTextOut(c:TCanvas; x,y:integer; s:string;var w:integer);
 begin
   c.Brush.Style:=bsClear;//gtk2
   c.TextOut(x,y,s);
   if c.TextWidth(s)>w then
     w:=c.TextWidth(s);
 end;

const VDist=18;
var p     : TPoint;
    png   : TPortableNetworkGraphic;
    aNode : TTreeNode;
    w1,w2 : integer;
begin
  p:=tvGameServer.ScreenToClient(Mouse.CursorPos);
  aNode:=tvGameServer.GetNodeAt(p.x,p.y);
  if (aNode<>nil) and not aNode.HasChildren then
  with TSpecialHintWindow.Create(self) do
  begin
    with TClientInfo(aNode.Data) do
    begin
      w1:=0;
      CanvasTextOut(Bitmap.Canvas,3,3+0*VDist,rMain_HintCountry,w1);
      CanvasTextOut(Bitmap.Canvas,3,3+1*VDist,rMain_HintCity,w1);
      CanvasTextOut(Bitmap.Canvas,3,3+2*VDist,rMain_HintMenu,w1);
      CanvasTextOut(Bitmap.Canvas,3,3+3*VDist,rMain_HintRating,w1);
      CanvasTextOut(Bitmap.Canvas,3,3+4*VDist,rMain_HintGames,w1);
      CanvasTextOut(Bitmap.Canvas,3,3+5*VDist,rMain_HintRegistered,w1);
      CanvasTextOut(Bitmap.Canvas,3,3+6*VDist,rMain_HintRelease,w1);
      w2:=0;
      CanvasTextOut(Bitmap.Canvas,w1+5,3+0*VDist,Country,w2);
      CanvasTextOut(Bitmap.Canvas,w1+5,3+1*VDist,City,w2);
      CanvasTextOut(Bitmap.Canvas,w1+5,3+2*VDist,MenuLang,w2);
      CanvasTextOut(Bitmap.Canvas,w1+5,3+3*VDist,inttostr(Rating),w2);
      CanvasTextOut(Bitmap.Canvas,w1+5,3+4*VDist,inttostr(GamesPlayed),w2);
      CanvasTextOut(Bitmap.Canvas,w1+5,3+5*VDist,DateToStr(Registered),w2);
      CanvasTextOut(Bitmap.Canvas,w1+5,3+6*VDist,Release,w2);

      png:=TPortableNetworkGraphic.Create;
      try
        if LoadFlagFromResource(Country,png) then
          Bitmap.Canvas.Draw(w1+w2+5,3+0*VDist+2,png);
        if LoadFlagFromResource(MenuLang,png) then
          Bitmap.Canvas.Draw(w1+w2+5,3+2*VDist+2,png);
      finally
        png.Free;
      end;
    end;

    p:=Mouse.CursorPos;
    SetBounds(p.x-5,p.y-5,w1+w2+25,130);
    Show;
  end;
end;

procedure TfmMain.pmGameServerPopup(Sender: TObject);
var i:integer;
begin
  for i:=0 to pmGameServer.Items.Count-1 do
    pmGameServer.Items[i].Enabled:=false;

  //refresh
  pmGameServer.Items[4].Enabled:=true;
  //best value
  pmGameServer.Items[6].Enabled:=true;

  if (tvGameServer.Selected<>nil) then
  begin
    if tvGameServer.Selected.HasChildren then
    with TClientInfo(tvGameServer.Selected.GetFirstChild.Data) do
    begin
      //kibitz
      pmGameServer.Items[0].Enabled:=(GroupID<>TCPClient.PlayerData.GroupID) and
                                     (AllowKibitz) and
                                     (tvGameServer.Selected.Text<>rMain_Joined);
    end else
    with TClientInfo(tvGameServer.Selected.Data) do
    begin
      //invite
      pmGameServer.Items[1].Enabled:=(GroupID<>TCPClient.PlayerData.GroupID) and
                                     ((TCPClient.PlayerData.Mates.Count<4) or (gsRunning in Scrabble.GameState)) and
                                     not (ClientState in [csNotOpen,csBusy,csAfk]) and
                                     not (gsKibitz in Scrabble.GameState);
      //leave
      if (PlayerName=TCPClient.PlayerData.PlayerName) and
         (tvGameServer.Selected.Parent<>nil) then
        pmGameServer.Items[2].Enabled:=true;
      //message
      pmGameServer.Items[3].Enabled:=(PlayerName<>TCPClient.PlayerData.PlayerName);
      //state
      pmGameServer.Items[7].Enabled:=(PlayerName=TCPClient.PlayerData.PlayerName);
    end;
  end;
end;

function SortByGroup(Item1, Item2: Pointer): Integer;
begin
  if TClientInfo(Item1).GroupID<TClientInfo(Item2).GroupID then Result:=-1 else
  if TClientInfo(Item1).GroupID>TClientInfo(Item2).GroupID then Result:=1 else
  if TClientInfo(Item1).IsKibitz and not TClientInfo(Item2).IsKibitz then Result:=1 else
  if not TClientInfo(Item1).IsKibitz and TClientInfo(Item2).IsKibitz then Result:=-1 else
  if TClientInfo(Item1).PlayerName<TClientInfo(Item2).PlayerName then Result:=-1 else
  if TClientInfo(Item1).PlayerName>TClientInfo(Item2).PlayerName then Result:=1 else
     Result:=0;
end;

procedure TfmMain.UpdateGameServer;
var aNode : TTreeNode;
    aList : TList;
    aInfo : TClientInfo;
    {%H-}aSelected  : string;
    i : integer;
begin
  tvGameServer.BeginUpdate;
  aList:=TList.Create;
  try
    if tvGameServer.Selected<>nil then
      aSelected:=tvGameServer.Selected.Text else
      aSelected:='';
    tvGameServer.Items.Clear;
    with TCPClient.PlayerList.LockList do
    try
      for i:=0 to Count-1 do
      begin
        aInfo:=TClientInfo(Items[i]);
        aList.Add(aInfo);
      end;
    finally
      TCPClient.PlayerList.UnlockList;
    end;
    aList.Sort(@SortByGroup);

    aNode:=nil;
    for i:=0 to aList.Count-1 do
    with TClientInfo(aList.Items[i]) do
    begin
      if (i>0) and (TClientInfo(aList.Items[i-1]).GroupID<>GroupID) then
        aNode:=nil;
      if (aNode=nil) and
         (((i<aList.Count-1) and (TClientInfo(aList.Items[i+1]).GroupID=GroupID)) or
          (GameStarted>0.1)) then
        aNode:=tvGameServer.Items.AddObject(nil,rMain_Joined,nil);

      if aNode<>nil then
      begin
        if (aNode.Text=rMain_Joined) and (GameStarted>0.1) then
          aNode.Text:=Language.Format(rMain_GameServerInfo,
                      [DateTimeToStr(GameStarted), MoveNumber+1]);
        tvGameServer.Items.AddChildObject(aNode,PlayerName,TClientInfo(aList.Items[i]));
      end else
        aNode:=tvGameServer.Items.AddObject(nil,PlayerName,TClientInfo(aList.Items[i]));
    end;
    //selection breaks data, i.e. caption is left aligned behind icon
{    if aSelected<>'' then
      tvGameServer.Selected:=tvGameServer.Items.FindNodeWithText(aSelected) else
      tvGameServer.Selected:=nil;}
  finally
    aList.Free;
    tvGameServer.EndUpdate;
  end;
end;

procedure TfmMain.UpdateScore;
var
  i,j,z,SumPoints,SumTime:integer;
begin
  bsScore.Clear;
  SumPoints:=0;
  SumTime:=0;
  for i:=0 to Scrabble.NumberOfPlayers-1 do
  begin
    z:=0;

    for j:=0 to GameCourse.HistoryCount-1 do
     if GameCourse.History[j]<>nil then  //when a move is being computed
      with TGameCourseMove(GameCourse.History[j]) do
       if (PlayerNumber=i) then
       begin
         inc(z,TimeUsed);
         inc(SumTime,TimeUsed);
       end;
    bsScore.AddXY(i+1,Scrabble.Player[i].Points,IntToStr(Scrabble.Player[i].Points)+' | '+SecondsToString(z),fmGameOptions.PlayerColor[i]);
    inc(SumPoints,Scrabble.Player[i].Points);
  end;
  chScore.Title.Text[0]:=rMain_StatSum+' '+inttostr(SumPoints)+' | '+SecondsToString(SumTime);
end;
