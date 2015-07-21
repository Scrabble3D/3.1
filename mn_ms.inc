{ messages }

procedure TfmMain.edChatKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var i:integer;
    aMessage:TStringList;
    aCmd,aMsg,aRec:string;
begin
  case key of
   VK_UP:
    if (ChatHistory.Tag{%H-}{%H-}<ChatHistory.Count-1) then
    begin
      ChatHistory.Tag:=ChatHistory.Tag+1;
      (Sender as TEdit).Text:=ChatHistory[ChatHistory.Tag];
    end;
   VK_DOWN:
    if (ChatHistory.Count>0) and (ChatHistory.Tag>0) then
    begin
      ChatHistory.Tag:=ChatHistory.Tag-1;
      (Sender as TEdit).Text:=ChatHistory[ChatHistory.Tag];
    end;
   VK_RETURN:
    begin
      if (Sender as TEdit).Text<>'' then
      try
        if IsGameServer and ((Sender as TEdit).Text[1]='/') then
        begin
          aMessage:=TStringList.Create;
          try
            aMessage.Delimiter:=' ';
            aMessage.DelimitedText:=Copy(edChat.Text,2,length(edChat.Text));
            if aMessage.Count=0 then
              raise EScrabbleError.Create(rCommand_Wrong);
            aMsg:=aMessage.Strings[0];
            aMessage.Delete(0);
            case aMsg of
             'whisper':
             begin
               aCmd:='nwChat';
               aMsg:='Chat=';
               if aMessage.Count>0 then
               begin
                 aRec:=aMessage.Strings[0];
                 aMessage.Delete(0);
                 if aMessage.Count=0 then
                   raise EScrabbleError.Create(rCommand_Wrong);
               end else
                 raise EScrabbleError.Create(rCommand_Wrong);
             end;
             'say':
             begin
               aCmd:='nwChat';
               aMsg:='Chat=';
               aRec:='group';
             end;
             'shout':
             begin
               aCmd:='nwChat';
               aMsg:='Chat=';
               aRec:='all';
             end;
             'tattle':
             begin
               aCmd:='nwChat';
               aMsg:='Chat=';
               aRec:='kibitzes';
             end;
             'finger':
             begin
               aCmd:='nwInformation';
               aRec:=TCPClient.PlayerData.PlayerName;
               aMsg:='Info=finger';
               if aMessage.Count>0 then
               begin
                 aMsg:=aMsg+nwDelimiter+'Name='+aMessage.Strings[0];
                 aMessage.Delete(0);
               end else
                 raise EScrabbleError.Create(rCommand_Wrong);
               aMessage.Clear;
             end;
             'best':
             begin
               aCmd:='nwInformation';
               aRec:=TCPClient.PlayerData.PlayerName;
               aMsg:='Info=best';
               aMessage.Clear;
             end;
             'help':
             begin
               OnMessage(smInformation,rCommand_Help1+#13+rCommand_Help2+#13+
                                       rCommand_Help3+#13+rCommand_Help4+#13+
                                       rCommand_Help5+#13+rCommand_Help6+#13+
                                       rCommand_Help7);
             end
             else raise EScrabbleError.Create(rCommand_Unknown);
            end; //case
            for i:=0 to aMessage.Count-1 do
              aMsg:=aMsg+aMessage.Strings[i]+' ';
          finally
            aMessage.Free;
          end;
        end else //if Text[1]='/'
        begin
          aCmd:='nwChat';
          aMsg:='Chat='+(Sender as TEdit).Text;
          if IsGameServer then
          begin
            case cbChatReceiver.Items.IndexOf(cbChatReceiver.Text) of
//             -1 : OnMessage(smError,rMain_UnknownReceiver);
              0 : aRec:='group';
              1 : aRec:='all';
              2 : aRec:='kibitzes';
             else
             begin
               aRec:=cbChatReceiver.Text;
               OnMessage(smChat,rMain_Whisper+' '+cbChatReceiver.Text+': '+(Sender as TEdit).Text);
             end;
            end//case
          end else
            aRec:='group';
        end;
        TCPClient.OnSend(aCmd,aRec,aMsg);
        ChatHistory.Add((Sender as TEdit).Text);
        ChatHistory.Tag:=ChatHistory.Count;
        (Sender as TEdit).Text:='';
      except
        on E:Exception do
        begin
          OnMessage(smInformation,rCommand_Help1+#13+rCommand_Help2+#13+rCommand_Help3+#13+rCommand_Help4+#13+
                                  rCommand_Help5+#13+rCommand_Help6+#13+rCommand_Help7);
          OnMessage(smError,E.Message);
        end;
      end;
    end;
  end;//case
end;

procedure TfmMain.cbChatReceiverDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  num:array[0..2] of byte=(7,2,32);
var
  bmp:TBitmap;
  bRect:TRect;
begin
  with (Control as TComboBox) do
  begin
    Canvas.Brush.Style:=bsClear;//gtk2
    {$ifdef _Linux}
    if ThemeServices.ThemesEnabled then
      ThemeServices.DrawElement(Canvas.Handle,ThemeServices.GetElementDetails(tcComboBoxDontCare), aRect, nil)
    else
    {$endif}
    begin
      if odSelected in State then
        Canvas.Brush.Color:=clHighlight else
        Canvas.Brush.Color:=clBtnFace;
      Canvas.FillRect(aRect);
    end;
    if index<3 then
    begin
      bmp:=TBitmap.Create;
      ilMain.GetBitmap(num[index],bmp);
      try
        {$ifdef Windows}
        bRect:=Bounds(aRect.Left+2,aRect.Top+2,ItemHeight-4,ItemHeight-4);
        {$else}
        bRect:=Bounds(aRect.Left+2,aRect.Top+2,ItemHeight,ItemHeight);
        {$endif}
        Canvas.StretchDraw(bRect,bmp);
        aRect.Left:=aRect.Left+ItemHeight+2;
      finally
        bmp.Free;
      end;
    end;
    Canvas.TextOut(aRect.Left+2,
                   aRect.Top+((aRect.Bottom-aRect.Top) div 2-Canvas.TextHeight('ABC') div 2),
                   Items[Index]);
  end;
end;

procedure TfmMain.cbChatReceiverMeasureItem(Contro1l: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  if index>-1 then
    aHeight:=23 else
    aHeight:=16;
end;

procedure TfmMain.DoException(Sender: TObject; E: Exception);
begin
  if assigned(OnMessage) then
    OnMessage(smError,'An unexpected error has occured: '+E.Message);
end;

procedure TfmMain.DoUpdatePoints(Who: byte; Value: word);
begin
  sbPoints:=Value;
  sbWho:=Who;
  pbPoints.Repaint;
end;

function TfmMain.DoGetBonusText(const x, y, z: byte): string;
begin
  case Scrabble.FieldType[x,y,z] of
    ftDoubleLetter : Result:=rScrabbleGrid_DoubleLetterScore;
    ftTripleLetter : Result:=rScrabbleGrid_TripleLetterScore;
    ftQuadLetter   : Result:=rScrabbleGrid_QuadLetterScore;
    ftDoubleWord   : Result:=rScrabbleGrid_DoubleWordScore;
    ftTripleWord   : Result:=rScrabbleGrid_TripleWordScore;
    ftQuadWord     : Result:=rScrabbleGrid_QuadWordScore;
    ftMalusSingleLetter:Result:='-'+rScrabbleGrid_SingleLetterScore;
    ftMalusDoubleLetter:Result:='-'+rScrabbleGrid_DoubleLetterScore;
    ftMalusTripleLetter:Result:='-'+rScrabbleGrid_TripleLetterScore;
    ftMalusQuadLetter:Result:='-'+rScrabbleGrid_QuadLetterScore;
    else Result:='';
  end;//case
end;

procedure TfmMain.DoGetMessageFont(aMessageType: TScrabbleMessageType; aSender: string; out aBackgroundColor: TColor; out aFont: TFont);
var i     : integer;
    h,l,s : byte;
begin
  case aMessageType of
   smError : {%H-}aFont.Style:=[fsBold];
   smDebug : aFont.Style:=[fsItalic];
   smChat  :
    for i:=0 to Scrabble.NumberOfPlayers-1 do
     if aSender=Scrabble.Player[i].Name then
     begin
       case i of
        0 : ColorToHLS(fmGameOptions.cbPlayer1.ButtonColor,h,l,s);
        1 : ColorToHLS(fmGameOptions.cbPlayer2.ButtonColor,h,l,s);
        2 : ColorToHLS(fmGameOptions.cbPlayer3.ButtonColor,h,l,s);
        3 : ColorToHLS(fmGameOptions.cbPlayer4.ButtonColor,h,l,s);
       end; //case
       aBackGroundColor:=HLStoColor(h,round(255*fmGameOptions.tbLuminance.Position/100),s);
       aFont.Color:=LightOrDark(aBackGroundColor);
       break;
     end;
  end;//case
end;

function TfmMain.DoGetPopupSetting(aMessageType: TScrabbleMessageType): boolean;
begin
  case aMessageType of
    smError:Result:=fmGameOptions.cbError.Checked;
    smWarning:Result:=fmGameOptions.cbWarnings.Checked;
    smInformation:Result:=fmGameOptions.cbInformation.Checked;
    smChat:Result:=fmGameOptions.cbChat.Checked;
    smOwnMove:Result:=fmGameOptions.cbOwnMove.Checked;
    smGameResult:Result:=fmGameOptions.cbGameResult.Checked;
    else Result:=false;
  end;
end;

procedure TfmMain.DoSetPopupSetting(aMessageType: TScrabbleMessageType; aValue: boolean);
begin
  case aMessageType of
   smError:fmGameOptions.cbError.Checked:=aValue;
   smWarning:fmGameOptions.cbWarnings.Checked:=aValue;
   smInformation:fmGameOptions.cbInformation.Checked:=aValue;
   smChat:fmGameOptions.cbChat.Checked:=aValue;
   smOwnMove:fmGameOptions.cbOwnMove.Checked:=aValue;
   smGameResult:fmGameOptions.cbGameResult.Checked:=aValue;
  end;
  fmGameOptions.SaveSettings('');//save non-rule settings
end;
