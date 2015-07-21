{ Message dialog

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

unit umessages;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Menus, Dialogs, StdCtrls, ExtCtrls, LResources,
  Grids,            //StringGrid
  Graphics,         //clWindow
  Clipbrd,
  LCLIntf, LCLType, //DrawText
  utypes,           //TScrabbleMessageType
  ulanguage         //MenuItems
  ;
type

  TScrabbleMessage=class
    msgType:TScrabbleMessageType;
    msgTime:TDateTime;
    msgSender:string;
    msgText:string;
  end;

  TOnGetPopupSetting=function(aMessageType:TScrabbleMessageType):boolean of object;
  TOnSetPopupSetting=procedure(aMessageType:TScrabbleMessageType;aValue:boolean) of object;
  TOnGetMessageFont=procedure(aMessageType:TScrabbleMessageType;aSender:string;out aBackgroundColor:TColor;out aFont:TFont) of object;
  { TScrabbleMessages }

  TScrabbleMessages=class
      procedure DoButtonClick(Sender: TObject);
      procedure DoButtonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure DoChangeFont(Sender: TObject);
      procedure DoClear(Sender: TObject);
      procedure DoCopyAll(Sender: TObject);
      procedure DoDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
      procedure DoGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
      procedure DoResize(Sender: TObject);
    private //visualization
      FLastMessageType:TScrabbleMessageType;
      FStringGrid:TStringGrid;
      FPopupMenu:TPopupMenu;
      FOnGetPopupSetting:TOnGetPopupSetting;
      FOnSetPopupSetting:TOnSetPopupSetting;
      FOnGetMessageFont:TOnGetMessageFont;
    private //popup window
      FForm:TForm;
      FImage:TImage;
      FMemo:TMemo;
      FBevel:TBevel;
      FCheckBox:TCheckBox;
      FButton:TButton;
    private //data
      FMessages:TList;
      FShowDebug: boolean;
      function GetFontName: string;
      function GetFontSize: byte;
      procedure SetBiDiMode(AValue: TBiDiMode);
      procedure SetFontName(AValue: string);
      procedure SetFontSize(AValue: byte);
    public
      constructor Create(aParent:TWinControl);
      destructor Destroy; override;

      procedure Clear;
      procedure Repaint;
      procedure UpdateLanguage;
      procedure AddMessage(aMsgType:TScrabbleMessageType; aMsg: string; aSender:string);

      property OnGetPopupSetting:TOnGetPopupSetting read FOnGetPopupSetting write FOnGetPopupSetting;
      property OnSetPopupSetting:TOnSetPopupSetting read FOnSetPopupSetting write FOnSetPopupSetting;
      property OnGetMessageFont:TOnGetMessageFont read FOnGetMessageFont write FOnGetMessageFont;
      property ShowDebugMessages:boolean read FShowDebug write FShowDebug;
      property BiDiMode:TBiDiMode write SetBiDiMode;
      property FontName:string read GetFontName write SetFontName;
      property FontSize:byte read GetFontSize write SetFontSize;
    end; //TScrabbleMessages

implementation

uses
  uscrabble, uconfig;

{ TScrabbleMessages }

constructor TScrabbleMessages.Create(aParent: TWinControl);
var
  aMenuItem: TMenuItem;
  aFontData: TFontData;
begin
  inherited Create;
  FMessages:=TList.Create;
  FStringGrid:=TStringGrid.Create(aParent);
  with FStringGrid do
  begin
    Parent:=aParent;
    Align:=alClient;
    BorderStyle:=bsNone;
    ColCount:=2;
    DefaultDrawing:=false;
    FixedCols:=1;
    FixedRows:=0;
    Flat:=true;
    Options:=[{goEditing,}goSmoothScroll,goThumbTracking];
    RowCount:=1;
    ShowHint:=true;
    OnResize:=@DoResize;
    OnDrawCell:=@DoDrawCell;
//    OnGetEditText:=@DoGetEditText;
  end;
  FPopupMenu:=TPopupMenu.Create(FStringGrid);
  FPopupMenu.Parent:=FStringGrid;
  FStringGrid.PopupMenu:=FPopupMenu;
  FPopupMenu.Items.Clear;
  aMenuItem:=TMenuItem.Create(FPopupMenu);
  with aMenuItem do
  begin
    Caption:=rMessages_Clear;
    OnClick:=@DoClear;
  end;
  FPopupMenu.Items.Add(aMenuItem);
  aMenuItem:=TMenuItem.Create(FPopupMenu);
  with aMenuItem do
  begin
    Caption:=rMessages_ChangeFont;
    OnClick:=@DoChangeFont;
  end;
  FPopupMenu.Items.Add(aMenuItem);
  aMenuItem:=TMenuItem.Create(FPopupMenu);
  with aMenuItem do
  begin
    Caption:=rMessages_CopyAll;
    OnClick:=@DoCopyAll;
  end;
  FPopupMenu.Items.Add(aMenuItem);
  FForm:=TForm.Create(nil);
  with FForm do
  begin
    Parent:=nil;
    Name:='fmMessages';
    BorderIcons:=[];
    BorderStyle:=bsSizeToolWin;
    Caption:=rMessages_Caption;
    FormStyle:=fsStayOnTop;
    SetBounds(Screen.Width div 2-285 div 2,
              Screen.Height div 2-150 div 2,
              285,150);
    Visible:=false;
  end;
  FImage:=TImage.Create(FForm);
  with FImage do
  begin
    Parent:=FForm;
    SetBounds(8,8,100,100);
    Stretch:=true;
    Picture.LoadFromLazarusResource('scrabble_big');
  end;
  FMemo:=TMemo.Create(FForm);
  with FMemo do
  begin
    Parent:=FForm;
    Name:='MessageDialog';
    BorderStyle:=bsNone;
    Color:=clForm;
    ReadOnly:=true;
    ScrollBars:=ssAutoVertical;
    SetBounds(116,8,150,100);
    Anchors:=[akTop,akLeft,akRight,akBottom];
  end;
  FBevel:=TBevel.Create(FForm);
  with FBevel do
  begin
    Parent:=FForm;
    SetBounds(8,116,270,2);
    Anchors:=[akLeft,akRight,akBottom];
  end;
  FCheckBox:=TCheckBox.Create(FForm);
  with FCheckBox do
  begin
    Parent:=FForm;
    SetBounds(8,123,24,22);
    Anchors:=[akLeft,akBottom];
  end;
  FButton:=TButton.Create(FForm);
  with FButton do
  begin
    Parent:=FForm;
    Caption:='Ok';
    SetBounds(200,123,75,25);
    Anchors:=[akRight,akBottom];
    OnClick:=@DoButtonClick;
    OnKeyup:=@DoButtonKeyUp;
  end;
  Config.ReadWindowPosition(FForm);

  aFontData:=GetFontData(Application.MainForm.Font.Reference.Handle);
  FStringGrid.Font.Name:=Config.Read('General/Messages/FontName',aFontData.Name);
  FStringGrid.Font.Size:=Config.Read('General/Messages/FontSize',aFontData.Height);
end;

destructor TScrabbleMessages.Destroy;
begin
  Clear;
  Config.SaveWindowPosition(FForm);
  Config.Write('General/Messages/FontName',FStringGrid.Font.Name);
  Config.Write('General/Messages/FontSize',FStringGrid.Font.Size);
  FPopupMenu.Items.Clear;//frees items
  FPopupMenu.Free;
  FForm.Free;
  FMessages.Free;
  FMessages:=nil; //FreeAndNil(FMessages);
  FStringGrid.Free;
  inherited;
end;

procedure TScrabbleMessages.DoButtonClick(Sender: TObject);
begin
  if assigned(FOnSetPopupSetting) and FCheckBox.Enabled then
    FOnSetPopupSetting(FLastMessageType,not FCheckBox.Checked);
  FForm.Hide;
end;

procedure TScrabbleMessages.DoButtonKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Key:=0;  //don't close on any key
end;

procedure TScrabbleMessages.DoChangeFont(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Font:=FStringGrid.Font;
    if Execute then
    begin
      FStringGrid.Font:=Font;
      DoResize(nil); //adjust rowheight and repaint
    end;//Execute
  finally
    Free;
  end;
end;

procedure TScrabbleMessages.DoClear(Sender: TObject);
var i:integer;
begin
  for i:=0 to FMessages.Count-1 do
    TScrabbleMessage(FMessages[i]).Free;
  FMessages.Clear;
  FStringGrid.Clean;
end;

procedure TScrabbleMessages.DoCopyAll(Sender: TObject);
var i:integer;
    s:string;
begin
  s:='';
  for i:=FMessages.Count-1 downto 0 do
   with TScrabbleMessage(FMessages[i]) do
    s:=s+TimeToStr(msgTime)+#9+msgText+#13#10;
  ClipBoard.AsText:=s;
end;

procedure TScrabbleMessages.DoDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  aMsg:TScrabbleMessage;
  aFont:TFont;
  aColor:TColor;
begin
  if FMessages.Count>aRow then
    aMsg:=TScrabbleMessage(FMessages[FMessages.Count-1-aRow]) else
    exit;
  with FStringGrid do
  begin
    Canvas.Brush.Color:=clWindow;
    Canvas.Font.Style:=[];
    Canvas.Font.Color:=clDefault;
    Canvas.FillRect(aRect);
    aRect.Top:=aRect.Top+1;
    aRect.Bottom:=aRect.Bottom-1;
    if assigned(FOnGetMessageFont) then
    begin
      aFont:=Canvas.Font;
      aColor:=clWindow;//Canvas.Brush.Color;
      FOnGetMessageFont(aMsg.msgType,aMsg.msgSender,aColor,aFont);
      Canvas.Brush.Color:=aColor;
      Canvas.Font:=aFont;
      Canvas.FillRect(aRect);
    end;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    if FMessages.Count>aRow then
    case aCol of
     0:DrawText(Canvas.Handle,PChar(TimeToStr(aMsg.msgTime)),-1,aRect, DT_WORDBREAK or DT_LEFT or DT_NOPREFIX);
     1:DrawText(Canvas.Handle,PChar(aMsg.msgText),-1,aRect, DT_WORDBREAK or DT_LEFT or DT_NOPREFIX);
    end;
  end;//with
end;

procedure TScrabbleMessages.DoGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
  Value:=TScrabbleMessage(FMessages[FMessages.Count-1-aRow]).msgText;
end;

procedure TScrabbleMessages.DoResize(Sender: TObject);
var
  i:integer;
  aRect:TRect;
  aMsg:TScrabbleMessage;
begin
  if assigned(FMessages) and (FMessages.Count>0) then
  with FStringGrid do
  begin
    BeginUpdate;
    try
      ColWidths[0]:=Canvas.TextWidth(TimeToStr(Now))+10;
      ColWidths[1]:=ClientWidth-ColWidths[0];
      for i:=0 to RowCount-1 do
      begin
        aRect:=Rect(0,0,ColWidths[1],10);
        aMsg:=TScrabbleMessage(FMessages[FMessages.Count-1-i]);
        DrawText(Canvas.Handle,PChar(aMsg.msgText),-1,aRect, DT_CALCRECT or DT_WORDBREAK or DT_LEFT or DT_NOPREFIX);
        RowHeights[i]:=aRect.Bottom+3;
      end;
      {$ifdef Darwin}
      LeftCol:=0; //darwin moves to right most cell and hides #0
      {$endif}
    finally
      EndUpdate;
    end;
  end;
end;

procedure TScrabbleMessages.SetBiDiMode(AValue: TBiDiMode);
begin
  FForm.BiDiMode:=aValue;
  FStringGrid.BiDiMode:=aValue;
end;

function TScrabbleMessages.GetFontName: string;
begin
  Result:=FStringGrid.Font.Name;
end;

function TScrabbleMessages.GetFontSize: byte;
begin
  Result:=FStringGrid.Font.Size;
end;

procedure TScrabbleMessages.SetFontName(AValue: string);
begin
  FStringGrid.Font.Name:=aValue;
end;

procedure TScrabbleMessages.SetFontSize(AValue: byte);
begin
  FStringGrid.Font.Size:=aValue;
end;

procedure TScrabbleMessages.Clear;
begin
  DoClear(nil);
end;

procedure TScrabbleMessages.Repaint;
begin
  FStringGrid.Repaint;
end;

procedure TScrabbleMessages.UpdateLanguage;
begin
  FForm.Caption:=rMessages_Caption;
  FPopupMenu.Items[0].Caption:=rMessages_Clear;
  FPopupMenu.Items[1].Caption:=rMessages_ChangeFont;
  FPopupMenu.Items[2].Caption:=rMessages_CopyAll
end;

procedure TScrabbleMessages.AddMessage(aMsgType:TScrabbleMessageType; aMsg: string; aSender:string);
var
  aMessage:TScrabbleMessage;
begin
  if (aMsgType<>smDebug) or FShowDebug then
  begin
    aMessage:=TScrabbleMessage.Create;
    aMessage.msgType:=aMsgType;
    aMessage.msgTime:=Now;
    aMessage.msgSender:=aSender;
    aMessage.msgText:=aMsg;
    FMessages.Add(aMessage);
    //reduce loading time
    if not (gsLoading in Scrabble.GameState) then
    begin
      FStringGrid.RowCount:=FMessages.Count;
      DoResize(self);
      FStringGrid.Repaint;
    end;

    if aMsgType<>FLastMessageType then
      FCheckBox.Enabled:=false;
    if FForm.Visible then
      FMemo.Lines.Insert(0,aMsg) else
    if assigned(FOnGetPopupSetting) then
    begin
      FCheckBox.Enabled:=true;//aMsgType<>smError;
      case aMsgType of
       smError:FCheckBox.Caption:=rMessage_Error;
       smWarning:FCheckBox.Caption:=rMessage_Warning;
       smInformation:FCheckBox.Caption:=rMessage_Information;
       smChat:FCheckBox.Caption:=rMessage_Chat;
       smOwnMove:FCheckBox.Caption:=rMessage_OwnMove;
       smGameResult:FCheckBox.Caption:=rMessage_GameResult;
       smDebug:FCheckBox.Caption:=rMessage_Debug;
      end;
      FMemo.Lines.Clear;
      FMemo.Lines.Add(aMsg);
      if FOnGetPopupSetting(aMsgType) then
      begin
        FForm.Show;
        FForm.BringToFront;
      end;
    end;
    FLastMessageType:=aMsgType;
  end;
end;

initialization

{$I messages.res}

end.
