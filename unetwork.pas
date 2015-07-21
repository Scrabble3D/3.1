{ Network dialog

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

unit unetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, LCLIntf, LCLType, Themes,
  utcptypes, ulanguage, udefaults, types;

type

  { TfmNetwork }

  TfmNetwork = class(TForm)
    btnNetworkCancel: TBitBtn;
    btnNetworkOk: TBitBtn;
    cbWanServer: TComboBox;
    cbCountry: TComboBox;
    edEmail: TEdit;
    edCity: TEdit;
    edLoginLAN: TEdit;
    edLoginWAN: TEdit;
    edNetworkPort: TEdit;
    edPassword: TEdit;
    edServerAddr: TEdit;
    lbCity: TLabel;
    lbCountry: TLabel;
    lbEmail: TLabel;
    lbLoginLAN: TLabel;
    lbLoginWAN: TLabel;
    lbPassword: TLabel;
    lbWanServer: TLabel;
    lbNetworkPort: TLabel;
    pcConnection: TPageControl;
    rbClient: TRadioButton;
    rbServer: TRadioButton;
    tsLAN: TTabSheet;
    tsWAN: TTabSheet;
    procedure cbCountryDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pcConnectionChange(Sender: TObject);
    procedure rbClientServerChange(Sender: TObject);
  private
    function GetIsGameServer: boolean;
  public
    property IsGameServer:boolean read GetIsGameServer;
  end;

var
  fmNetwork: TfmNetwork;

implementation

uses
  uconfig;

{ TfmNetwork }

procedure TfmNetwork.pcConnectionChange(Sender: TObject);
begin
  edPassword.Enabled:=IsGameServer;
end;

procedure TfmNetwork.rbClientServerChange(Sender: TObject);
begin
  edServerAddr.Enabled:=rbClient.Checked;
end;

function TfmNetwork.GetIsGameServer: boolean;
begin
  Result:=pcConnection.ActivePage=tsWan;
end;

procedure TfmNetwork.FormShow(Sender: TObject);
var
  i:integer;
begin
  cbWanServer.Items.Clear;
  for i:=0 to Config.Read('Network/Servers/Count',0)-1 do
    cbWanServer.Items.Add(Config.Read('Network/Servers/f'+inttostr(i)+'/Name',''));
  edLoginLan.Text:=Config.Read('Network/LoginName/LAN','');
  edLoginWan.Text:=Config.Read('Network/LoginName/WAN','');
  edPassword.Text:=Config.Read('Network/Password','');
  edNetworkPort.Text:=Config.Read('Network/Port',cPort);
  edServerAddr.Text:=Config.Read('Network/ServerAddr','');
  cbCountry.Text:=Config.Read('Network/Country','');
  edCity.Text:=Config.Read('Network/City','');
  edEmail.Text:=Config.Read('Network/Email','');
  cbWanServer.ItemIndex:=Config.Read('Network/LastServer',0);
end;

procedure TfmNetwork.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult=mrOk) then
  begin
    if (IsGameServer and (edLoginWAN.Text='')) or
       (not IsGameServer and (edLoginLAN.Text='')) then
    begin
      ShowMessage(rNetwork_ErrorName);
      CanClose:=false;
    end else
    if IsGameServer and (edPassword.Text='') then
    begin
      ShowMessage(rNetwork_ErrorPassword);
      CanClose:=false;
    end else
    if IsGameServer and (cbWanServer.Text='') then
    begin
      ShowMessage(rNetwork_ErrorServer);
      CanClose:=false;
    end else
      CanClose:=true;
  end else
    CanClose:=true;

  if CanClose then
  begin
    Config.Write('Network/LoginName/LAN',edLoginLAN.Text);
    Config.Write('Network/LoginName/WAN',edLoginWAN.Text);
    Config.Write('Network/Password',edPassword.Text);
    Config.Write('Network/Port',edNetworkPort.Text);
    Config.Write('Network/ServerAddr',edServerAddr.Text);
    Config.Write('Network/Country',cbCountry.Text);
    Config.Write('Network/City',edCity.Text);
    Config.Write('Network/Email',edEmail.Text);
    Config.Write('Network/LastServer',cbWanServer.ItemIndex);
    Config.SaveWindowPosition(fmNetwork);
  end;

end;

procedure TfmNetwork.cbCountryDrawItem(Control: TWinControl; Index: Integer; aRect: TRect; State: TOwnerDrawState);
var
  aPng:TPortableNetworkGraphic;
  y:integer;
begin
  aPng:=TPortableNetworkGraphic.Create;
  try
    if LoadFlagFromResource(cbCountry.Items[Index],aPng) then
    with (Control as TComboBox) do
    begin
      //erase background
      {$IFDEF _Linux}
      if ThemeServices.ThemesEnabled then
        ThemeServices.DrawElement(Canvas.Handle,ThemeServices.GetElementDetails(tcComboBoxDontCare), aRect, nil) else
      {$ENDIF}
      begin
        if odSelected in State then
          Canvas.Brush.Color:=clHighlight else
          Canvas.Brush.Color:=clDefault;//BtnFace;
        Canvas.FillRect(aRect);
      end;

      if DroppedDown then
        y:={$ifndef LCLGtk}aRect.Top+{$endif}(aRect.Bottom-aRect.Top) div 2 else
        y:=Height div 2;
      if BidiMode=bdLeftToRight then
      begin
        Canvas.Draw(aRect.Left+2,y-aPng.Height div 2,aPng);
        aRect.Left:=aRect.Left+aPng.Width+4;
      end else
      begin
        Canvas.Draw(aRect.Right-aPng.Width-2,y-aPng.Height div 2,aPng);
        aRect.Right:=aRect.Right-aPng.Width-4;
      end;
    end;
    with (Control as TComboBox) do
     if BiDiMode=bdLeftToRight then
      DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), aRect, DT_SINGLELINE or DT_VCENTER) else
      DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), aRect, DT_SINGLELINE or DT_RIGHT);
  finally
    aPng.Free;
  end;
end;

procedure TfmNetwork.FormCreate(Sender: TObject);
begin
  Config.ReadWindowPosition(self);
end;

procedure TfmNetwork.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key=chr(VK_ESCAPE) then
    ModalResult:=mrCancel else
  if Key=chr(VK_RETURN) then
    ModalResult:=mrOk;
end;

initialization
  {$I unetwork.lrs}

end.

