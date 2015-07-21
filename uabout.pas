{ about dialog

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


unit uabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, LCLType, Forms, Controls, Graphics, Dialogs,
  LCLIntf, ExtCtrls, StdCtrls, ComCtrls, Buttons;

type

  { TfmAbout }

  TfmAbout = class(TForm)
    imAbout: TImage;
    imGittip: TImage;
    imDonateBitcoin: TImage;
    lbLicenceLink: TLabel;
    lbContact: TLabel;
    lbTitle: TLabel;
    lbVersion: TLabel;
    mmLicence: TMemo;
    mmContrib: TMemo;
    pcAbout: TPageControl;
    tsLicence: TTabSheet;
    tsContrib: TTabSheet;
    tsGeneral: TTabSheet;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure imDonateBitcoinClick(Sender: TObject);
    procedure imGittipClick(Sender: TObject);
    procedure lbContactClick(Sender: TObject);
    procedure lbLicenceLinkClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmAbout: TfmAbout;

implementation

uses
  uconfig, utypes, uversion;

{ TfmAbout }

procedure TfmAbout.FormShow(Sender: TObject);
begin
  lbVersion.Caption:='version: '+cVersion+LineBreak+
                     'build date: '+cBuild+LineBreak+
                     '(c) GNU General Public License, v3'+LineBreak+
                     'author: Heiko Tietze';
  lbContact.Caption:=Config.Read('Network/Links/Contact','heiko_tietze@web.de');
  lbLicenceLink.Caption:='GNU General Public License, v3';
end;

procedure TfmAbout.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key=chr(VK_ESCAPE) then
    Close;
end;

procedure TfmAbout.imDonateBitcoinClick(Sender: TObject);
begin
  OpenUrl('bitcoin:1E9JCzZda61uajHCo9hvF8ZUmexQiUecGA');
end;

procedure TfmAbout.imGittipClick(Sender: TObject);
begin
  OpenUrl('https://www.gittip.com/Scrabble3D/');
end;

procedure TfmAbout.lbContactClick(Sender: TObject);
begin
  OpenURL('mailto:'+Config.Read('Network/Links/Contact','heiko_tietze@web.de')+'?subject=Scrabble3D');
end;

procedure TfmAbout.lbLicenceLinkClick(Sender: TObject);
begin
  OpenURL('http://www.gnu.org/licenses/gpl.html');
end;

initialization
  {$I uabout.lrs}

end.

