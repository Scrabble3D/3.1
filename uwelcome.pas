{ Welcome assistant

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

unit uwelcome;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Themes, LCLType, LCLIntf,
  ulanguage, ugameoptions, udefaults, types;

type

  { TfmWelcome }

  TfmWelcome = class(TForm)
    btnWelcomeNext: TButton;
    btnWelcomeBack: TButton;
    btnWelcomeCancel: TButton;
    imScrabble3D: TImage;
    imWelcome: TImage;
    lbStep: TLabel;
    lbWelcome: TLabel;
    lbSelection: TListBox;
    pnStep: TPanel;
    pbWelcome: TProgressBar;
    shWelcome: TShape;
    procedure btnWelcomeNextClick(Sender: TObject);
    procedure btnWelcomeBackClick(Sender: TObject);
    procedure btnWelcomeNextKeyPress(Sender: TObject; var Key: char);
    procedure DoActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbSelectionDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lbSelectionSelectionChange(Sender: TObject; User: boolean);
  private
    procedure Apply;
    procedure UpdateStep;
  public
    { public declarations }
  end;

var
  fmWelcome: TfmWelcome;

implementation

{ TfmWelcome }

procedure TfmWelcome.btnWelcomeNextClick(Sender: TObject);
begin
  if pnStep.Tag=3 then
  begin
    fmGameOptions.cbPresets.Text:='Standard';
    fmGameOptions.acSavePreset.Execute;
    ModalResult:=mrOk;
  end else
  begin
    Apply;
    pnStep.Tag:=pnStep.Tag+1;
    lbSelection.ItemIndex:=-1;
    UpdateStep;
  end;
end;

procedure TfmWelcome.btnWelcomeBackClick(Sender: TObject);
begin
  btnWelcomeNext.Caption:=rWelcome_Next1;
  pnStep.Tag:=pnStep.Tag-1;
  lbSelection.ItemIndex:=-1;
  UpdateStep;
end;

procedure TfmWelcome.btnWelcomeNextKeyPress(Sender: TObject; var Key: char);
begin
  if Key=chr(VK_ESCAPE) then
    ModalResult:=mrCancel;
end;

procedure TfmWelcome.DoActivate(Sender: TObject);
begin
  lbSelection.Selected[0]:=false;
  btnWelcomeNext.Enabled:=false;
  fmWelcome.OnActivate:=nil;
end;

procedure TfmWelcome.FormShow(Sender: TObject);
begin
  pnStep.Tag:=0;
  UpdateStep;
  fmWelcome.OnActivate:=@DoActivate;
end;

procedure TfmWelcome.lbSelectionDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  s:string;
  y:integer;
  png:TPortableNetworkGraphic;
begin
  with Control as TListBox do
  begin
    if odSelected in State then
       Canvas.Brush.Color:=clHighlight else
       Canvas.Brush.Color:=clDefault;
    Canvas.FillRect(aRect);
    aRect.Left:=aRect.Left+2;
    //paint icon
    y:={$ifndef LCLGtk}aRect.Top+{$endif}(aRect.Bottom-aRect.Top) div 2;
    s:=Defaults[lbSelection.Items[Index]].FlagID;
    if (s<>'') and (LazarusResources.Find(s)<>nil) then
    begin
      png:=TPortableNetworkGraphic.Create;
      try
        png.LoadFromLazarusResource(s);
        Canvas.Draw(aRect.Left,y-png.Height div 2,png);
        aRect.Left:=aRect.Left+png.Width+2;
      finally
        png.Free;
      end;
    end;
    //draw text
    DrawText(Canvas.Handle, PChar(lbSelection.Items[Index]), Length(lbSelection.Items[Index]), aRect, DT_SINGLELINE or DT_VCENTER);
  end;
end;

procedure TfmWelcome.lbSelectionSelectionChange(Sender: TObject; User: boolean);
begin
  btnWelcomeNext.Enabled:=(lbSelection.SelCount>0) or (pnStep.Tag=3);
{  if btnWelcomeNext.Enabled then
  begin
    lbStep.Color:=clNone;
    lbStep.Font.Color:=clDefault;
  end else
  begin
    lbStep.Color:=clHighlight;
    lbStep.Font.Color:=clHighlightText;
  end;  }
end;

procedure TfmWelcome.Apply;
 function SelectListViewItem(aListView:TListView; aCaption: string): boolean;
 var
   i:integer;
 begin
   aListView.Selected:=nil;
   for i:=0 to aListView.Items.Count-1 do
     if aListView.Items[i].Caption=aCaption then
      aListView.Selected:=aListView.Items[i];//.Selected:=true
   Result:=aListView.Selected<>nil;
 end;
begin
  btnWelcomeNext.Enabled:=false;
  btnWelcomeBack.Enabled:=false;
  btnWelcomeCancel.Enabled:=false;
  lbSelection.Enabled:=false;
  with Defaults[lbSelection.Items[lbSelection.ItemIndex]] do
  try
    if lbSelection.ItemIndex>-1 then
    case pnStep.Tag of
     0: if SelectListViewItem(fmGameOptions.lvLocalizations,English) then
          fmGameOptions.btnLocalizationClick(nil);
     1: if SelectListViewItem(fmGameOptions.lvDictionaries,English) then
          fmGameOptions.acLoadDictionary.Execute;
     2: begin
          with fmGameOptions.cbRulesSet do
            ItemIndex:=Items.IndexOf(English+' ('+Native+')');
          fmGameOptions.cbRulesSetChange(self);
          //easy mode
          fmGameOptions.rbWCMTakeBack.Checked:=true;
          fmGameOptions.rbNoLimit.Checked:=true;
        end;
   end;
  finally
    btnWelcomeCancel.Enabled:=true;
    lbSelection.Enabled:=true;
    btnWelcomeNext.Enabled:=true;
    UpdateStep;  //enable
  end;
end;

procedure TfmWelcome.UpdateStep;
  procedure FillListBox;
  var
    lang:TDefaultsLanguage;
  begin
    lbSelection.Items.Clear;
    for lang:=low(TDefaultsLanguage) to high(TDefaultsLanguage) do
    with Defaults.Value[lang] do
    begin
      case pnStep.Tag of
       0: if LangFile<>'' then
            lbSelection.Items.Add(English+' ('+Native+')');
       1: if DicFile<>'' then
            lbSelection.Items.Add(English+' ('+Native+')');
       2: //if Defaults.Value[lang].LangFile<>'' then
            lbSelection.Items.Add(English+' ('+Native+')');
      end;
    end;
  end;
begin
  FillListBox;
  lbSelection.Visible:=true;//set false in step 3
  btnWelcomeNext.Enabled:=false;
  case pnStep.Tag of
   0 : begin
         lbStep.Caption:=rWelcome_Step1;
         pbWelcome.Position:=0;
         btnWelcomeNext.Caption:=rWelcome_Next1;
       end;
   1 : begin
         lbStep.Caption:=rWelcome_Step2;
         pbWelcome.Position:=33;
       end;
   2 : begin
         lbStep.Caption:=rWelcome_Step3;
         pbWelcome.Position:=66;
       end;
   3 : begin
         lbSelection.Visible:=false;
         pbWelcome.Position:=100;
         btnWelcomeNext.Enabled:=true;
         btnWelcomeNext.Caption:=rWelcome_Next2;
         lbStep.Caption:=rWelcome_Step4;
       end;
  end;
  lbSelectionSelectionChange(self,false);
  btnWelcomeBack.Enabled:=pnStep.Tag<>0;
end;

initialization
  {$I uwelcome.lrs}

end.


