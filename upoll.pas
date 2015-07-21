{ Feedback dialog for polls

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

unit upoll;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  Dialogs, LCLIntf, LCLType, LResources;

const
  mrTimeout = mrLast+1;

type

  { TPoll }
   TPollContent=(pcNone=-1,pcPrivate=0,pcPublic=1,pcResume=2,pcNotFound=3,pcPause=4,pcUnPause=5);

  TPollState=(plYes=0,plNo=1,plWait=2);
  TPoll=class(TForm)
    private
      FBevel    : TBevel;
      FCheckBox : TCheckBox;
      FButton   : TButton;
      FAnswersPie : TBitmap;
      FLabel    : array[0..3] of TLabel;
      FImage    : array[0..3] of TImage;
      FPollState : array[0..3] of TPollState;
      FPlayerCount : byte;
      procedure DoButtonClick(Sender: TObject);
      function GetAnswerPie: TBitmap;
      procedure SetAnswer(aIndex: string; aValue: boolean);
      procedure DoClose(Sender: TObject; var CloseAction: TCloseAction);reintroduce;
    public
      constructor Create; reintroduce;
      destructor Destroy; override;
      procedure Init(const aMates:TStrings);
      function WaitFor(const aVisible:boolean):boolean;
      property Answer[aIndex: string]:boolean write SetAnswer;
      property AnswerPie:TBitmap read GetAnswerPie;
    end;

    function TimeoutMsg(const aMessage:string; const aTimeOut:word=30):TModalResult;

var
  Poll : TPoll;

implementation

uses
  ulanguage, uconfig;

{ TTimeOutDialog }
var
  aDialog1,aDialog2:TForm;

function TimeoutMsg(const aMessage: string; const aTimeOut: word): TModalResult;
var aStart,aPast   : longword;
    aBitmap        : TCustomBitmap;
    x              : integer;
    pDialog        : ^TForm;
begin
  //close old dialog if a new one is opened
  if assigned(aDialog1) then
  begin
    aDialog1.ModalResult:=mrCancel;
    aDialog1.Hide;
    aDialog1:=nil;
    pDialog:=addr(aDialog2);
  end else
  begin
    if assigned(aDialog2) then
    begin
      aDialog2.ModalResult:=mrCancel;
      aDialog2.Hide;
      aDialog2:=nil;
    end;
    pDialog:=addr(aDialog1);
  end;
  if aTimeOut=0 then
    exit; //just cancel dlg

  pDialog^:=CreateMessageDialog(aMessage,mtConfirmation,[mbYes,mbNo]);
  with pDialog^ do
  try
    FormStyle:=fsStayOnTop;
    Position:=poMainFormCenter;
    Show;//not modal
    aStart:=GetTickCount;
    aBitmap:=GetDialogIcon(idDialogConfirm);
    aBitmap.Canvas.Brush.Style:=bsSolid;
    aBitmap.Canvas.Pen.Style:=psClear;
    aBitmap.Canvas.Brush.Color:=clBtnFace;
    x:=0;
    repeat
      aPast:=(GetTickCount-aStart) div 1000;
      if (aPast>0) and (aPast{%H-}<>x) then
      begin
        Canvas.Brush.Color:=clGreen;
        Canvas.FillRect(Bounds(0,0,round(Width{%H-}*aPast/aTimeOut),2));
        if aTimeOut-aPast<10 then
          Caption:=Language.Plural(rPoll_TimeOut,aTimeOut-aPast);
        x:=aPast;
      end;
      Enabled:=ModalResult<>mrCancel;
      try
        sleep(1);
        Application.ProcessMessages;
      except
        raise;
      end;
      if Application.Terminated then
        ModalResult:=mrCancel;
      if ModalResult<>mrNone then
        break;
      if (aPast>=aTimeOut) then
        ModalResult:=mrTimeout;
    until (ModalResult<>mrNone) or (Visible=false);
    Result:=ModalResult;
  finally
    aBitmap.Free;
    Free;
    pDialog^:=nil;
  end;
end;

{ TPoll }

procedure TPoll.DoClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult=mrNone then
    ModalResult:=mrNo;
  CloseAction:=caHide;
end;

procedure TPoll.DoButtonClick(Sender: TObject);
begin
  ModalResult:=FButton.ModalResult;
end;

function TPoll.GetAnswerPie: TBitmap;
const Size=64;
      ArcPos:array[2..4,0..3,0..3] of Byte=(((2,4,2,0),(2,0,2,4),(0,0,0,0),(0,0,0,0)),
                                            ((4,3,2,0),(2,0,0,3),(0,3,4,3),(0,0,0,0)),
                                            ((4,0,0,0),(0,4,4,4),(4,4,4,0),(0,0,0,4)));
var i:integer;
begin
  FAnswersPie.Width:=Size;
  FAnswersPie.Height:=Size;
  with FAnswersPie do
  begin
    Canvas.Brush.Color:=clBtnFace;
    Canvas.Brush.Style:=bsSolid;
    Canvas.FillRect(Bounds(0,0,Size,Size));
    Canvas.Pen.Color:=clBlack;
    if FPlayerCount>1 then
     for i:=0 to FPlayerCount-1 do
     begin
       case FPollState[i] of
        plYes  : Canvas.Brush.Color:=clGreen;
        plNo   : Canvas.Brush.Color:=clRed;
        plWait : Canvas.Brush.Color:=clBtnFace;
       end;
       Canvas.Pie(0,0,Size,Size,
                  ArcPos[FPlayerCount, i, 0]*(Size div 4),
                  ArcPos[FPlayerCount, i, 1]*(Size div 4),
                  ArcPos[FPlayerCount, i, 2]*(Size div 4),
                  ArcPos[FPlayerCount, i, 3]*(Size div 4));
     end;//for
  end;//with
  Result:=FAnswersPie;
end;

procedure TPoll.SetAnswer(aIndex: string; aValue: boolean);
const ResNames:array[TPollState] of string=('plYes','plNo','plWait');
var i:integer;
    aPollResult:TModalResult;
begin
  for i:=0 to 3 do
   if FLabel[i].Caption=aIndex then
   begin
     case aValue of
      true  : FPollState[i]:=plYes;
      false : FPollState[i]:=plNo;
     end;
     FImage[i].Picture.LoadFromLazarusResource(ResNames[FPollState[i]]);
   end;
  aPollResult:=mrOk;
  for i:=0 to FPlayerCount-1 do
   case FPollState[i] of
    plWait : aPollResult:=mrNone;
    plNo   : if aPollResult<>mrNone then aPollResult:=mrNo;
    plYes  : ;
   end;//case
  if aPollResult<>mrNone then
  begin
    FButton.Enabled:=true;
    FButton.ModalResult:=aPollResult;
    if not Visible or (FCheckBox.Checked) then
      ModalResult:=aPollResult;
  end;
end;

constructor TPoll.Create;
var i:integer;
begin
  inherited CreateNew(Application);
  Parent:=nil;
  Caption:=rPoll_Caption;
  BorderIcons:=[];
  BorderStyle:=bsToolWindow;
  FormStyle:=fsStayOnTop;
  Position:=poMainFormCenter;
  Width:=165;
  Height:=170;
  OnClose:=@DoClose;
  FBevel:=TBevel.Create(self);
  with FBevel do
  begin
    Parent:=self;
    SetBounds(8,108,149,2);
  end;
  FCheckBox:=TCheckBox.Create(self);
  with FCheckBox do
  begin
    Parent:=self;
    SetBounds(8,112,150,22);
    Caption:=rPoll_CheckBox;
    Checked:=Config.Read('General/Poll/AutoClose',true);
  end;
  FButton:=TButton.Create(self);
  with FButton do
  begin
    Parent:=self;
    SetBounds(80,136,75,25);
    Caption:='&Ok';
    ModalResult:=mrOk;
    Enabled:=false;
    OnClick:=@DoButtonClick;
  end;
  for i:=0 to 3 do
  begin
    FLabel[i]:=TLabel.Create(self);
    with FLabel[i] do
    begin
      Parent:=self;
      Left:=8;
      Top:=8+i*25;
    end;
    FImage[i]:=TImage.Create(self);
    with FImage[i] do
    begin
      Parent:=self;
      Left:=135;
      Top:=6+i*24;
      Transparent:=true;
    end;
  end;
  FAnswersPie:=TBitmap.Create;
end;

destructor TPoll.Destroy;
var i:integer;
begin
  for i:=0 to 3 do
  begin
    FImage[i].Free;
    FLabel[i].Free;
  end;
  FButton.Free;
  Config.Write('General/Poll/AutoClose',FCheckBox.Checked);
  FCheckBox.Free;
  FBevel.Free;
  FAnswersPie.Free;
  inherited;
end;

procedure TPoll.Init(const aMates: TStrings);
var i:integer;
begin
  for i:=0 to 3 do
  begin
    FPollState[i]:=plWait;
    with FLabel[i] do
    begin
      if aMates.Count>i then
      begin
        Caption:=aMates[i];
        Enabled:=true;
      end else
      begin
        case i of
         0:Caption:=rPoll_Player1;
         1:Caption:=rPoll_Player2;
         2:Caption:=rPoll_Player3;
         3:Caption:=rPoll_Player4;
        end;
        Enabled:=false;
      end;
    end;
    with FImage[i] do
    begin
      if i<aMates.Count then
        Picture.LoadFromLazarusResource('plWait') else
        Picture.Clear;
    end;
  end;
  FPlayerCount:=aMates.Count;
  ModalResult:=mrNone;
  FButton.Enabled:=false;
end;

function TPoll.WaitFor(const aVisible:boolean):boolean;
begin
  if aVisible and (FPlayerCount>1) then
    Show;
  try
    repeat
      try
        Application.ProcessMessages;
      except
        if Application.CaptureExceptions then
          Application.HandleException(Self)
        else
          raise;
      end;
      if Application.Terminated then
        ModalResult:=mrCancel;
      if ModalResult<>mrNone then
        break;
      Application.Idle(true);
    until False;
    Result:=(ModalResult=mrOK);
  finally
    Hide;
  end;
end;

initialization
  {$I poll.res}
end.

