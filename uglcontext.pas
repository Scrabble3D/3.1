{ Basic OpenGL routines

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

unit uglcontext;

{$mode objfpc}
{$H+}

interface

{$I conditions.inc}

uses
  Classes, SysUtils, Controls, Graphics,
  LResources, //LazarusResources @ CreateTexture
  IntfGraphics, LCLType, //TLazIntfImage @ bmp2raw, Screenshot
  {$ifdef Debug}
  LCLProc, //debugln
  {$endif}
  FPImage, //TFPColor @ BmpToRaw
  {$ifdef UseOpenGl}
  Math, //Power | Mousewheel
  OpenGLContext, GL, glu, glext
  {$endif}
  ;

type

  { TGLContext }

  TPosition=record
    X,Y,Z:single;
  end;

  TTransformations=set of (rx,ry,rz,tx,ty,tz);

  TLight=record
    Ambient, Diffuse, Specular : byte; //%; 0..100
    Position : array[0..2] of shortint;//absolute
  end;

  T2DByteArray=array of array of byte;

  TGLContext=class{$ifdef UseOpenGl}(TOpenGLControl){$endif}
    private
      {$ifdef UseOpenGl}
      FTransformations : TTransformations;
      FInitialized     : boolean;
      FLastPos         : TPosition;
      FFrameCount      : integer;
      FLastFrameTicks  : integer;
      FFps             : integer;
      FCanUseMipmaps   : boolean;
      procedure Initialize;
      procedure DoResize(Sender: TObject);
      procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure DoPaint(Sender: TObject);
      function GetFps: integer;
      {$endif}
    private
      FOnInitialize    : TNotifyEvent;
      FPaintObjects    : TNotifyEvent;
      FBkColor         : TColor;
      FLimitRotationX  : integer; //only x yet; -1 no limit
      FPerspectiveByMouse : boolean;
      FTranslation     : TPosition;
      FRotation        : TPosition;
      FLight           : TLight;
      procedure SetLight(AValue: TLight);
      procedure SetBkColor(AValue: TColor);
    public
      {$ifdef UseOpenGl}
      procedure ConvertColor(aColor: TColor; var r, g, b: GLFloat);
      function PickID(const x,y:word; Render:TNotifyEvent):integer;
      procedure {%H-}DoMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);virtual;
      procedure BlendFromIndex(Value1, Value2: integer);
      property Fps:integer read GetFps;
      function ScreenShot:TBitmap;
      property CanUseMipmaps: boolean read FCanUseMipmaps;
      {$endif}
    public
      constructor Create(aOwner: TComponent; aTransformations:TTransformations); reintroduce;
      destructor Destroy;override;

      property Light:TLight read FLight write SetLight;
      property Rotation:TPosition read FRotation write FRotation;
      property LimitRotation:integer read FLimitRotationX write FLimitRotationX;
      property Translation:TPosition read FTranslation write FTranslation;
      property BkColor:TColor read FBkColor write SetBkColor;
      property OnPaintObjects:TNotifyEvent read FPaintObjects write FPaintObjects;
      property OnInitialize:TNotifyEvent read FOnInitialize write FOnInitialize;
      property PerspectiveByMouse:boolean read FPerspectiveByMouse write FPerspectiveByMouse;
    end;//TGLContext

const
  cFovy=25.0;

implementation

{ TGLContext }

{$ifdef UseOpenGl}
procedure TGLContext.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  z:single;
begin
  if FPerspectiveByMouse then
  begin
    if Shift=[ssLeft] then
    begin
      if rx in FTransformations then
      begin
        z:=FRotation.X-(FLastPos.Y-y);
        if z<0 then z:=360-z;
        if (FLimitRotationX=-1) or (FLimitRotationX>z) then
           FRotation.X:=z;
      end;
      if ry in FTransformations then
        FRotation.Y:=FRotation.Y-(FLastPos.X-x);
      Invalidate;
    end; //Shift=[ssLeft]

    if Shift=[ssLeft,ssAlt] then
    begin
      FTranslation.X:=FTranslation.X-(FLastPos.X-x);
      FTranslation.Y:=FTranslation.Y-(y-FLastPos.Y);
      Cursor:=crHandPoint;
      Invalidate;
    end else //Shift=[ssLeft,ssCtrl]
      Cursor:=crDefault;
  end;
  FLastPos.X:=x;
  FLastPos.Y:=y;
end;

function TGLContext.PickID(const x, y: word; Render: TNotifyEvent): integer;
const
  BufSize=64;
var
  aBuffer    : array[0..BufSize-1] of GLUInt;
  aViewport  : array[0..3] of GlInt;
  aHits,i    : Integer;
  z          : GLUInt;
begin
  if MakeCurrent then
  begin
    glGetIntegerv(GL_VIEWPORT, aViewport);
    glSelectBuffer(BufSize, aBuffer);
    glMatrixMode(GL_PROJECTION);
    glRenderMode(GL_SELECT);

    glPushMatrix;
    glLoadIdentity;
    gluPickMatrix(x, aViewport[3]-y, 1.0, 1.0, aViewport);
    gluPerspective(cFovy, aViewport[2]/aViewport[3], 1.0, 1000.0);

    glMatrixMode(GL_MODELVIEW);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glLoadIdentity;

    glTranslatef(FTranslation.X/10, FTranslation.Y/10, FTranslation.Z);
    glRotatef(FRotation.X,1.0,0.0,0.0);
    glRotatef(FRotation.Y,0.0,1.0,0.0);
    glRotatef(FRotation.Z,0.0,0.0,1.0);

    glInitNames;
    glPushName(0);
    Render(nil);
    glPopName();

    glMatrixMode(GL_PROJECTION);
    glPopMatrix;

    Result:=-1;
    aHits:=glRenderMode(GL_RENDER);
    z:=High(GLUInt);
    for i:=0 to aHits-1 do
     if aBuffer[(i*4)+1]<z then
      begin
        Result:=aBuffer[(i*4)+3];
        z:=aBuffer[(i*4)+1];
      end;
  end;
end;

procedure TGLContext.DoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  z:single;
begin
  if (tz in FTransformations) and FPerspectiveByMouse then
  begin
    z:=WheelDelta/120;
    FTranslation.Z:=FTranslation.Z*Power(1.1,z);
    Invalidate;
    Handled:=true;
  end;
end;

procedure TGLContext.BlendFromIndex(Value1, Value2: integer); //just for testing
var
  a,b:GLenum;
begin
  case Value1 of
    0: a:=GL_ZERO;
    1: a:=GL_ONE;
    2: a:=GL_SRC_COLOR;
    3: a:=GL_ONE_MINUS_SRC_COLOR;
    4: a:=GL_SRC_ALPHA;
    5: a:=GL_ONE_MINUS_SRC_ALPHA;
    6: a:=GL_DST_ALPHA;
    7: a:=GL_ONE_MINUS_DST_ALPHA;
    8: a:=GL_DST_COLOR;
    9: a:=GL_ONE_MINUS_DST_COLOR;
   10: a:=GL_SRC_ALPHA_SATURATE;
  end;
  case Value2 of
    0: b:=GL_ZERO;
    1: b:=GL_ONE;
    2: b:=GL_SRC_COLOR;
    3: b:=GL_ONE_MINUS_SRC_COLOR;
    4: b:=GL_SRC_ALPHA;
    5: b:=GL_ONE_MINUS_SRC_ALPHA;
    6: b:=GL_DST_ALPHA;
    7: b:=GL_ONE_MINUS_DST_ALPHA;
    8: b:=GL_DST_COLOR;
    9: b:=GL_ONE_MINUS_DST_COLOR;
   10: b:=GL_SRC_ALPHA_SATURATE;
  end;
  glEnable(GL_BLEND);
  glBlendFunc(a, b);
end;

procedure TGLContext.SetBkColor(AValue: TColor);
var
  r,g,b:single;
begin
  //glClearColor does not work until intialization is done
  //therefore setbkcolor is called again from init with stored FBkColor value
  if MakeCurrent then
  begin
    FBkColor:=aValue;
    ConvertColor(FBkColor,r,g,b);
    glClearColor(r,g,b,1.0);    // sets background color
    Invalidate;
  end;
end;

procedure TGLContext.SetLight(AValue: TLight);
var
  aGlLight:array[0..3] of GLfloat;
begin
  FLight:=aValue;

  if MakeCurrent then
  begin
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    aGlLight[0]:=FLight.Ambient/100; aGlLight[1]:=aGlLight[0]; aGlLight[2]:=aGlLight[0]; aGlLight[3]:=1;
    glLightfv(GL_LIGHT0, GL_AMBIENT, @aGlLight);
    aGlLight[0]:=FLight.Diffuse/100; aGlLight[1]:=aGlLight[0]; aGlLight[2]:=aGlLight[0]; aGlLight[3]:=1;
    glLightfv(GL_LIGHT0, GL_DIFFUSE, @aGlLight);
    aGlLight[0]:=FLight.Specular/100; aGlLight[1]:=aGlLight[0]; aGlLight[2]:=aGlLight[0]; aGlLight[3]:=1;
    glLightfv(GL_LIGHT0, GL_SPECULAR, @aGlLight);
    aGlLight[0]:=FLight.Position[0];aGlLight[1]:=FLight.Position[1];aGlLight[2]:=FLight.Position[2];aGlLight[3]:=1;
    glLightfv(GL_LIGHT0, GL_POSITION, @aGlLight);

  //  glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, @FLight.Ambient);

    Invalidate;
  end;
end;

procedure TGLContext.Initialize;
const
  cHint=GL_NICEST;//GL_FASTEST;
  cLight:TLight=(Ambient:(50);
                 Diffuse:(70);
                 Specular:(0);
                 Position:(10,-10,30));
{$ifndef darwin}
const
  GL_MAX_SAMPLES_EXT = $8D57;
var
  FGLsamples: integer;
{$endif}
begin
  if MakeCurrent then
  begin
    //load extensions; mipmapping is done in utextures; extensions must not be loaded in oncreate at darwin
    FCanUseMipmaps:=Load_GL_VERSION_3_0;
    {$ifdef Debug}
    debugln('CanUseMipmaps: ',BoolToStr(FCanUseMipmaps,true));
    {$endif}
    {$ifndef darwin} //darwin does not allow changing context in onpaint, which calls initialize
      //best multisampling
      glGetIntegerv(GL_MAX_SAMPLES_EXT, @FGLsamples);
      {$ifdef Debug}
      debugln('GL_MAX_SAMPLES_EXT: ',inttostr(FGLsamples));
      {$endif}
      if (FGLsamples>0) and (FGLsamples<=16) then
        MultiSampling:=FGLsamples;
    {$endif}

    //bk color
    SetBkColor(FBkColor);
    //light
    Light:=cLight;
    //smoothing
    glEnable(GL_LINE_SMOOTH);
    glHint(GL_LINE_SMOOTH_HINT,cHint);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT,cHint);

    if assigned(FOnInitialize) then
      FOnInitialize(self);

    FInitialized:=true;
    DoResize(self);
    Invalidate;
  end;
end;

procedure TGLContext.DoPaint(Sender: TObject);
begin
  if not FInitialized then
    Initialize;
  if not (csDestroying in Parent.ComponentState) and MakeCurrent then
  begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(cFovy,width/height,1.0,1000.0);

    glMatrixMode(GL_MODELVIEW);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glLoadIdentity();

    glTranslatef(FTranslation.X/10, FTranslation.Y/10, FTranslation.Z);
    glRotatef(FRotation.X,1.0,0.0,0.0);
    glRotatef(FRotation.Y,0.0,1.0,0.0);
    glRotatef(FRotation.Z,0.0,0.0,1.0);

    if assigned(FPaintObjects) then
      FPaintObjects(self);

    SwapBuffers;

    inc(FFrameCount);
    inc(FLastFrameTicks,FrameDiffTimeInMSecs);
    if (FLastFrameTicks>=1000) then
    begin
      Ffps:=FFrameCount;
      dec(FLastFrameTicks,1000);
      FFrameCount:=0;
    end;
  end;
end;

function TGLContext.GetFps: integer;
begin
  Result:=FFps;
end;

procedure TGLContext.ConvertColor(aColor: TColor; var r, g, b: GLFloat);
begin
  // convert 0..255 range into 0..1 range
  aColor := ColorToRGB(aColor);
  r:=(aColor and $FF) * (1 / 255);
  g:=((aColor shr 8) and $FF) * (1 / 255);
  b:=((aColor shr 16) and $FF) * (1 / 255);
end;

procedure TGLContext.DoResize(Sender: TObject);
begin
  if (FInitialized) and MakeCurrent then
    glViewport(0, 0, Width, Height);
end;

constructor TGLContext.Create(aOwner: TComponent; aTransformations:TTransformations);
begin
  inherited Create(aOwner);
  Parent:=aOwner as TWinControl;
  DoubleBuffered:=true;
  {$ifdef darwin} //check initialize for other OS'es
  Multisampling:=8;
  {$endif}
  Align:=alClient;
  OnPaint:=@DoPaint;
  OnResize:=@DoResize;
  OnMouseMove:=@DoMouseMove;
  OnMouseWheel:=@DoMouseWheel;
  FPerspectiveByMouse:=true;
  FTransformations:=aTransformations;
  FTranslation.Z:=-30;
  FRotation.X:=60;
  FLimitRotationX:=-1;
  FBkColor:=clForm;
  FCanUseMipmaps:=false;
end;

destructor TGLContext.Destroy;
begin
  inherited;
end;

function TGLContext.ScreenShot: TBitmap;
var
  p: array of array[0..3] of byte;
  aViewport: array[0..3] of GlInt;
  c: TFPColor;
  dummy:TLazIntfImage;
  dummyHandle, dummyMaskHandle: HBitmap;
  x,y:word;
  z:longword;
begin
  MakeCurrent;
  glGetIntegerv(GL_VIEWPORT, aViewport);

  Result:=TBitmap.Create;
  Result.Width:=aViewport[2];
  Result.Height:=aViewport[3];

  dummy:=TLazIntfImage.Create(0,0);
  try
    dummy.LoadFromBitmap(result.Handle, result.MaskHandle);

    setlength(p, aViewport[2] * aViewport[3]);
    glReadPixels(0, 0, aViewport[2], aViewport[3], GL_RGBA, GL_UNSIGNED_BYTE, @p[0,0]);
    z:=0;
    for y:=0 to Result.Height-1 do
     for x:=0 to Result.Width-1 do
    begin
      c.Red:=p[z][0]*256;;
      c.Green:=p[z][1]*256;;
      c.Blue:=p[z][2]*256;;
      dummy.Colors[x,Result.Height-1-y]:=c;
      inc(z);
    end;

    dummy.CreateBitmaps(dummyHandle, dummyMaskHandle, false);
    result.Handle := dummyHandle;
    result.MaskHandle := dummyMaskHandle;

    setlength(p,0);
  finally
    dummy.free;
  end;
end;

{$else}

procedure TGLContext.SetLight(AValue: TLight);
begin

end;

procedure TGLContext.SetBkColor(AValue: TColor);
begin

end;

constructor TGLContext.Create(aOwner: TComponent;
  aTransformations: TTransformations);
begin

end;

destructor TGLContext.Destroy;
begin
  inherited Destroy;
end;

{$endif} //UseOpenGl

end.
{
case of
       0:glBlendFunc(GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA);
       1:glBlendFunc(GL_DST_COLOR, GL_SRC_COLOR);
       2:glBlendFunc(GL_DST_COLOR, GL_SRC_ALPHA);
       3:glBlendFunc(GL_ONE, GL_ZERO);
       4:glBlendFunc(GL_ONE, GL_ONE);
       5:glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE_MINUS_SRC_COLOR);
       6:glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
       7:glBlendFunc(GL_ONE_MINUS_DST_ALPHA, GL_DST_ALPHA);
       8:glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
       9:glBlendFunc(GL_SRC_ALPHA, GL_SRC_COLOR);
       10:glBlendFunc(GL_ZERO, GL_ZERO);
       11:glBlendFunc(GL_ZERO, GL_ONE);
      end;
}
