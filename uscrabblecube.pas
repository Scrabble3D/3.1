{ Scrabblecube

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

unit uscrabblecube;

{$mode objfpc}{$H+}

{$I conditions.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, Fileutil, LResources,
  IntfGraphics, FPImage, LCLProc, LCLIntf,
  {$ifdef UseOpenGl}
  GL, glext, glu,
  {$endif}
  uglcontext, utexture,
  uscrabble, uboard,
  uletter, utypes
  ;

type

  {$ifdef UseOpenGl}
  TVertex = packed record //VBO
    s,t,
    u,v,w,
    x,y,z : TGLFloat; //GL_T2F_N3F_V3F
  end;
  {$endif}

  TOnGetFieldColor=function(x,y,z:byte; aLetter:TLetter; var a:single; var FieldType:byte):TColor of object;
  TOnGetFieldLetter=function(x,y,z:byte):TLetter of object;
  TOnDrawLetter=procedure(aLetter:TLetter;aCanvas:TCanvas) of object;

  { TScrabbleCube }

  TScrabbleCube=class{$ifdef useopengl}(TGlContext){$endif}
    private
      {$ifdef useopengl}
      bshalf: single;
      UseVBO: boolean;
      Cube: TGluint;
      FInitialized: boolean;
      Textures: TTextureList;
      procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure {%H-}DoMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;

      procedure PaintPlane;
      procedure PaintCoords;
      procedure PaintCubes;
      procedure PaintLetters;
      procedure DoPaint(Sender: TObject);

      procedure CallList(aValue:string);
      procedure DoInitialize(Sender: TObject);
      {$endif}
    private
      FTransparency: byte;
      FUseGreekLetter: boolean;
      FOnGetFieldColor: TOnGetFieldColor;
      FOnGetFieldLetter: TOnGetFieldLetter;
      FOnDrawLetter: TOnDrawLetter;
    protected
    public
      {$ifdef useopengl}
      constructor Create(aOwner: TComponent);
      destructor Destroy;override;
      procedure ResetPosition;
      procedure ClearTextures;
      property Fps;
      {$else}
      constructor Create(aOwner:TComponent);
      procedure Invalidate;
      {$endif}
      property Transparency : byte read FTransparency write FTransparency;
      property UseGreekLetter:boolean read FUseGreekLetter write FUseGreekLetter;
      property OnGetFieldColor:TOnGetFieldColor read FOnGetFieldColor write FOnGetFieldColor;
      property OnGetFieldLetter:TOnGetFieldLetter read FOnGetFieldLetter write FOnGetFieldLetter;
      property OnDrawLetter:TOnDrawLetter read FOnDrawLetter write FOnDrawLetter;
    end;

implementation

{$ifdef useopengl}

const
  MaxTransparency = 50;

{ TScrabbleCube }

procedure TScrabbleCube.DoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  z: single;
  zr: integer;
  ad: TActiveDimension;
begin
  if Shift=[ssAlt] then
  begin
    z:=WheelDelta/120;
    if byte(round(FTransparency+z)) in [1..MaxTransparency] then
      FTransparency:=round(FTransparency+z);
    Invalidate;
  end else
  if Shift=[ssCtrl] then
  begin
    zr:=round(WheelDelta/120);
    ad:=Scrabble.ActiveDimension;
    if (ad.Position+zr>=0) and
      (ad.Position+zr<Scrabble.BoardSize) then
    begin
      ad.Position:=ad.Position+zr;
      Scrabble.ActiveDimension:=ad;
    end;
  end else
    inherited;
end;

procedure TScrabbleCube.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ad: TActiveDimension;
begin
  Cursor:=crDefault;
  if (Button=mbRight) and (Shift=[ssCtrl]) then
  begin
    ad:=Scrabble.ActiveDimension;
    ad.Axis:=TDimension((integer(ad.Axis)+1) mod 3);
    Scrabble.ActiveDimension:=ad;
  end;
end;

procedure TScrabbleCube.DoInitialize(Sender: TObject);
const
  //6 sides x 4 triangles x 8 coordinates (st,uvw,xyz)
  cCube:array[0..6*4*8-1] of TGLFloat=(
  {Front Face}
  1.0, 0.0, 0.0, 0.0, 1.0, 0.5, 0.5, 0.5,
  0.0, 0.0, 0.0, 0.0, 1.0,-0.5, 0.5, 0.5,
  0.0, 1.0, 0.0, 0.0, 1.0,-0.5,-0.5, 0.5,
  1.0, 1.0, 0.0, 0.0, 1.0, 0.5,-0.5, 0.5,
  {Back Face}
  0.0, 0.0, 0.0, 0.0,-1.0, 0.5, 0.5,-0.5,
  0.0, 1.0, 0.0, 0.0,-1.0, 0.5,-0.5,-0.5,
  1.0, 1.0, 0.0, 0.0,-1.0,-0.5,-0.5,-0.5,
  1.0, 0.0, 0.0, 0.0,-1.0,-0.5, 0.5,-0.5,
  {Right Face}
  1.0, 0.0, 1.0, 0.0, 0.0, 0.5, 0.5,-0.5,
  0.0, 0.0, 1.0, 0.0, 0.0, 0.5, 0.5, 0.5,
  0.0, 1.0, 1.0, 0.0, 0.0, 0.5,-0.5, 0.5,
  1.0, 1.0, 1.0, 0.0, 0.0, 0.5,-0.5,-0.5,
  {Left Face}
  1.0, 0.0,-1.0, 0.0, 0.0,-0.5, 0.5, 0.5,
  0.0, 0.0,-1.0, 0.0, 0.0,-0.5, 0.5,-0.5,
  0.0, 1.0,-1.0, 0.0, 0.0,-0.5,-0.5,-0.5,
  1.0, 1.0,-1.0, 0.0, 0.0,-0.5,-0.5, 0.5,
  {Top Face}
  1.0, 0.0, 0.0, 1.0, 0.0, 0.5, 0.5,-0.5,
  0.0, 0.0, 0.0, 1.0, 0.0,-0.5, 0.5,-0.5,
  0.0, 1.0, 0.0, 1.0, 0.0,-0.5, 0.5, 0.5,
  1.0, 1.0, 0.0, 1.0, 0.0, 0.5, 0.5, 0.5,
  {Bottom Face}
  0.0, 1.0, 0.0,-1.0, 0.0,-0.5,-0.5,-0.5,
  1.0, 1.0, 0.0,-1.0, 0.0, 0.5,-0.5,-0.5,
  1.0, 0.0, 0.0,-1.0, 0.0, 0.5,-0.5, 0.5,
  0.0, 0.0, 0.0,-1.0, 0.0,-0.5,-0.5, 0.5);
var
  aLight: TLight;
  {$ifndef LCLQt}
  VBOPointer: Pointer;
  vGL_T2F_N3F_V3F: ^TVertex;
  {$endif}
  i: integer;
begin
  {$ifndef LCLQt}
  if Load_GL_version_1_5 then
  begin
    //bonus marker
    UseVBO:=true;
    glGenBuffers(1, @Cube);
    glBindBuffer(GL_ARRAY_BUFFER, Cube);
    glEnableClientState(GL_VERTEX_ARRAY);
    glBufferData(GL_ARRAY_BUFFER, 4*3*3*sizeof(TVertex), nil, GL_STATIC_DRAW);
    VBOPointer:=glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    for i:=0 to 4*6-1 do
    begin
      vGL_T2F_N3F_V3F := VBOPointer;
      vGL_T2F_N3F_V3F^.s:=cCube[i*8+0];
      vGL_T2F_N3F_V3F^.t:=cCube[i*8+1];
      vGL_T2F_N3F_V3F^.u:=cCube[i*8+2];
      vGL_T2F_N3F_V3F^.v:=cCube[i*8+3];
      vGL_T2F_N3F_V3F^.w:=cCube[i*8+4];
      vGL_T2F_N3F_V3F^.x:=cCube[i*8+5];
      vGL_T2F_N3F_V3F^.y:=cCube[i*8+6];
      vGL_T2F_N3F_V3F^.z:=cCube[i*8+7];
      inc(VBOPointer,sizeof(TVertex));
//      inc(integer(VBOPointer),sizeof(TVertex));
    end;
    glUnMapBuffer(GL_ARRAY_BUFFER);
  end else
  {$endif}
  begin
   UseVBO:=false;
   Cube:=glGenLists(1);
   glNewList(Cube, GL_COMPILE);
    glBegin(GL_QUADS);
     for i:=0 to 4*6-1 do
     begin
       glTexCoord2f(cCube[i*8+0],cCube[i*8+1]);
       glNormal3f(cCube[i*8+2],cCube[i*8+3],cCube[i*8+4]);
       glVertex3f(cCube[i*8+5],cCube[i*8+6],cCube[i*8+7]);
     end;
    glEnd;
   glEndList;
  end;
  FInitialized:=true;

  aLight:=Light;
  aLight.Diffuse:=100;
  aLight.Ambient:=100;
  Light:=aLight;
end;

procedure TScrabbleCube.PaintPlane;
const
  cLight=1.0;
  cDark=0.5;
  cTransp=0.5;
var
  m,n,o : single;
begin
  m:=-bshalf-1;
  n:=bshalf;
  o:=-Scrabble.ActiveDimension.Position+bshalf;
  case LightOrDark(BkColor) of
   clWhite: glColor4f(cLight,cLight,cLight,cTransp);
   clBlack: glColor4f(cDark,cDark,cDark,cTransp);
  end;

  case Scrabble.ActiveDimension.Axis of
   dx: begin
        glBegin(GL_QUADS);
         glVertex3f(m, m, o-1);
         glVertex3f(m, n, o-1);
         glVertex3f(n, n, o-1);
         glVertex3f(n, m, o-1);
        glEnd;
      end;
   dy: begin
        glBegin(GL_QUADS);
          glVertex3f(m, o-1, m);
          glVertex3f(m, o-1, n);
          glVertex3f(n, o-1, n);
          glVertex3f(n, o-1, m);
        glEnd;
      end;
   dz: begin
        glBegin(GL_QUADS);
          glVertex3f(-o, m, m);
          glVertex3f(-o, m, n);
          glVertex3f(-o, n, n);
          glVertex3f(-o, n, m);
        glEnd;
      end;
   end;//case
end;

procedure TScrabbleCube.PaintCoords;
const
  cPercent=75;
var
  i,z:integer;
  s : string;
begin
  glColor4f(1,1,1,1);

  for i:=0 to Scrabble.BoardSize-1 do   //1..n
  begin
    glPushMatrix;
    glTranslatef(i-bshalf,-bshalf-0.5,bshalf+0.5);
    glRotatef(Rotation.X,-1.0,0.0,0.0);
    glRotatef(Rotation.Y,0.0,-1.0,0.0);

    s:=Scrabble.PosToString(dx,i,not UseGreekLetter);
    z:=Textures.AddFromString(s,cPercent,s);
    glBindTexture(GL_TEXTURE_2D,Textures[z]);
    CallList('LetterField');
    glPopMatrix;
  end;

  for i:=0 to Scrabble.BoardSize-1 do  //A..Z
  begin
    glPushMatrix;
    glTranslatef(-bshalf-1.0,i-bshalf,bshalf+0.0);
    glRotatef(Rotation.X,-1.0,0.0,0.0);
    glRotatef(Rotation.Y,0.0,-1.0,0.0);

    s:=Scrabble.PosToString(dy,Scrabble.BoardSize-1-i,not UseGreekLetter);
    z:=Textures.AddFromString(s,cPercent,s);
    glBindTexture(GL_TEXTURE_2D,Textures[z]);
    CallList('LetterField');
    glPopMatrix;
  end;

  for i:=0 to Scrabble.BoardSize-1 do //alpha..zeta
  begin
    glPushMatrix;
    glTranslatef(-bshalf-1.5,-bshalf-0.5,i-bshalf);
    glRotatef(Rotation.X,-1.0,0.0,0.0);
    glRotatef(Rotation.Y,0.0,-1.0,0.0);

    s:=Scrabble.PosToString(dz,Scrabble.BoardSize-1-i,not UseGreekLetter);
    z:=Textures.AddFromString(s,cPercent,s);
    glBindTexture(GL_TEXTURE_2D,Textures[z]);
    CallList('LetterField');
    glPopMatrix;
  end;
end;

procedure TScrabbleCube.PaintCubes;
var
  x,y,z: integer;
  r,g,b: TGLFloat;
  a: single;
  aFieldType: byte;
  aLetter: TLetter;
begin
  if assigned(FOnGetFieldColor) then
  for x:=0 to Scrabble.BoardSize-1 do
   for y:=0 to Scrabble.BoardSize-1 do
    for z:=0 to Scrabble.BoardSize-1 do
  begin
    aLetter:=OnGetFieldLetter(x,y,z);
    if aLetter<>nil then
      Continue;

    glPushMatrix;
    //position
    glTranslatef(x-bshalf,y-bshalf,z-bshalf);
    //color
    ConvertColor(FOnGetFieldColor(x,Scrabble.BoardSize-y-1,Scrabble.BoardSize-z-1,aLetter,a,aFieldType),r,g,b);
    glColor4f(r,g,b,a*(FTransparency/MaxTransparency));
    //draw
    CallList('Cube');
    glPopMatrix;
  end;
end;

procedure TScrabbleCube.PaintLetters;
var
  x,y,z,t,i: integer;
  aLight  : array[0..3] of GLfloat;
  a: single;
  s: string;
  aFieldType: byte;
  aLetter: TLetter;
  aBitmap: TBitmap;
begin
  if assigned(FOnGetFieldColor) then
  for x:=0 to Scrabble.BoardSize-1 do
   for y:=0 to Scrabble.BoardSize-1 do
    for z:=0 to Scrabble.BoardSize-1 do
  begin
    aLetter:=OnGetFieldLetter(x,y,z);

    if aLetter=nil then
      Continue;

    glPushMatrix;
    glTranslatef(aLetter.Where[dx]-bshalf,bshalf-aLetter.Where[dy]-1,bshalf-aLetter.Where[dz]-1);

    ConvertColor(FOnGetFieldColor(aLetter.Where[dx],aLetter.Where[dy],aLetter.Where[dz],aLetter,a,aFieldType),
                 aLight[0],aLight[1],aLight[2]);
    for t:=0 to 2 do aLight[t]:=aLight[t]/2;
    aLight[3]:=1.0;
//    glLightfv(GL_LIGHT1, GL_AMBIENT, @aLight);
    glLightfv(GL_LIGHT1, GL_DIFFUSE, @aLight);
    glMaterialfv(GL_FRONT, GL_EMISSION, @aLight);
    s:=UTF8Encode(aLetter.What+inttostr(aLetter.Value)+inttostr(aLetter.Who));
    t:=Textures.IndexOfName(s);
    if t=-1 then
    for i:=0 to length(cTextureSize)-1 do
    begin
      aBitmap:=TBitmap.Create;
      try
        with aBitmap do
        begin
          PixelFormat:=pf24bit;
          Width:=cTextureSize[i];
          Height:=cTextureSize[i];
          Canvas.Brush.Color:=clWhite;
          Canvas.Brush.Style:=bsSolid;
          Canvas.FillRect(0,0,cTextureSize[i],cTextureSize[i]);
          Canvas.Pen.Color:=clLtGray;
          Canvas.Rectangle(Canvas.ClipRect);
        end;
        FOnDrawLetter(aLetter,aBitmap.Canvas);
        t:=Textures.AddFromBitmap(s, aBitmap);
      finally
        aBitmap.Free;
      end;
    end; //t=-1
    glBindTexture(GL_TEXTURE_2D,Textures[t]);
    CallList('Cube');
    glPopMatrix;
  end; //for to
end;

procedure TScrabbleCube.CallList(aValue: string);
begin
  case aValue of
   'LetterField':
   begin
     glBegin(GL_QUADS);
      glNormal3f(0.0, 1.0, 0.0);
      glTexCoord2f(1.0, 0.0); glVertex3f( 0.5, 0.5, 0.5);
      glTexCoord2f(0.0, 0.0); glVertex3f(-0.5, 0.5, 0.5);
      glTexCoord2f(0.1, 1.0); glVertex3f(-0.5,-0.5, 0.5);
      glTexCoord2f(1.0, 1.0); glVertex3f( 0.5,-0.5, 0.5);
     glEnd;
   end;
   'Cube':
   begin
     if UseVBO then
     begin
       glInterleavedArrays(GL_T2F_N3F_V3F, sizeOf(TVertex), nil);
       glDrawArrays(GL_QUADS, 0, 4*6);
     end else
       glCallList(Cube);
   end;
  end; //case
end;

procedure TScrabbleCube.DoPaint(Sender: TObject);
var
  z:glenum;
begin
  if ([csDestroying,csLoading]*ComponentState=[]) and
       (Scrabble.BoardSize>0) then
  begin
    if abs(Scrabble.BoardSize/2-bshalf)>0.001 then
    begin
      bshalf:=Scrabble.BoardSize/2;
      with Translation do Z:=-Scrabble.BoardSize*3.5;

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      glLoadIdentity;
      glTranslatef(Translation.X/10, Translation.Y/10, Translation.Z);
    end;
    //plane
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    PaintPlane;
    glEnable(GL_CULL_FACE);
    //coords
    glEnable(GL_TEXTURE_2D);
    glBlendFunc(GL_DST_COLOR, GL_ZERO);
    PaintCoords;
    //cubes w/o letters
    glDisable(GL_TEXTURE_2D);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    PaintCubes;
    //cubes with letters
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    glEnable(GL_LIGHTING);
    glDisable(GL_LIGHT0);
    glEnable(GL_LIGHT1);
    PaintLetters;

    z:=glGetError();
    if (z<>GL_NO_ERROR) then
      OnMessage(smDebug,'ScrabbleCube Error: '+gluErrorString(z));
  end;
end;

constructor TScrabbleCube.Create(aOwner: TComponent);
begin
  inherited Create(aOwner,[rx,ry,rz,tz]);
  Name:='ScrabbleCube';
  OnPaintObjects:=@DoPaint;
  OnInitialize:=@DoInitialize;

  OnMouseUp:=@DoMouseUp;
  OnMouseWheel:=@DoMouseWheel;
  Align:=alClient;
  ResetPosition;
  FUseGreekLetter:=true;
  FTransparency:=25;
  FInitialized:=false;
  Textures:=TTextureList.Create;
  with Rotation do X:=360;  //unlimited
end;

destructor TScrabbleCube.Destroy;
begin
  if FInitialized then
  begin
    if UseVBO then
      glDeleteBuffers(1,@Cube) else
      glDeleteLists(Cube,1);
  end;
  Textures.Free;
  inherited Destroy;
end;

procedure TScrabbleCube.ResetPosition;
var
  p:TPosition;
begin
  p:=Translation;
  p.X:=0;
  p.Y:=0;
  if assigned(Scrabble) and (Scrabble.BoardSize>0) then
    p.Z:=-Scrabble.BoardSize*3.5 else
    p.Z:=-30;
  Translation:=p;
  p:=Rotation;
  p.X:=360;
  p.Y:=0;
  p.Z:=0;
  Rotation:=p;
  Invalidate;
end;

procedure TScrabbleCube.ClearTextures;
begin
  Textures.Clear;
  Invalidate;
end;

{$else} //useopengl

constructor TScrabbleCube.Create(aOwner: TComponent);
begin
  inherited Create;
  TWinControl(aOwner).Caption:='OpenGL is not available for Qt version of Scrabble3D';
end;

procedure TScrabbleCube.Invalidate;
begin
end;

{$endif}

initialization

 {$I textures.res}

end.

