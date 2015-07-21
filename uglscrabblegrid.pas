{ 2D view based on OpenGL

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

unit uglscrabblegrid;

{$mode objfpc}{$H+}

{$I conditions.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics, Fileutil,
  LResources, LCLIntf, LCLProc, zipper,
  {$ifdef UseOpenGl}
  GL, glu, glext, uglcontext, uwavefront,
  {$endif}

  uscrabblegrid,
  uscrabble, utexture, uconfig,
  uletter, utypes;

type

  TVertex = packed record
    x,y,z : TGLFloat; //GL_V3F
  end;

  { TGlScrabbleGrid }

  TLetterObj=class;

  TPaintOptions=set of (poBlend,poCull,poTexture,poLight0,poLight1);

  TGlScrabbleGrid=class(TScrabbleGrid)
    private  //draw
      Board       : TWavefront;
      Piece       : TWavefront;
      Letter      : TLetterObj;
      Textures    : TTextureList;
      Markers     : TGluint;
      UseVBO      : boolean;//markers using vertext buffer objects when >1.5
      FInitialized: boolean;

      procedure DoInitialize(Sender: TObject);
      procedure LoadNames(Sender:TObject);

      procedure DoPaint(Sender: TObject);
      procedure PaintOptions(po:TPaintOptions);
      procedure PaintFields(const DoLoadName: boolean=false);
      procedure PaintCoords(const DoLoadName: boolean=false);
      procedure PaintMarkers();
      procedure PaintLetters();
      procedure PaintPieces(const PaintAll: boolean=true);
    private //props
      FShowMarkers, FTransparentBoard, FReflection: boolean;
      FDesign     : string;
      function GetLight: TLight;
      function GetRotation: TPosition;
      function GetSticky: boolean;
      function GetTranslation: TPosition;
      procedure SetBkColor(AValue: TColor);
      procedure SetDesign(AValue: string);
      procedure SetLight(AValue: TLight);
      procedure SetMipmapping(AValue: boolean);
      procedure SetRotation(AValue: TPosition);
      procedure SetSticky(AValue: boolean);
      procedure SetTranslation(AValue: TPosition);
      procedure ReloadModelsFrom(const aFileName:string);
      procedure CallList(aValue:string);
    protected //abstract
      FSelectedCell       : TPoint;
      function GetSelectedCell: TPoint; override;
      function GetFps: integer; override;
      function MouseToCell(x, y: integer; var ax,ay,az: byte):boolean; override;
    public
      GridView    : TGlContext;
      constructor Create(aOwner:TComponent); override;
      destructor Destroy; override;
      procedure Paint(Sender:TObject); override;
      procedure ResetPosition;
      procedure RefreshLetters;
      function ScreenShot:TBitmap; override;

      property Rotation:TPosition read GetRotation write SetRotation;
      property Translation:TPosition read GetTranslation write SetTranslation;
      property Light:TLight read GetLight write SetLight;
      property BkColor:TColor write SetBkColor;
      property ShowMarkers: boolean write FShowMarkers ;
      property TransparentBoard: boolean write FTransparentBoard;
      property Reflection: boolean write FReflection;
      property Design:string read FDesign write SetDesign;
      property AdjustByMouse:boolean read GetSticky write SetSticky;
      property Mipmapping: boolean write SetMipmapping;
    end;

  { TLetterObj }

  TLetterObj=class(TWavefront)
  private
    FParent : TGlScrabbleGrid;
    procedure SetLetter(aLetter: TLetter);
  published
    property Letter:TLetter write SetLetter;
  public
    constructor Create(aParent:TGlScrabbleGrid);
  end;

implementation

const
  cSpace=0.025; //space between fields
  cDist=0.015;  //y distance from zero
  cBlack:array[0..3] of GlFloat=(0.0,0.0,0.0,0.0);
//  cCapSize=40;  //percentage of captions, e.g. coords

{ TPiece }

procedure TLetterObj.SetLetter(aLetter: TLetter);
var
  i,j,z:integer;
  s:string;
  aBitmap:Graphics.TBitmap;
begin
  for i:=0 to Materials.Count-1 do
   if (UpperCase(Materials[i].Name)='LETTER') and assigned(FParent.OnDrawLetter) then
   begin
     s:=UTF8Encode(aLetter.What+inttostr(aLetter.Value)+inttostr(aLetter.Who));
     z:=Textures.IndexOfName(s);
     //create texture if not done yet
     if z=-1 then
     for j:=0 to length(cTextureSize)-1 do
     begin
       aBitmap:=Graphics.TBitmap.Create;
       try
         with aBitmap do
         begin
           PixelFormat:=pf24bit;
           Width:=cTextureSize[j];
           Height:=cTextureSize[j];
           Canvas.Brush.Color:=clWhite;//clBlack;//
           Canvas.Brush.Style:=bsSolid;
           Canvas.FillRect(Bounds(0,0,cTextureSize[j],cTextureSize[j]));
         end;
         FParent.OnDrawLetter(aLetter,aBitmap.Canvas);
         z:=Textures.AddFromBitmap(s, aBitmap);
       finally
         aBitmap.Free;
       end;
     end; //z=-1
     //assign texture to current material
     Materials[i].TextureName:=s;
     break;
   end;//is Letter
end;

constructor TLetterObj.Create(aParent: TGlScrabbleGrid);
begin
  inherited Create;
  FParent:=aParent;
end;

{ TGlScrabbleGrid }
procedure TGlScrabbleGrid.LoadNames(Sender: TObject);
begin
  glDisable(GL_CULL_FACE);//keep faces
  glScalef(10.0/BoardSize,1.0,10.0/BoardSize); //scale for different board sizes
  PaintFields(true);
  PaintCoords(true);
  glScalef(BoardSize/10.0,1.0,BoardSize/10.0); //unscale
end;

function TGlScrabbleGrid.MouseToCell(x, y: integer; var ax, ay, az: byte): boolean;
var
  z: integer;
begin
  if ([csDestroying,csLoading]*ComponentState=[]) and
     (BoardSize>0) and
     (x>=0) and (y>=0) then
    z:=GridView.PickID(x,y,@LoadNames) else
    exit(false);
  if z>-1 then
  begin
    FSelectedCell.X:=z div 256;
    FSelectedCell.Y:=z mod 256;
    if (FSelectedCell.X>0) and (FSelectedCell.Y>0) and
       (FSelectedCell.x<=BoardSize) and (FSelectedCell.y<=BoardSize) then
      Scrabble.Convert2DTo3D(FSelectedCell.X-1,FSelectedCell.Y-1,ax,ay,az);
    Result:=true;
  end else
    Result:=false;
end;

procedure TGlScrabbleGrid.DoInitialize(Sender: TObject);
const
  cMS=0.15; //markersize
  //4 x triangles = 3 with 3 coordinates (3 position)
  cMarkers:array[0..4*3*3-1] of TGLFloat=(
              0.5+cMS,       cDist, 1.0+cDist,
              0.5,           cDist, 1.0+cDist+cMS,
              0.5-cMS,       cDist, 1.0+cDist,

              0.5-cMS,       cDist, 0.0-cDist,
              0.5,           cDist, 0.0-cDist-cMS,
              0.5+cMS,       cDist, 0.0-cDist,

              1.0+cDist,     cDist, 0.5-cMS,
              1.0+cDist+cMS, cDist, 0.5,
              1.0+cDist,     cDist, 0.5+cMS,

              0.0-cDist,     cDist, 0.5+cMS,
              0.0-cDist-cMS, cDist, 0.5,
              0.0-cDist,     cDist, 0.5-cMS);
{$ifndef LCLQt}  //vbo does not work with Qt
 {$define VBO}
{$endif}
var
  {$ifdef VBO}
  VBOPointer: Pointer;
  vGL_V3F: ^TVertex;
  i: integer;
  {$endif}
  z: GLenum;
begin
  {$ifdef VBO}
  if Load_GL_version_1_5 then
  begin
    //bonus marker
    UseVBO:=true;
    glGenBuffers(1, @Markers);
    glBindBuffer(GL_ARRAY_BUFFER, Markers);
    glEnableClientState(GL_VERTEX_ARRAY);
    glBufferData(GL_ARRAY_BUFFER, 4*3*3*sizeof(TVertex), nil, GL_STATIC_DRAW);
    VBOPointer:=glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    for i:=0 to 4*3-1 do
    begin
      vGL_V3F := VBOPointer;
      vGL_V3F^.x:=cMarkers[i*3+0];//3 = length(TGL_V3FVertex)
      vGL_V3F^.y:=cMarkers[i*3+1];
      vGL_V3F^.z:=cMarkers[i*3+2];
      inc(VBOPointer,sizeof(TVertex));
    end;
    glUnMapBuffer(GL_ARRAY_BUFFER);
  end else
  {$endif}
  begin
    UseVBO:=false;
    Markers:=glGenLists(1);
    glNewList(Markers, GL_COMPILE);
     glBegin(GL_TRIANGLES);
      glNormal3f(0.0, 1.0, 0.0);
      glVertex3f(cMarkers[0], cMarkers[1], cMarkers[2]);
      glVertex3f(cMarkers[3], cMarkers[4], cMarkers[5]);
      glVertex3f(cMarkers[6], cMarkers[7], cMarkers[8]);
     glEnd;
     glBegin(GL_TRIANGLES);
      glNormal3f(0.0, 1.0, 0.0);
      glVertex3f(cMarkers[ 9], cMarkers[10], cMarkers[11]);
      glVertex3f(cMarkers[12], cMarkers[13], cMarkers[14]);
      glVertex3f(cMarkers[15], cMarkers[16], cMarkers[17]);
     glEnd;
     glBegin(GL_TRIANGLES);
      glNormal3f(0.0, 1.0, 0.0);
      glVertex3f(cMarkers[18], cMarkers[19], cMarkers[20]);
      glVertex3f(cMarkers[21], cMarkers[22], cMarkers[23]);
      glVertex3f(cMarkers[24], cMarkers[25], cMarkers[26]);
     glEnd;
     glBegin(GL_TRIANGLES);
      glNormal3f(0.0, 1.0, 0.0);
      glVertex3f(cMarkers[27], cMarkers[28], cMarkers[29]);
      glVertex3f(cMarkers[30], cMarkers[31], cMarkers[32]);
      glVertex3f(cMarkers[33], cMarkers[34], cMarkers[35]);
     glEnd;
    glEndList;
  end;
  FInitialized:=true;

  //models
  if FDesign='' then
  begin
    Board.LoadFromRessource('board');
    Piece.LoadFromRessource('piece');
    Letter.LoadFromRessource('letter');
  end else
    ReloadModelsFrom(FDesign);

  z:=glGetError();
  if (z<>GL_NO_ERROR) then
    OnMessage(smDebug,'ScrabbleGrid DoInitialize: '+gluErrorString(z));
end;

procedure TGlScrabbleGrid.PaintFields(const DoLoadName: boolean);
var
  aCol,aRow: byte;
  x,y,z: byte;
  i: integer;
  aLight: array[0..3] of GLfloat;
  a: single;
  aFieldType: byte;
begin
  for aCol:=0 to BoardSize-1 do
   for aRow:=0 to BoardSize-1 do
  begin
    glPushMatrix;
    //position
    Scrabble.Convert2DTo3D(aCol,aRow,x,y,z);
    glTranslatef(aCol-BSHalf+cSpace/2,cDist,aRow-BSHalf+cSpace/2);
    //color
    if assigned(OnGetFieldColor) then
      GridView.ConvertColor(OnGetFieldColor(x,y,z,nil,a,aFieldType),aLight[0],aLight[1],aLight[2]);
    aLight[3]:=0.0;
    glLightfv(GL_LIGHT1, GL_AMBIENT, @aLight);
    glLightfv(GL_LIGHT1, GL_DIFFUSE, @cBlack);
    //position id
    if DoLoadName then
      glLoadName((aCol+1)*256+(aRow+1));
    //quads
    case aFieldType of
      0: i:=Textures.IndexOfName('Normal.bmp');
      1: i:=Textures.IndexOfName('DoubleLetter.bmp');
      2: i:=Textures.IndexOfName('TripleLetter.bmp');
      3: i:=Textures.IndexOfName('QuadLetter.bmp');
      4: i:=Textures.IndexOfName('DoubleWord.bmp');
      5: i:=Textures.IndexOfName('TripleWord.bmp');
      6: i:=Textures.IndexOfName('QuadWord.bmp');
      7: i:=Textures.IndexOfName('Start.bmp');
      8: i:=Textures.IndexOfName('Letter.bmp');
      9: i:=Textures.IndexOfName('NewLetter.bmp');
     10: i:=Textures.IndexOfName('MalusSingleLetter.bmp');
     11: i:=Textures.IndexOfName('MalusDoubleLetter.bmp');
     12: i:=Textures.IndexOfName('MalusTripleLetter.bmp');
     13: i:=Textures.IndexOfName('MalusQuadLetter.bmp');
     else
         i:=-1;
    end;//case
    //create texture if not done yet
    if i>-1 then
    begin
      glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D,Textures[i])
    end else
      glDisable(GL_TEXTURE_2D);
    //to blend or not to blend
    if FTransparentBoard then
      glEnable(GL_BLEND) else
      glDisable(GL_BLEND);
    CallList('LetterField');
    glPopMatrix;
  end; //for to acol, arow
end;

procedure TGlScrabbleGrid.PaintCoords(const DoLoadName: boolean);
var
  aCol: byte;
  z: integer;
  s: string;
begin
  if Scrabble.Dimension=D3 then
  begin
    glPushMatrix;
    glTranslatef(-1-BSHalf+cSpace/2,cDist,-1-BSHalf+cSpace/2);
    if DoLoadName then
      glLoadName(0);
    case Scrabble.ActiveDimension.Axis of
     dx : s:='z='+Scrabble.PosToString(dz,Scrabble.ActiveDimension.Position,not UseGreekLetter);
     dy : s:='y='+Scrabble.PosToString(dy,Scrabble.ActiveDimension.Position,not UseGreekLetter);
     dz : s:='x='+Scrabble.PosToString(dx,Scrabble.ActiveDimension.Position,not UseGreekLetter);
    end;

    z:=Textures.AddFromString(s,BoardSize*3,s);
    glBindTexture(GL_TEXTURE_2D, Textures[z]);
    CallList('LetterField');
    glPopMatrix;
  end;

  for aCol:=0 to BoardSize-1 do
  begin
    //horizontal
    glPushMatrix;
    glTranslatef(aCol-BSHalf+cSpace/2,cDist,-1-BSHalf+cSpace/2);
    if DoLoadName then
      glLoadName(256*(aCol+1));
    if BiDiMode=bdLeftToRight then
      z:=aCol else
      z:=BoardSize-1-aCol;
    case Scrabble.ActiveDimension.Axis of
     dx,dy : s:=Scrabble.PosToString(dx,z,not UseGreekLetter);
     dz    : s:=Scrabble.PosToString(dz,z,not UseGreekLetter);
    end;

    z:=Textures.AddFromString(s,BoardSize*3,s);
    glBindTexture(GL_TEXTURE_2D, Textures[z]);
    CallList('LetterField');
    glPopMatrix;

    //vertical
    glPushMatrix;
    if BiDiMode=bdLeftToRight then
      glTranslatef(-1-BSHalf+cSpace/2,cDist,aCol-BSHalf+cSpace/2) else
      glTranslatef(BSHalf+cSpace/2,cDist,aCol-BSHalf+cSpace/2);
    if DoLoadName then
      glLoadName(aCol+1);
    case Scrabble.ActiveDimension.Axis of
     dx,dz : s:=Scrabble.PosToString(dy,aCol,not UseGreekLetter);
     dy : s:=Scrabble.PosToString(dz,aCol,not UseGreekLetter);
    end;
    z:=Textures.AddFromString(s,BoardSize*3,s);
    glBindTexture(GL_TEXTURE_2D, Textures[z]);
    CallList('LetterField');
    glPopMatrix;
  end; //for to
end;

procedure TGlScrabbleGrid.PaintMarkers;
var
  aCol,aRow: byte;
  x,y,z: byte;
  aLight: array[0..3] of GLfloat;
  a: single;
  aFieldType: byte;
begin
  if FShowMarkers then
  begin
    glDepthFunc(GL_ALWAYS);
    for aCol:=0 to BoardSize-1 do
     for aRow:=0 to BoardSize-1 do
    begin
      Scrabble.Convert2DTo3D(aCol,aRow,x,y,z);
      //color
      if assigned(OnGetFieldColor) then
        GridView.ConvertColor(OnGetFieldColor(x,y,z,nil,a,aFieldType),aLight[0],aLight[1],aLight[2]);
      if aFieldType=0 then
        Continue;
      aLight[3]:=0;
      glPushMatrix;
      //light from color
      glLightfv(GL_LIGHT1, GL_AMBIENT, @aLight);
      glLightfv(GL_LIGHT1, GL_DIFFUSE, @cBlack);
      //position
      glTranslatef(aCol-BSHalf,cDist,aRow-BSHalf);
      //4 triangles
      CallList('FieldMarker');
      glPopMatrix;
    end;//for to
  end;
end;

procedure TGlScrabbleGrid.PaintPieces(const PaintAll: boolean=true);
var
  aCol,aRow: byte;
  x,y,z: byte;
  aLetter: TLetter;
begin
  if assigned(OnGetFieldLetter) then
  begin
    for aCol:=0 to BoardSize-1 do
     for aRow:=0 to BoardSize-1 do
     begin
       if not PaintAll and (aRow=BoardSize-1) then
         Continue; //bottom line piece should not have a shadow
       Scrabble.Convert2DTo3D(aCol,aRow,x,y,z);
       aLetter:=OnGetFieldLetter(x,y,z);
       if aLetter<>nil then
       begin
         glPushMatrix;
         //position
         glTranslatef(aCol-BSHalf,0.0,aRow-BSHalf);
         //draw piece
         Piece.Draw;
         //pop
         glPopMatrix;
       end;
     end;//for to
  end; //assigned
end;

procedure TGlScrabbleGrid.PaintLetters;
var
  aCol,aRow: byte;
  x,y,z: byte;
  aLight: array[0..3] of GLfloat;
  aLetter: TLetter;
  a: single;
  aFieldType: byte;
begin
  if assigned(OnGetFieldLetter) then
  begin
    glDepthFunc(GL_ALWAYS); //avoid z-fighting with fields
    for aCol:=0 to BoardSize-1 do
     for aRow:=0 to BoardSize-1 do
     begin
       Scrabble.Convert2DTo3D(aCol,aRow,x,y,z);
       aLetter:=OnGetFieldLetter(x,y,z);
       if aLetter<>nil then
       begin
         glPushMatrix;
         //position
         glTranslatef(aCol-BSHalf,0.0,aRow-BSHalf);
         //get color
         if assigned(OnGetFieldColor) then
         begin
           GridView.ConvertColor(OnGetFieldColor(x,y,z,aLetter,a,aFieldType),aLight[0],aLight[1],aLight[2]);
           aLight[3]:=0.0;
           glLightfv(GL_LIGHT1, GL_AMBIENT, @aLight);
           glLightfv(GL_LIGHT1, GL_DIFFUSE, @aLight);
         end;
         //draw letter
         Letter.Letter:=aLetter;
         Letter.Draw;
         //pop
         glPopMatrix;
       end;
     end;//for to
  end;//assigned
end;

procedure TGlScrabbleGrid.DoPaint(Sender: TObject);
var
  z: GLenum;
begin
  if ([csDestroying,csLoading]*ComponentState=[]) and
     (BoardSize>0) then
  begin
    PaintOptions([poLight0,poCull]);
    Board.Draw;   //board

    glScalef(10.0/BoardSize,1.0,10.0/BoardSize); //scale for different board sizes

    PaintOptions([poLight1,poTexture,poBlend]);
    PaintCoords;  //coordinates

    if FReflection then
    begin
      PaintOptions([poLight1]);
      glDepthFunc(GL_GEQUAL);
      glScalef(1.0,-0.5,1.0);
      PaintPieces(false);
      glScalef(1.0,-2.0,1.0);
      glDepthFunc(GL_LEQUAL);
    end;

    if FTransparentBoard then
      PaintOptions([poLight1,poBlend]) else
      PaintOptions([poLight1]);
    PaintFields;  //squares

    PaintOptions([poLight1]); //blending makes no sense because of additional fields background
    PaintMarkers; //markers

    PaintOptions([poLight0,poCull,poTexture]);
    PaintPieces;   //piece
    PaintOptions([poLight1,poCull,poTexture]);
    PaintLetters; //letter

    glScalef(BoardSize/10.0,1.0,BoardSize/10.0); //unscale

    z:=glGetError();
    if (z<>GL_NO_ERROR) then
      OnMessage(smDebug,'ScrabbleGrid DoPaint: '+gluErrorString(z));
  end;
end;

procedure TGlScrabbleGrid.PaintOptions(po: TPaintOptions);
const
  cAmbient:array[0..3] of GlFloat=(0.75,0.75,0.75,0);
  cDiffuse:array[0..3] of GlFloat=(0.25,0.25,0.25,0);
begin
  if poBlend in po then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_DST_COLOR, GL_ZERO);
  end else
    glDisable(GL_BLEND);

  if poCull in po then
    glEnable(GL_CULL_FACE) else
    glDisable(GL_CULL_FACE);
  glCullFace(GL_BACK);

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

  if poTexture in po then
    glEnable(GL_TEXTURE_2D) else
    glDisable(GL_TEXTURE_2D);

  if poLight0 in po then
    glEnable(GL_LIGHT0) else
    glDisable(GL_LIGHT0);

  if poLight1 in po then
  begin
    glEnable(GL_LIGHT1);  //background light, independent from illumination position
    glLightfv(GL_LIGHT1, GL_AMBIENT, @cAmbient);
    glLightfv(GL_LIGHT1, GL_DIFFUSE, @cDiffuse);
  end else
    glDisable(GL_LIGHT1); //foreground, only valid for pieces
end;

function TGlScrabbleGrid.GetLight: TLight;
begin
  Result:=GridView.Light;
end;

function TGlScrabbleGrid.GetRotation: TPosition;
begin
  Result:=GridView.Rotation;
end;

function TGlScrabbleGrid.GetSticky: boolean;
begin
  Result:=Gridview.PerspectiveByMouse;
end;

function TGlScrabbleGrid.GetSelectedCell: TPoint;
begin
  Result:=FSelectedCell;
end;

function TGlScrabbleGrid.GetTranslation: TPosition;
begin
  Result:=GridView.Translation;
end;

procedure TGlScrabbleGrid.SetBkColor(AValue: TColor);
begin
  GridView.BkColor:=aValue;
end;

procedure TGlScrabbleGrid.ReloadModelsFrom(const aFileName:string);
const
  aFieldTypes:array[0..13] of string=('Normal','DoubleLetter','TripleLetter',
          'QuadLetter','DoubleWord','TripleWord','QuadWord','Start','Letter',
          'NewLetter','MalusSingleLetter','MalusDoubleLetter','MalusTripleLetter',
          'MalusQuadLetter');
var
  uz:TUnzipper;
  i:integer;
begin
  if (aFileName='') or not FileExistsUTF8(Config.Path+aFileName) then
    exit;

  uz:=TUnzipper.Create;
  try
    uz.OutputPath:=GetTempDir;
    uz.FileName:=Config.Path+aFileName;
    uz.UnZipAllFiles;

    if not GridView.MakeCurrent then //make current to avoid that textures get mixed-up when 3d is active
      exit;
    Textures.Clear;
    for i:=0 to 13 do
     if FileExistsUTF8(GetTempDir+'/'+aFieldTypes[i]+'.bmp') then
      Textures.AddFromFile(GetTempDir+'/'+aFieldTypes[i]+'.bmp');
    if FileExists(GetTempDir+'board.obj') then
      Board.LoadFromFile(GetTempDir+'board.obj') else
    if LazarusResources.Find('board')<>nil then
      Board.LoadFromRessource('board');

    if FileExists(GetTempDir+'piece.obj') then
      Piece.LoadFromFile(GetTempDir+'piece.obj') else
    if LazarusResources.Find('piece')<>nil then
      Piece.LoadFromRessource('piece');

    if FileExists(GetTempDir+'letter.obj') then
      Letter.LoadFromFile(GetTempDir+'letter.obj') else
    if LazarusResources.Find('letter')<>nil then
      Letter.LoadFromRessource('letter');

    for i:=0 to uz.Entries.Count-1 do
      DeleteFileUTF8(GetTempDir+uz.Entries[i].DiskFileName);
  finally
    uz.Clear;
    uz.Free;
  end;
end;

procedure TGlScrabbleGrid.CallList(aValue: string);
begin
  case aValue of
   'LetterField':
   begin
     glBegin(GL_QUADS);
      glNormal3f(0.0, 1.0, 0.0);
      glTexCoord2f(0.0,0.0); glVertex3f(0.0, cDist, 0.0);
      glTexCoord2f(1.0,0.0); glVertex3f(1.0-cSpace, cDist, 0.0);
      glTexCoord2f(1.0,1.0); glVertex3f(1.0-cSpace, cDist, 1.0-cSpace);
      glTexCoord2f(0.0,1.0); glVertex3f(0.0, cDist, 1.0-cSpace);
     glEnd;
   end;
   'FieldMarker':
   begin
     if UseVBO then
     begin
       glInterleavedArrays(GL_V3F, sizeOf(TVertex), nil);
       glDrawArrays(GL_TRIANGLES, 0, 4*3);
     end else
       glCallList(Markers);
   end;
  end; //case
end;

procedure TGlScrabbleGrid.SetDesign(AValue: string);
begin
  if FDesign<>aValue then
  begin
    FDesign:=AValue;
    try
      ReloadModelsFrom(FDesign);
    except
      on E: Exception do
        OnMessage(smError,E.Message);
    end;
  end;
end;

constructor TGlScrabbleGrid.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  GridView:=TGlContext.Create(aOwner,[rx,tz]);
  with GridView do
  begin
    Name:='ScrabbleGrid';
    ShowHint:=true;
    LimitRotation:=90;
    ControlStyle:=ControlStyle+[csDisplayDragImage];
    OnInitialize:=@DoInitialize;
    OnPaintObjects:=@DoPaint;
    OnDragDrop:=@DoDragDrop;
    OnDragOver:=@DoDragOver;
    OnMouseDown:=@DoMouseDown;
    OnShowHint:=@DoShowHint;
  end;
  FInitialized:=false;
  Textures:=TTextureList.Create;
  FDesign:='';
  Board:=TWavefront.Create;   //do not load textures before gl initalization
  Piece:=TWavefront.Create;
  Letter:=TLetterObj.Create(self);
end;

destructor TGlScrabbleGrid.Destroy;
begin
  if FInitialized then
  begin
    if UseVBO then
      glDeleteBuffers(1,@Markers) else
      glDeleteLists(Markers,1);
  end;
  Letter.Free;
  Piece.Free;
  Board.Free;
  Textures.Free;
  GridView.Free;
  inherited Destroy;
end;

procedure TGlScrabbleGrid.Paint(Sender:TObject);
begin
  if ([csDestroying,csLoading]*ComponentState=[]) then
    GridView.Invalidate;
end;

procedure TGlScrabbleGrid.RefreshLetters;
begin
  Textures.Clear;  //coordinates
  Letter.Textures.Clear; //letters
  GridView.Invalidate;
end;

function TGlScrabbleGrid.ScreenShot: TBitmap;
begin
  Result:=GridView.ScreenShot;
end;

procedure TGlScrabbleGrid.SetLight(AValue: TLight);
begin
  if not CompareMem(@aValue,@GridView.Light,sizeof(TLight)) then
    GridView.Light:=aValue;
end;

procedure TGlScrabbleGrid.SetMipmapping(AValue: boolean);
begin
  Textures.Mipmapping:=aValue;
  Letter.Textures.Mipmapping:=aValue;
end;

function TGlScrabbleGrid.GetFps: integer;
begin
  Result:=GridView.Fps;
end;

procedure TGlScrabbleGrid.SetRotation(AValue: TPosition);
begin
  GridView.Rotation:=aValue;
end;

procedure TGlScrabbleGrid.SetSticky(AValue: boolean);
begin
  Gridview.PerspectiveByMouse:=aValue;
end;

procedure TGlScrabbleGrid.SetTranslation(AValue: TPosition);
begin
  GridView.Translation:=aValue;
end;

procedure TGlScrabbleGrid.ResetPosition;
var
  p:TPosition;
begin
  p:=GridView.Translation;
  p.X:=0;
  p.Y:=0;
  p.Z:=-28;
  GridView.Translation:=p;
  p:=GridView.Rotation;
  p.X:=90;
  p.Y:=0;
  p.Z:=0;
  GridView.Rotation:=p;
  GridView.Invalidate;
end;

initialization
 {$I board.obj}
 {$I board.mtl}
 {$I piece.obj}
 {$I piece.mtl}
 {$I letter.obj}
 {$I letter.mtl}
end.

