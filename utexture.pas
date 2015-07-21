{ Helper routines for OpenGL textures

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

unit utexture;

{$mode objfpc}{$H+}

interface

{.$define fgl} //works but isn't faster

uses
  Classes, SysUtils, Graphics,
  IntfGraphics, //TLazIntfImage @ bmp2raw
  Stringhashlist, {$ifdef fgl}fgl,{$endif}
  LResources,
  LCLIntf, LCLProc, LCLType, types, //drawtext
  FPImage, //TFPColor @ BmpToRaw
  Fileutil, //FileExistsUTF8
  gl, glext;

type

  { TTextureList }
  TTexture=class
    private
      FTexture: gluint;
      FName: string;
      FMipmapCount: byte;
    protected
      property Texture:gluint read FTexture;
      property Name:string read FName;
      property Mipmaps:byte read FMipmapCount write FMipmapCount;
    public
      constructor Create(aName:string; aSize:word; aItemData:Pointer; const Mipmap:boolean);
      destructor Destroy; override;
    end;
  {$ifdef fgl}TTextures = specialize TFPGList<TTexture>;{$endif}

  TTextureList={$ifdef fgl}class(TTextures){$else}class(TList){$endif}
    private
      FMipmapping: boolean;
      function GetByIndex(aIndex: longword): gluint;
      function BmpToRaw(const aBitmap: TBitmap): PByte;
      function AddTexture(const aName: String; const aSize: word; aItemData: Pointer): Integer;
      procedure SetMipmapping(AValue: boolean);
      function IsPowerOfTwo(const aValue: longword): boolean;
    public
      function AddFromRessource(const aResName:string):integer;
      function AddFromFile(const aFileName:string):integer;
      function AddFromBitmap(const aName:string; const aBmp:TBitmap):integer;
      function AddFromString(const aValue:string; const aPercent:integer=-1; aID:string=''):integer;
      procedure Clear; {$ifndef fgl}override;{$endif}
    public
      constructor Create;
      destructor Destroy; override;
      function IndexOfName(const aName:string):integer;
      property ByIndex[aIndex:longword]:gluint read GetByIndex; default;
      property Mipmapping: boolean read FMipmapping write SetMipmapping;
    end;

  ETextureError=class(Exception);

const
  cTextureAlpha = GL_RGB;//GL_RGBA
  //set level of detail here, e.g.
  //cTextureSize : array[0..2] of byte = (128,64,32);
  //but internal generation of mipmaps has good quality and is much faster
  //in case of length(cTextureSize); see AddTexture()
  cTextureSize : array[0..0] of byte = (64);

implementation

{ TTexture }

function SortByName(Item1, Item2: Pointer): Integer;
begin
  Result:=CompareText(TTexture(Item1).Name,TTexture(Item2).Name);
end;

constructor TTexture.Create(aName:string; aSize:word; aItemData:Pointer; const Mipmap:boolean);
var
  maxani: integer;
begin
  inherited Create;
  FName:=aName;
  FMipmapCount:=1;
  glGenTextures(1, @FTexture);
  glBindTexture(GL_TEXTURE_2D, FTexture);

  //fallback filter
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, cTextureAlpha, aSize, aSize, 0, cTextureAlpha, GL_UNSIGNED_BYTE, aItemData);

  //mipmapping
  if Mipmap then
  begin
    glGenerateMipmap(GL_TEXTURE_2D);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
  end;

  //antialiasing
  glGetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @maxani);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAX_ANISOTROPY_EXT, maxani);
end;

destructor TTexture.Destroy;
begin
  glDeleteTextures(1, @FTexture);
  inherited;
end;

{ TGLTextures }

function TTextureList.GetByIndex(aIndex: longword): gluint;
begin
  if aIndex<Count then
    Result:=TTexture(Items[aIndex]).Texture;
end;

function TTextureList.IndexOfName(const aName: string): integer;
var
  i,j,p,z: integer;
begin
  i:=0;
  j:=self.Count-1;
  while i<=j do
  begin
    z:=(i+j) shr 1;
    p:=CompareText(TTexture(Items[z]).Name,aName);
    if p=0 then
      exit(z); //found
    if p<0 then
      i:=z+1 else
      j:=z-1;
  end;
  Result:=-1; //not found
end;

function TTextureList.BmpToRaw(const aBitmap: TBitmap): PByte;
type
  TRawImage = packed record
     p:array[0..0] of byte;
   end;
  PRawImage = ^TRawImage;
var
  aLazIntfImage : TLazIntfImage;
  x,y,i,z       : integer;
  aColor        : TFPColor;
begin
  //convert bitmap
  if cTextureAlpha=GL_RGB then
    z:=3 else {%H-}z:=4;
  aLazIntfImage:=aBitmap.CreateIntfImage;
  try
    GetMem(Result,aBitmap.Width*aBitmap.Height*z);
    i:=0;
    for x:=0 to aBitmap.Width-1 do
     for y:=0 to aBitmap.Height-1 do
     begin
       {$RANGECHECKS OFF}
       with PRawImage(Result)^ do
       begin
         aColor:=aLazIntfImage.Colors[y,x];
         p[i+0]:=aColor.red div 256;//y*aTexture.Height+x*3
         p[i+1]:=aColor.green div 256;//(aColor shr 8) and $FF;
         p[i+2]:=aColor.blue div 256;//((aColor shr 16) and $FF);
         if z=4 then
           p[i+3]:=1;
         inc(i,z);
       end;
       {$RANGECHECKS ON}
     end;
  finally
    aLazIntfImage.Free;
  end;
end;

function TTextureList.AddFromRessource(const aResName: string):integer;
var
  aBitmap: TBitmap;
  aBmpData: PByte;
begin
  Result:=IndexOfName(aResName);
  if Result=-1 then
  begin
    if LazarusResources.Find(aResName)=nil then
      raise ETextureError.Create('Texture ressource not found ('+aResName+')');

    aBitmap:=TBitmap.Create;
    try
      aBitmap.PixelFormat:=pf24bit;
      aBitmap.Canvas.Brush.Color:=clBlack;//White;
      aBitmap.Canvas.Brush.Style:=bsSolid;
      with TPicture.Create do
      try
        Jpeg.LoadFromLazarusResource(aResName);
        aBitmap.Width:=Width;
        aBitmap.Height:=Height;
        aBitmap.Canvas.Draw(0,0,Bitmap);
      finally
        Free;//TPicture
      end;
      aBmpData:=BmpToRaw(aBitmap);
      try
        Result:=AddTexture(aResName,aBitmap.Width,aBmpData);
      finally
        FreeMem(aBmpData);
      end;
    finally
      aBitmap.Free;
    end;
  end;
end;

function TTextureList.AddFromFile(const aFileName: string):integer;

var
  aBitmap: TBitmap;
  aBmpData: PByte;
  aMaxTexSize:integer;
  s:string;
begin
  Result:=IndexOfName(ChangeFileExt(ExtractFileName(aFileName),''));
  if Result=-1 then
  begin
    if not FileExistsUTF8(aFileName) then //{$ifdef Linux} or (FileGetAttrUTF8(aFileName)<>faDirectory) {$endif} then
      raise ETextureError.Create('Texture file not found ('+aFileName+')');

    aBitmap:=TBitmap.Create;
    try
      aBitmap.PixelFormat:=pf24bit;
      aBitmap.Canvas.Brush.Color:=clBlack;//White;
      aBitmap.Canvas.Brush.Style:=bsSolid;
      with TPicture.Create do
      try
        LoadFromFile(aFileName);
        aBitmap.Width:=Width;
        aBitmap.Height:=Height;
        if aBitmap.Width<>aBitmap.Height then
          raise ETextureError.Create('Width and Height not equal');
        glGetIntegerv(GL_MAX_TEXTURE_SIZE,@aMaxTexSize);
        if aMaxTexSize<aBitmap.Width then
          raise ETextureError.Create('Texture size of '+inttostr(aBitmap.Width)+'px exceeds your hardware capabilities ('+inttostr(aMaxTexSize)+')');
{        if not IsPowerOfTwo(aBitmap.Width) then and not Load_GL_ARB_texture_non_power_of_two then
          raise ETextureError.Create('Graphics does not support NPOT graphics');
}
        aBitmap.Canvas.Draw(0,0,Bitmap);
      finally
        Free;//TPicture
      end;
      s:=ExtractFileName(aFileName);
//      s:=ChangeFileExt(s,'');
      aBmpData:=BmpToRaw(aBitmap);
      try
        Result:=AddTexture(s,aBitmap.Width,aBmpData);
      finally
        FreeMem(aBmpData);
      end;
    finally
      aBitmap.Free;
    end
  end;
end;

function TTextureList.AddFromBitmap(const aName:string; const aBmp: TBitmap):integer;
var
  aBmpData: PByte;
begin
  aBmpData:=BmpToRaw(aBmp);
  try
    Result:=AddTexture(aName,aBmp.Width,aBmpData);
  finally
    FreeMem(aBmpData);
  end;
end;

function TTextureList.AddFromString(const aValue: string; const aPercent:integer; aID:string=''): integer;
var
  aBitmap: TBitmap;
  aBmpData: PByte;
  z:TSize;//integer;
  aRect:TRect;
  i:integer;
begin
  if aID='' then
    aID:=aValue;
  Result:=IndexOfName(aID);
  if Result=-1 then
  for i:=0 to length(cTextureSize)-1 do
  begin
    aBitmap:=TBitmap.Create;
    try
      aBitmap.PixelFormat:=pf24bit;
      aBitmap.Canvas.Brush.Color:=clWhite;//Black;
      aBitmap.Canvas.Brush.Style:=bsSolid;
      aBitmap.TransparentColor:=clWhite;
      aBitmap.Transparent:=true;
      aBitmap.Width:=cTextureSize[i];
      aBitmap.Height:=cTextureSize[i];
      aBitmap.Canvas.FillRect(0,0,cTextureSize[i],cTextureSize[i]);

      aBitmap.Canvas.Font.Size:=cTextureSize[i] div 5; //128=24, 64=12
      aBitmap.Canvas.Font.Name:='Arial';
      aBitmap.Canvas.Font.Color:=clBlack;//White;

      if aPercent>0 then
      repeat
        aBitmap.Canvas.Font.Size:=aBitmap.Canvas.Font.Size+1;
        z:=aBitmap.Canvas.TextExtent(aValue);
      until ((z.cx>cTextureSize[i]*(aPercent/100)) or (z.cy>cTextureSize[i]*(aPercent/100)));
      aBitmap.Canvas.Font.Size:=aBitmap.Canvas.Font.Size-1;
      aRect:=aBitmap.Canvas.ClipRect;
      if aPercent>0 then
      DrawText(aBitmap.Canvas.Handle,PChar(aValue),-1,aRect,
                  DT_CENTER or DT_SINGLELINE or DT_VCENTER) else
      DrawText(aBitmap.Canvas.Handle,PChar(aValue),-1,aRect,
                  DT_TOP or DT_WORDBREAK or DT_LEFT);
      aBmpData:=BmpToRaw(aBitmap);
      try
        Result:=AddTexture(aID,aBitmap.Width,aBmpData);
      finally
        FreeMem(aBmpData);
      end;
    finally
      aBitmap.Free;
    end
  end;
end;

function TTextureList.AddTexture(const aName: String; const aSize: word; aItemData: Pointer): Integer;
var
  i: integer;
  aTexture: TTexture;
begin
  if (length(cTextureSize)>1) then//internal generation of mipmaps has good quality and is much faster
    {%H-}i:=IndexOfName(aName) else
    i:=-1;
  if i>-1 then
  with TTexture(Items[i]) do
  begin
    if FMipmapping then
    begin
      glBindTexture(GL_TEXTURE_2D, FTexture);
      glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
      glTexImage2D(GL_TEXTURE_2D, Mipmaps, cTextureAlpha, aSize, aSize, 0, cTextureAlpha, GL_UNSIGNED_BYTE, aItemData);
      Mipmaps:=Mipmaps+1;
    end;
    Result:=i;
  end else
  begin
    aTexture:=TTexture.Create(aName,aSize,aItemData,FMipmapping);
    Result:=Add(aTexture);
    {$ifndef fgl}Sort(@SortByName);{$endif}//speed up search
  end;
end;

procedure TTextureList.SetMipmapping(aValue: boolean);
begin
  if FMipmapping<>aValue then
  begin
    FMipmapping:=aValue;
    Clear;
  end;
end;

function TTextureList.IsPowerOfTwo(const aValue: longword): boolean;
begin
  Result:=(aValue>0) and (aValue and Pred(aValue)=0);
end;

procedure TTextureList.Clear;
var
  i:integer;
begin
  for i:=Count-1 downto 0 do
    TTexture(Items[i]).Free;
  inherited Clear;
end;

constructor TTextureList.Create;
begin
  inherited Create;
end;

destructor TTextureList.Destroy;
begin
  self.Clear;
  inherited;
end;

end.

