{ Wavefront Object Loader

  v3.1.3; 2015-Mar-01
  Copyleft (C) GPLv3: 1996-2015 Heiko Tietze heiko_tietze@web.de

  based on:
  //------------------------------------------------------------------------
  //
  // Author      : Jan Horn
  // Email       : jhorn@global.co.za
  // Website     : http://home.global.co.za/~jhorn
  // Date        : 13 May 2001
  // Version     : 1.0
  // Description : Wavefront OBJ loader
  //
  //------------------------------------------------------------------------

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

  http://www.fileformat.info/format/wavefrontobj/
  http://www.fileformat.info/format/material/
  http://www.martinreddy.net/gfx/3d/OBJ.spec
}

unit uwavefront;

{$mode objfpc}
{$H+}

interface

uses //OpenGL,
 gl, utypes, utexture,
 classes, sysutils, strutils, fgl, lresources;

type

  TRGBColor = record
    R,G,B : glFloat;
  end;
  TCoord = record
    X,Y,Z : glFloat;
  end;
  TTexCoord = record
    U,V : glFloat;
  end;

  { TMaterial }

  TMaterial = class        // Material Structure
    Name      : string;
    Ambient   : TRGBColor;
    Diffuse   : TRGBColor;
    Specular  : TRGBColor;
    Emissive  : TRGBColor;
    Shininess : glFloat;
    Opacity   : glFloat;
    FileName  : string;
    TextureName : string;
  end;
  TMaterialList = specialize TFPGList<TMaterial>;

  { TFace }

  TFace = class
    VerticeCount : Integer;     // Number of vertices in faces
    vIndex : array of integer;  // indexes to vertices
    tIndex : array of integer;  // indexes to vertex textures
    nIndex : array of integer;  // indexes to vertex normals
    destructor Destroy; override;
  end;
  TFaceList = specialize TFPGList<TFace>;

  { TGroup }

  TGroup = class
    Name      : string;
    Faces     : TFaceList;      // The faces in the group
    Material  : TMaterial;      // index only! to Material; mats are stored in matlist
    destructor Destroy; override;
  end;
  TGroupList = specialize TFPGList<TGroup>;

  { TWaveFront }

  TWaveFront=class
    private
      NumberOfNormals   : Integer;
      NumberOfTexCoords : Integer;
      NumberOfVertices  : Integer;
      Vertex    : array of TCoord;
      Normal    : array of TCoord;
      TexCoord  : array of TTexCoord;
      Groups     : TGroupList;
      FName  : string;
      function GetCoords(s : string) : TCoord;
      function GetTexCoords(s : String) : TTexCoord;
      procedure AddGroup(s : string);
      procedure ReadVertexData(s : string);
      procedure ReadFaceData(const s : string);
      procedure GetMaterialName(s : string);
      procedure LoadMaterials(const aMatName : string; const FromRes:boolean);
      function StringToValue(aValue:string;const aStart:byte):double;
      procedure GetMaterial(S : String);
    protected
      Materials  : TMaterialList;
      Textures : TTextureList;
      procedure LoadModelData(const aStream:TStrings; const FromRes:boolean=false);
      procedure ResetModel;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Draw;
      procedure LoadFromRessource(const aResName:string);
      procedure LoadFromFile(const aFileName:string);
    end;

implementation

{ TGroup }

destructor TGroup.Destroy;
var
  i:integer;
begin
  for i:=0 to Faces.Count-1 do
    Faces[i].Free;
  Faces.Free;
  inherited Destroy;
end;

{ TFace }

destructor TFace.Destroy;
begin
  setlength(vIndex,0);
  setlength(tIndex,0);
  setlength(nIndex,0);
  inherited Destroy;
end;

function TWaveFront.GetCoords(s : string) : TCoord;
begin
  with TStringList.Create do
  try
    Delimiter:=' ';
    StrictDelimiter:=true;
    DelimitedText:=s;

    Result.X :=StrToFloat(StringReplace(Strings[0], '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]));
    Result.Y :=StrToFloat(StringReplace(Strings[1], '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]));
    Result.Z :=StrToFloat(StringReplace(Strings[2], '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]));
  finally
    Free;
  end;
end;

function TWaveFront.GetTexCoords(s : String) : TTexCoord;
begin
  with TStringList.Create do
  try
    Delimiter:=' ';
    StrictDelimiter:=true;
    DelimitedText:=s;
    Result.U :=StrToFloat(StringReplace(Strings[0], '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]));
    Result.V :=StrToFloat(StringReplace(Strings[1], '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]));
  finally
    Free;
  end;
end;

procedure TWaveFront.AddGroup(s: string);
var
  aGroup:TGroup;
begin
  aGroup:=TGroup.Create;
  aGroup.Name:=Trim(Copy(s, 2, length(s)));
  aGroup.Faces:=TFaceList.Create;
  Groups.Add(aGroup);
end;

procedure TWaveFront.ReadVertexData(s : string);
begin
  case s[2] of
    ' ' : begin                      // Read the vertex coords
            inc(NumberOfVertices);
            setlength(Vertex, NumberOfVertices);
            Vertex[NumberOfVertices-1]:=GetCoords(Trim(Copy(s, 2, Length(s))));
          end;
    'N' : begin                      // Read the vertex normals
            inc(NumberOfNormals);
            setlength(Normal, NumberOfNormals);
            Normal[NumberOfNormals-1]:=GetCoords(Trim(Copy(s, 3, Length(s))));
          end;
    'T' : begin                      // Read the vertex texture coords
            inc(NumberOfTexCoords);
            setlength(TexCoord, NumberOfTexCoords);
            TexCoord[NumberOfTexCoords-1]:=GetTexCoords(Trim(Copy(s, 3, Length(s))));
          end;
  end;
end;

procedure TWaveFront.ReadFaceData(const s : string);
var
  i : Integer;
  aFace : TFace;
  sl : TStringList;
begin
  if Groups.Count=0 then
    AddGroup('dummy');
  with TStringList.Create do
  try
    //cut leading 'f ' from input
    CommaText:=Trim(Copy(s, pos(' ', s)+1, length(s)));
    if Count=0 then exit;

    aFace:=TFace.Create;

    for i:=0 to Count-1 do
    begin
      sl:=TStringList.Create;
      try
        sl.Delimiter:='/';
        sl.StrictDelimiter:=true;
        sl.DelimitedText:=Strings[i];
//        if (sl.Count>1) and (sl[1]='') then sl.delete(1); //remove nil vector like f//f

        inc(aFace.VerticeCount);
        SetLength(aFace.vIndex, aFace.VerticeCount);
        SetLength(aFace.nIndex, aFace.VerticeCount);
        SetLength(aFace.tIndex, aFace.VerticeCount);

        if (sl.Count>0) and (sl[0]<>'') then
          aFace.vIndex[aFace.VerticeCount-1]:=StrToInt(sl[0]) else
          aFace.vIndex[aFace.VerticeCount-1]:=-1;

        if (sl.Count>1) and (sl[1]<>'') then
          aFace.tIndex[aFace.VerticeCount-1]:=StrToInt(sl[1]) else
          aFace.tIndex[aFace.VerticeCount-1]:=-1;

        if (sl.Count>2) and (sl[2]<>'') then
          aFace.nIndex[aFace.VerticeCount-1]:=StrToInt(sl[2]) else
          aFace.nIndex[aFace.VerticeCount-1]:=-1;
      finally
        sl.Free;
      end;
    end;//for/to tsl.count
    Groups.Last.Faces.Add(aFace);
  finally
    Free; //TStringList
  end;
end;

procedure TWaveFront.GetMaterialName(s : string);
var
  P,i : Integer;
begin
  if copy(S, 1, 6) <> 'USEMTL' then //exit;  // false call
    OnMessage(smError,'False material call : ' + s);
  P :=Pos(' ', S);
  S :=Copy(S, P+1, length(S));

  if Groups.Count=0 then
    AddGroup('dummy');

//  if s<>'NONE' then //Blender
  for i:=0 to Materials.Count-1 do
   if Materials[i].Name=s then
    Groups.Last.Material:=Materials[i];
end;

function TWaveFront.StringToValue(aValue:string;const aStart:byte):double;
var
  s:string;
begin
  s:=Trim(copy(aValue,aStart,length(aValue)));
  s:=StringReplace(s,'.',DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]);
  Result:=StrToFloat(s);
end;

procedure TWaveFront.GetMaterial(S : String);
var
  c : TRGBColor;
  p, p2 : Integer;
  ch : Char;
begin
  ch:= s[2];
  S :=Trim(Copy(S, 3, Length(S)));
  P :=Pos(' ', S);
  P2 :=PosEx(' ', S, P+1);
  S := StringReplace(S, '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]);

  C.R :=StrToFloat(Copy(S, 1, P-1));
  C.G :=StrToFloat(Copy(S, P+1, P2-P-1));
  C.B :=StrToFloat(Copy(S, P2+1, Length(S)));
  case CH of
    'A' : Materials.Last.Ambient :=C;
    'D' : Materials.Last.Diffuse :=C;
    'S' : Materials.Last.Specular :=C;
    'E' : Materials.Last.Emissive :=C;
  end;
end;

procedure TWaveFront.LoadMaterials(const aMatName : string; const FromRes:boolean);
var
  i:integer;
  s:string;
  sl:TStringList;
  st: TLazarusResourceStream;
begin
  sl:=TStringList.Create;
  try
    //load material data
    if FromRes then
    begin
      s:=lowercase(copy(aMatName, pos(' ', aMatName)+1, length(aMatName)));//MTLLIB BOARD.MTL
      s:=copy(s,1,length(s)-4);//.mtl
      if LazarusResources.Find(s)<>nil then
      begin
        st:=TLazarusResourceStream.Create(ExtractFileName(s), 'MTL');
        try
          sl.LoadFromStream(st);
        finally
          st.Free;
        end;
      end else
        OnMessage(smError,'Resource not found ('+s+')');
    end else
    begin
      s:=lowercase(copy(aMatName, pos(' ', aMatName)+1, length(aMatName)));
      s:=ExtractFilePath(FName)+s;
      if FileExists(s) then
        sl.LoadFromFile(s) else
        OnMessage(smError,'Material file not found ('+s+')');
    end;
    //parse material data
    for i:=0 to sl.Count-1 do
    begin
      if (sl[i]<>'') and (sl[i][1]<>'#') then
      begin
        s:=Uppercase(sl[i]);
        case s[1] of
          'N' : begin
                  if s[2] = 'E' then
                  begin
                    if copy(s, 1, 6) <> 'NEWMTL' then
                      OnMessage(smError,'False material call: ' + s);
                    Materials.Add(TMaterial.Create);
                    Materials.Last.TextureName:='';
                    Materials.Last.Name:=Trim(copy(s,7,length(s)));
                  end;
                  if s[2] = 'S' then
                    Materials.Last.Shininess:=StringToValue(s,3);
                end;
          'D' : Materials.Last.Opacity:=StringToValue(s,2);
          'I' : ; { TODO 4 : Wavefront: add illumination for material
          0		Color on and Ambient off
          1		Color on and Ambient on
          2		Highlight on
          3		Reflection on and Ray trace on
          4		Transparency: Glass on
          		Reflection: Ray trace on
          5		Reflection: Fresnel on and Ray trace on
          6		Transparency: Refraction on
          		Reflection: Fresnel off and Ray trace on
          7		Transparency: Refraction on
          		Reflection: Fresnel on and Ray trace on
          8		Reflection on and Ray trace off
          9		Transparency: Glass on
          		Reflection: Ray trace off
          10		Casts shadows onto invisible surfaces

          }
          'K' : GetMaterial(s);
          'M' : begin
                  Materials.Last.FileName:=lowercase(copy(s, Pos(' ',s)+1, length(s)));
                  if FromRes and (LazarusResources.Find(Materials.Last.FileName)<>nil) then
                    Textures.AddFromRessource(Materials.Last.FileName) else
                  if not FromRes and FileExists(ExtractFilePath(FName)+Materials.Last.FileName) then
                    Textures.AddFromFile(ExtractFilePath(FName)+Materials.Last.FileName) else
                  begin
                    OnMessage(smError,'Material texture not found ('+Materials.Last.FileName+')');
                    Materials.Last.FileName:='';
                  end;
                  Materials.Last.TextureName:=Materials.Last.FileName;
                end;
        end;//case
      end;//if not comment
    end;//for to count
  finally
    sl.Free;
  end;
end;

{ TWaveFront }

procedure TWaveFront.ResetModel;
var
  i:integer;
begin
  NumberOfNormals:=0;
  NumberOfTexCoords:=0;
  NumberOfVertices:=0;
  SetLength(Vertex, NumberOfVertices);
  SetLength(Normal, NumberOfNormals);
  SetLength(TexCoord, NumberOfTexCoords);
  for i:=0 to Groups.Count-1 do
    Groups[i].Free;
  Groups.Clear;
  for i:=0 to Materials.Count-1 do
    Materials[i].Free;
  Materials.Clear;
  Textures.Clear;
end;

constructor TWaveFront.Create;
begin
  inherited Create;
  Materials:=TMaterialList.Create;
  Groups:=TGroupList.Create;
  Textures:=TTextureList.Create;
//  LoadDefault;
end;

destructor TWaveFront.Destroy;
begin
  ResetModel;
  Textures.Free;
  Materials.Free;
  Groups.Free;
  inherited Destroy;
end;

procedure TWaveFront.LoadModelData(const aStream: TStrings; const FromRes:boolean=false);
var
  s:string;
  i:integer;
begin
  ResetModel;
  for i:=0 to aStream.Count-1 do
  begin
    s:=UpperCase(aStream[i]);
    if (s<>'') then
    case s[1] of
     '#' : ;//comment
     'G' : AddGroup(s);
     'V' : ReadVertexData(s);
     'F' : ReadFaceData(s);
     'U' : GetMaterialName(s);
     'M' : if (copy(s,1,6)='MTLLIB') then
             LoadMaterials(s,FromRes) else
             OnMessage(smError,'False material call : ' + s);
    end;//case
  end;//for to
end;

procedure TWaveFront.Draw;
var
  i,j,k : integer;
  aFace : TFace;
begin
  for i:=0 to Groups.Count-1 do
  begin
    if Groups[i].Material<>nil then
    begin
      glMaterialfv(GL_FRONT, GL_DIFFUSE, @Groups[i].Material.Diffuse);
      glMaterialfv(GL_FRONT, GL_SPECULAR, @Groups[i].Material.Specular);
      glMaterialfv(GL_FRONT, GL_AMBIENT, @Groups[i].Material.Ambient);
      glMaterialfv(GL_FRONT, GL_EMISSION, @Groups[i].Material.Emissive);
      glMaterialfv(GL_FRONT, GL_SHININESS, @Groups[i].Material.Shininess);
      if Groups[i].Material.Opacity<1.0 then
      begin
        glEnable(GL_BLEND);
        glBlendFunc(GL_DST_COLOR, GL_ZERO);
//        glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE_MINUS_SRC_COLOR);
//        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end else
        glDisable(GL_BLEND);
      if Groups[i].Material.TextureName<>'' then
      begin
        j:=Textures.IndexOfName(Groups[i].Material.TextureName);
        if j>-1 then
          glBindTexture(GL_TEXTURE_2D,Textures[j]);
        glEnable(GL_TEXTURE_2D);
      end else
        glDisable(GL_TEXTURE_2D);
    end;

    for j:=0 to Groups[i].Faces.Count-1 do
    begin
      aFace:=Groups[i].Faces[j];
      with aFace do
      begin
        case VerticeCount of
          3 : glBegin(GL_TRIANGLES);
          4 : glBegin(GL_QUADS);
        else
          glBegin(GL_POLYGON);
        end; //case

        for k:=0 to VerticeCount-1 do
        begin
          if nIndex[k]>-1 then
            glNormal3fv(@Normal[nIndex[k]-1]);
          if tIndex[k]>1-1 then
            glTexCoord2fv(@TexCoord[tIndex[k]-1]);
          if vIndex[k]>-1 then
            glVertex3fv(@Vertex[vIndex[k]-1]);
        end;

        glEnd();
      end;//with Face
    end; //for to Faces
  end;//for to groups
end;

procedure TWaveFront.LoadFromRessource(const aResName: string);
var
  sl:TStringList;
  st: TLazarusResourceStream;
begin
//  if FObjName<>aResName then
  begin
    FName:=aResName;
    sl:=TStringList.Create;
    try
      st:=TLazarusResourceStream.Create(FName, 'OBJ');
      try
        sl.LoadFromStream(st);
        LoadModelData(sl,true);
      finally
        st.Free;
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TWaveFront.LoadFromFile(const aFileName: string);
var
  sl:TStringList;
begin
//  if FObjName<>aFileName then
  begin
    FName:=aFileName;
    sl:=TStringList.Create;
    try
      sl.LoadFromFile(FName);
      LoadModelData(sl);
    finally
      sl.Free;
    end;
  end;
end;

end.
