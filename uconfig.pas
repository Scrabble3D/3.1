{ Routines to save/load settings

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

unit uconfig;

{$mode objfpc}{$H+}

interface

{$I conditions.inc}

uses
  Classes, SysUtils, Controls, FileUtil, Forms, XMLConf;

type

  { TConfig }

  TConfig=class(TXMLConfig)
  private
    FFilePath,FFileName:string;
    FLastError:string;
    function DoEncode(const s : widestring; ReplaceWhiteChars:boolean=false) : string;
    function DoDecode(const s : string; ReplaceWhiteChars:boolean=false): widestring;
    procedure DoOpen;
  public
    procedure SaveWindowPosition(Sender:TControl);
    procedure ReadWindowPosition(Sender:TControl);

    procedure Write(const aSection:string;const aValue:string);overload;
    procedure Write(const aSection:string;const aValue:integer);overload;
    procedure Write(const aSection:string;const aValue:boolean);overload;
    function Read(const aSection:string;const aDefault:string):string;overload;
    function Read(const aSection:string;const aDefault:integer):integer;overload;
    function Read(const aSection:string;const aDefault:boolean):boolean;overload;

    procedure Delete(const aSection: string);
  public
    constructor Create(AOwner: TComponent); override;
    property Path:string read FFilePath;
    property Name:string read FFileName;
    property LastError:string read FLastError;
  end;

var
  Config:TConfig;

implementation

const
  DefaultExtension='.conf';

{ TConfigFile }

constructor TConfig.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FLastError:='';
  DoOpen;
end;

procedure TConfig.DoOpen;
var
  f: text;
  s: string;
begin
  {$ifdef Portable}
   FFilePath:=ProgramDirectory;//ExtractFilePath(ParamStrUTF8(0));
   {$ifdef Darwin}
    FFilePath:=LeftStr(FFilePath, Pos('Scrabble3D.app',FFilePath)-1); //remove bundle stuff
   {$endif}
  {$else}
  if not FileExistsUTF8(ChangeFileExt(ParamStrUTF8(0),DefaultExtension)) then
  begin
    FFilePath:=GetAppConfigDirUTF8(false{.$ifndef Windows},true{.$endif});
  end else
    FFilePath:=ExtractFilePath(ParamStrUTF8(0));
  {$endif}
  FFileName:=ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),DefaultExtension);
  //error if xml-file is emtpy, ie. has no root node
  if FileExistsUtf8(FFilePath+FFileName) then
  begin
    AssignFile(f,UTF8ToSys(FFilePath+FFileName));
    Reset(f);
    Readln(f,s);
    CloseFile(f);
    if s<>'<?xml version="1.0" encoding="utf-8"?>' then
    begin
      if FileExistsUTF8(UTF8ToSys(FFilePath+FFileName)+'.bak') then
        DeleteFileUTF8(UTF8ToSys(FFilePath+FFileName)+'.bak');
      RenameFileUTF8(UTF8ToSys(FFilePath+FFileName),UTF8ToSys(FFilePath+FFileName)+'.bak');
      FLastError:='The configuration file '+FFilePath+FFileName+' was not readable. The file has been renamed to *.bak and a new configuration file created.';
    end;
  end;
  try
    Filename:=UTF8ToSys(FFilePath+FFileName);
  except
    on E:Exception do FLastError:='Error: '+E.Message;
  end;
end;

function TConfig.DoEncode(const s: widestring;ReplaceWhiteChars:boolean=false): string;
var
  i:integer;
  ws:widestring;
begin
  if ReplaceWhiteChars then
//    Result:=UTF8Encode(DecodeTriplet(s, '_')) else
  begin
    i:=1;
    ws:='';
    while i<length(s) do
    begin
      if s[i] in ['a'..'z','A'..'Z','/','0'..'9',':'] then
      begin
        ws:=ws+s[i];
        inc(i,1);
      end else
      if copy(s,i,3)='_20' then //legacy
        ws:=ws+' ' else
      begin
        ws:=ws+chr(strtoint(copy(s,i+1,3)));
        inc(i,4);
      end;
    end;
    Result:=UTF8Encode(ws);
  end else
    Result:=UTF8Encode(s);
end;

function TConfig.DoDecode(const s: string;ReplaceWhiteChars:boolean=false): widestring;
var
  i:integer;
  ws:widestring;
begin
  //no leading number, space, or % allowed in xml - replace ' ' by _20
  if ReplaceWhiteChars then
//    Result:=UTF8Decode(EncodeTriplet(s,'_',[' '])) else
  begin
    ws:=UTF8Decode(s);
    Result:='';
    for i:=1 to length(ws) do
     if ws[i] in ['a'..'z','A'..'Z','/','0'..'9','_',':'] then
      Result:=Result+ws[i] else
      Result:=Result+'_'+Format('%.3d',[ord(ws[i])]);
  end else
    Result:=UTF8Decode(s);
end;

procedure TConfig.SaveWindowPosition(Sender: TControl);
begin
  with Sender do
  begin
    Write('General/Position/'+Name+'/Left',Left);
    Write('General/Position/'+Name+'/Top',Top);
    Write('General/Position/'+Name+'/Width',Width);
    Write('General/Position/'+Name+'/Height',Height);
  end;
end;

procedure TConfig.ReadWindowPosition(Sender: TControl);
var
  x,y,w,h:integer;
begin
  with Sender do
  begin
    x:=Read('General/Position/'+Name+'/Left',Left);
    y:=Read('General/Position/'+Name+'/Top',Top);
    w:=Read('General/Position/'+Name+'/Width',Width);
    h:=Read('General/Position/'+Name+'/Height',Height);
  end;
  //if monitor settings have been changed
  if x<Screen.DesktopLeft then x:=Screen.DesktopLeft;
  if y<Screen.DesktopTop then y:=Screen.DesktopTop;
  if w>Screen.DesktopWidth then w:=Screen.DesktopWidth ;
  if h>Screen.DesktopHeight then h:=Screen.DesktopHeight;
  if x>Screen.DesktopWidth-w then x:=Screen.DesktopWidth-w;
  if y>Screen.DesktopHeight-h then y:=Screen.DesktopHeight-h;
  //apply
  Sender.SetBounds(x,y,w,h);
  Application.ProcessMessages; //apply
end;

procedure TConfig.Write(const aSection: string; const aValue: string);
begin
  SetValue(DoDecode(aSection,true),DoDecode(aValue));
end;

procedure TConfig.Write(const aSection: string; const aValue: integer);
begin
  SetValue(DoDecode(aSection,true),aValue);
end;

procedure TConfig.Write(const aSection: string; const aValue: boolean);
begin
  SetValue(DoDecode(aSection,true),aValue);
end;

function TConfig.Read(const aSection: string; const aDefault: string): string;
begin
  if aSection[length(aSection)]='/' then raise EInOutError.Create(aSection+' contains /');
  Result:=DoEncode(GetValue(DoDecode(aSection,true),DoDecode(aDefault)));
end;

function TConfig.Read(const aSection: string; const aDefault: integer): integer;
begin
  if aSection[length(aSection)]='/' then raise EInOutError.Create(aSection+' contains /');
  Result:=GetValue(DoDecode(aSection,true),aDefault);
end;

function TConfig.Read(const aSection: string; const aDefault: boolean): boolean;
begin
  if aSection[length(aSection)]='/' then raise EInOutError.Create(aSection+' contains /');
  Result:=GetValue(DoDecode(aSection,true),aDefault);
end;

procedure TConfig.Delete(const aSection: string);
begin
  DeletePath(DoDecode(aSection,true));
end;

initialization
  Config:=TConfig.Create(nil);
finalization
  Config.Flush;
  Config.Free;
end.

