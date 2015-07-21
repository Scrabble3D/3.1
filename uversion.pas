{ Version information

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

unit uversion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  function VersionToString(const aValue: integer): string;
  function StringToVersion(const aValue:string):integer;

const
  cUnknownVersion=-1;
  {$Note Installer: nwVersion}
  cVersion='3.1.3';
  cBuild='2015-Mar-01';
  cDesign=1;

implementation

const
  cMajorPos=1000000;
  cMinorPos=10000;
  cBuildPos=100;

function VersionToString(const aValue: integer): string;
var
  aMajor, aMinor, aBuild: integer;
  temp: integer;
begin
  if aValue=cUnknownVersion then
    Result:='' else
  if aValue<100000 then
    Result:=inttostr(aValue) else
  if aValue<1000000 then //legacy
  begin
    temp:=aValue;
    aMajor:=temp div 100000;
    temp:=temp-aMajor*100000;
    aMinor:=temp div 1000;
    temp:=temp-aMinor*1000;
    aBuild:=temp;
    Result:=inttostr(aMajor)+'.'+inttostr(aMinor)+'.'+inttostr(aBuild);
  end else
  begin
    temp:=aValue;
    aMajor:=temp div cMajorPos;
    temp:=temp-aMajor*cMajorPos;
    aMinor:=temp div cMinorPos;
    temp:=temp-aMinor*cMinorPos;
    aBuild:=temp div cBuildPos;
    temp:=temp-aBuild*cBuildPos;
    Result:=inttostr(aMajor)+'.'+inttostr(aMinor)+'.'+inttostr(aBuild);
    if temp>0 then
      Result:=Result+'-'+inttostr(temp);
  end;
end;

function StringToVersion(const aValue:string):integer;
var
  aMajor, aMinor, aBuild, aPatch: integer;
  i:integer;
  temp:string;
begin
  Result:=cUnknownVersion;
  if aValue<>'' then
  begin
    temp:=aValue;
    //major
    i:=pos('.',temp);
    aMajor:=StrToIntDef(copy(temp,1,i-1),0);
    delete(temp,1,i);
    //minor
    i:=pos('.',temp);
    aMinor:=StrToIntDef(copy(temp,1,i-1),0);
    delete(temp,1,i);
    //build
    i:=pos('-',temp);
    if i=0 then
      i:=length(temp)+1;
    aBuild:=StrToIntDef(copy(temp,1,i-1),0);
    delete(temp,1,i);
    //patch
    if length(temp)>0 then
      aPatch:=strtoint(temp) else
      aPatch:=0;

    Result:=aMajor*cMajorPos+aMinor*cMinorPos+aBuild*cBuildPos+aPatch;
  end;
end;

end.

