{ Random number generator with unique sequence (seed and standard procs is not safe)

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

unit urandom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function GenRandom(aLimit:longint=-1): longint;
procedure GenRandomize;

const Seed : integer = 42;

implementation

uses LCLIntf;

const
  IM = 139968;
  IA = 3877;
  IC = 29573;

//http://shootout.alioth.debian.org/gp4/benchmark.php?test=fasta&lang=fpascal&id=2
function GenRandom(aLimit : longint): longint;
begin
  Seed:=(Seed * IA + IC) mod IM;
  if aLimit>0 then
    Result:=Seed mod aLimit else
    Result:=Seed;
end;

procedure GenRandomize;
begin
  Seed:=GetTickCount mod IM;
end;

end.

