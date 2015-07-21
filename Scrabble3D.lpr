{ Scrabble3D

  v3.1.3; 2015-Mar-01
  Copyleft (c) GPLv3: 1996-2015 Heiko Tietze heiko_tietze@web.de

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

program Scrabble3D;

{$mode objfpc}
{$H+}

{$I conditions.inc}

{$ifdef Debug}
 {$Warning Heaptrace}
 {$define Heaptrace}
{$endif}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  cmem,     //c memory manager increases calc speed significantly
  {$ifdef Heaptrace}
  heaptrc, //heaptrc after cmem!
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazopenglcontext, umain, ugameoptions, uwordsearch,
  unewgame, uabout, unetwork, uremotegames, ustatistics, uwelcome;

 {$R Scrabble3D.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmWordSearch, fmWordSearch);
  Application.CreateForm(TfmNewGame, fmNewGame);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.CreateForm(TfmNetwork, fmNetwork);
  Application.CreateForm(TfmRemote, fmRemote);
  Application.CreateForm(TfmWelcome, fmWelcome);
  Application.CreateForm(TfmStatistics, fmStatistics);
  Application.CreateForm(TfmGameOptions, fmGameOptions);
  Application.Run;
end.

