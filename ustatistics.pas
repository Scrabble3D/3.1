{ Chart with statistics

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

unit ustatistics;

{$mode objfpc}{$H+}

{$I conditions.inc}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, LResources, LCLType,
  Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls;

type

  TBestMoveType=(bmtNumberOfMoves,bmtNumberOfGames,bmtBestMoveValue,bmtBestGameValue,
                 bmtSumMoveValue,bmtSumGameValue,bmtSumTime);
  TBestMoveSender=(bmsHuman,bmsComputer);

  { TfmStatistics }

  TfmStatistics = class(TForm)
    Bars: TBarSeries;
    chRating: TChart;
    Lines: TConstantLine;
    lvStatistics: TListView;
    Splitter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    FStatistics:array[TBestMoveType,TBestMoveSender] of integer;
    procedure UpdateListView;
  public
    procedure AddValues(const aValue:integer; const aTime:word; const aSender:string; const IsGameStat:boolean=false);
    procedure UpdateLanguage;
  end;

var
  fmStatistics: TfmStatistics;

implementation

uses
  utypes, uconfig, ulanguage;

{ TfmStatistics }

procedure TfmStatistics.FormCreate(Sender: TObject);
begin
  inherited;
  Config.ReadWindowPosition(self);
  with Config do
  begin
    FStatistics[bmtBestMoveValue,bmsHuman]:=Read('Statistics/BestMove/Value',0);
    FStatistics[bmtBestGameValue,bmsHuman]:=Read('Statistics/BestGame/Value',0);
    FStatistics[bmtSumMoveValue,bmsHuman]:=Read('Statistics/SumMove/Value',0);
    FStatistics[bmtSumGameValue,bmsHuman]:=Read('Statistics/SumGame/Value',0);
    FStatistics[bmtNumberOfMoves,bmsHuman]:=Read('Statistics/NumberOfMoves/Value',0);
    FStatistics[bmtNumberOfGames,bmsHuman]:=Read('Statistics/NumberOfGames/Value',0);
    FStatistics[bmtSumTime,bmsHuman]:=Read('Statistics/SumTime/Value',0);

    FStatistics[bmtBestMoveValue,bmsComputer]:=Read('Statistics/CompBestMove/Value',0);
    FStatistics[bmtBestGameValue,bmsComputer]:=Read('Statistics/CompBestGame/Value',0);
    FStatistics[bmtSumMoveValue,bmsComputer]:=Read('Statistics/CompSumMove/Value',0);
    FStatistics[bmtSumGameValue,bmsComputer]:=Read('Statistics/CompSumGame/Value',0);
    FStatistics[bmtNumberOfMoves,bmsComputer]:=Read('Statistics/CompNoM/Value',0);
    FStatistics[bmtNumberOfGames,bmsComputer]:=Read('Statistics/CompNoG/Value',0);
    FStatistics[bmtSumTime,bmsComputer]:=Read('Statistics/CompSumTime/Value',0);
  end;
  UpdateListView;
end;

procedure TfmStatistics.FormHide(Sender: TObject);
begin
  if assigned(Config) then
  begin
    Config.SaveWindowPosition(self);
    with Config do
    begin
      Write('Statistics/BestMove/Value',FStatistics[bmtBestMoveValue,bmsHuman]);
      Write('Statistics/BestGame/Value',FStatistics[bmtBestGameValue,bmsHuman]);
      Write('Statistics/SumMove/Value',FStatistics[bmtSumMoveValue,bmsHuman]);
      Write('Statistics/SumGame/Value',FStatistics[bmtSumGameValue,bmsHuman]);
      Write('Statistics/NumberOfMoves/Value',FStatistics[bmtNumberOfMoves,bmsHuman]);
      Write('Statistics/NumberOfGames/Value',FStatistics[bmtNumberOfGames,bmsHuman]);
      Write('Statistics/SumTime/Value',FStatistics[bmtSumTime,bmsHuman]);

      Write('Statistics/CompBestMove/Value',FStatistics[bmtBestMoveValue,bmsComputer]);
      Write('Statistics/CompBestGame/Value',FStatistics[bmtBestGameValue,bmsComputer]);
      Write('Statistics/CompSumMove/Value',FStatistics[bmtSumMoveValue,bmsComputer]);
      Write('Statistics/CompSumGame/Value',FStatistics[bmtSumGameValue,bmsComputer]);
      Write('Statistics/CompNoM/Value',FStatistics[bmtNumberOfMoves,bmsComputer]);
      Write('Statistics/CompNoG/Value',FStatistics[bmtNumberOfGames,bmsComputer]);
      Write('Statistics/CompSumTime/Value',FStatistics[bmtSumTime,bmsComputer]);
    end;
  end;
end;

procedure TfmStatistics.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key=chr(VK_ESCAPE) then
    ModalResult:=mrCancel;
end;

procedure TfmStatistics.FormShow(Sender: TObject);
var
  v:TBestMoveType;
begin
  Width:=Width-1;
  with lvStatistics do
  begin
   for v:=bmtNumberOfMoves to bmtBestGameValue do
   begin
     Items[integer(v)].SubItems[0]:=inttostr(FStatistics[v,bmsHuman]);
     Items[integer(v)].SubItems[1]:=inttostr(FStatistics[v,bmsComputer]);
   end;

   Items[integer(bmtBestGameValue)+1].SubItems[0]:=Language.Format('%f',[FStatistics[bmtSumMoveValue,bmsHuman]/FStatistics[bmtNumberOfMoves,bmsHuman]]);
   Items[integer(bmtBestGameValue)+1].SubItems[1]:=Language.Format('%f',[FStatistics[bmtSumMoveValue,bmsComputer]/FStatistics[bmtNumberOfMoves,bmsComputer]]);
   Items[integer(bmtBestGameValue)+2].SubItems[0]:=Language.Format('%f',[FStatistics[bmtSumGameValue,bmsHuman]/FStatistics[bmtNumberOfGames,bmsHuman]]);
   Items[integer(bmtBestGameValue)+2].SubItems[1]:=Language.Format('%f',[FStatistics[bmtSumGameValue,bmsComputer]/FStatistics[bmtNumberOfGames,bmsComputer]]);

   if FStatistics[bmtNumberOfMoves,bmsHuman]>0 then
     Items[integer(bmtBestGameValue)+3].SubItems[0]:=SecondsToString(round(FStatistics[bmtSumTime,bmsHuman]/FStatistics[bmtNumberOfMoves,bmsHuman]),false);
   if FStatistics[bmtNumberOfMoves,bmsComputer]>0 then
     Items[integer(bmtBestGameValue)+3].SubItems[1]:=SecondsToString(round(FStatistics[bmtSumTime,bmsComputer]/FStatistics[bmtNumberOfMoves,bmsComputer]),false);
   if FStatistics[bmtNumberOfGames,bmsHuman]>0 then
     Items[integer(bmtBestGameValue)+4].SubItems[0]:=SecondsToString(round(FStatistics[bmtSumTime,bmsHuman]/FStatistics[bmtNumberOfGames,bmsHuman]));
   if FStatistics[bmtNumberOfGames,bmsComputer]>0 then
     Items[integer(bmtBestGameValue)+4].SubItems[1]:=SecondsToString(round(FStatistics[bmtSumTime,bmsComputer]/FStatistics[bmtNumberOfGames,bmsComputer]));

   Items[integer(bmtBestGameValue)+5].SubItems[0]:=SecondsToString(FStatistics[bmtSumTime,bmsHuman]);
   Items[integer(bmtBestGameValue)+5].SubItems[1]:=SecondsToString(FStatistics[bmtSumTime,bmsComputer]);
  end;
  Width:=Width+1;
end;

procedure TfmStatistics.UpdateListView;
begin
  with lvStatistics do
  begin
   Column[1].Caption:=rStat_OwnCaption;
   Column[2].Caption:=rStat_ComputerCaption;
   with Items.Add do
   begin
     Caption:=rStat_NumberOfMoves;
     SubItems.Add('');//bmsHuman
     SubItems.Add('');//bmsComputer
   end;
   with Items.Add do
   begin
    Caption:=rStat_NumberOfGames;
    SubItems.Add('');//bmsHuman
    SubItems.Add('');//bmsComputer
   end;
   with Items.Add do
   begin
    Caption:=rStat_BestMove;
    SubItems.Add('');//bmsHuman
    SubItems.Add('');//bmsComputer
   end;
   with Items.Add do
   begin
    Caption:=rStat_BestGame;
    SubItems.Add('');//bmsHuman
    SubItems.Add('');//bmsComputer
   end;
   with Items.Add do
   begin
    Caption:=rStat_AverageMove;
    SubItems.Add('');//bmsHuman
    SubItems.Add('');//bmsComputer
   end;
   with Items.Add do
   begin
    Caption:=rStat_AverageGame;
    SubItems.Add('');//bmsHuman
    SubItems.Add('');//bmsComputer
   end;
   with Items.Add do
   begin
    Caption:=rStat_AverageTimeMove;
    SubItems.Add('');//bmsHuman
    SubItems.Add('');//bmsComputer
   end;
   with Items.Add do
   begin
    Caption:=rStat_AverageTimeGame;
    SubItems.Add('');//bmsHuman
    SubItems.Add('');//bmsComputer
   end;
   with Items.Add do
   begin
    Caption:=rStat_SumTime;
    SubItems.Add('');//bmsHuman
    SubItems.Add('');//bmsComputer
   end;
  end;//with
end;

procedure TfmStatistics.AddValues(const aValue:integer; const aTime:word;
  const aSender: string; const IsGameStat: boolean);
var
  aBestMoveSender:TBestMoveSender;
begin
  if aSender='Computer' then aBestMoveSender:=bmsComputer else aBestMoveSender:=bmsHuman;
  if not IsGameStat then
  begin
    FStatistics[bmtNumberOfMoves,aBestMoveSender]:=FStatistics[bmtNumberOfMoves,aBestMoveSender]+1;
    if FStatistics[bmtBestMoveValue,aBestMoveSender]<aValue then FStatistics[bmtBestMoveValue,aBestMoveSender]:=aValue;
    FStatistics[bmtSumMoveValue,aBestMoveSender]:=FStatistics[bmtSumMoveValue,aBestMoveSender]+aValue;
    FStatistics[bmtSumTime,aBestMoveSender]:=FStatistics[bmtSumTime,aBestMoveSender]+aTime;
  end else
  begin
    FStatistics[bmtNumberOfGames,aBestMoveSender]:=FStatistics[bmtNumberOfGames,aBestMoveSender]+1;
    if FStatistics[bmtBestGameValue,aBestMoveSender]<aValue then FStatistics[bmtBestGameValue,aBestMoveSender]:=aValue;
    FStatistics[bmtSumGameValue,aBestMoveSender]:=FStatistics[bmtSumGameValue,aBestMoveSender]+aValue;
  end;
end;

procedure TfmStatistics.UpdateLanguage;
begin
  with lvStatistics do
  begin
    Column[1].Caption:=rStat_OwnCaption;
    Column[2].Caption:=rStat_ComputerCaption;
    Items[0].Caption:=rStat_NumberOfMoves;
    Items[1].Caption:=rStat_NumberOfGames;
    Items[2].Caption:=rStat_BestMove;
    Items[3].Caption:=rStat_BestGame;
    Items[4].Caption:=rStat_AverageMove;
    Items[5].Caption:=rStat_AverageGame;
    Items[6].Caption:=rStat_AverageTimeMove;
    Items[7].Caption:=rStat_AverageTimeGame;
    Items[8].Caption:=rStat_SumTime;
    Column[0].Width:=Parent.ClientWidth-205;
//    AdjustSize;
  end;//with
  chRating.AxisList[0].Title.Caption:=rStat_LeftAxis;
  chRating.AxisList[1].Title.Caption:=rStat_BottomAxis;
end;

initialization
  {$I ustatistics.lrs}

end.

