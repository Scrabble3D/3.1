{ Thread-safe string list

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

unit uthreadstringlist;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils,LCLIntf;

type
  { TThreadStringList }
  TThreadStringList = class
    private
      FStringList: TStringList;
      {$ifdef CPU32}
      aCriticalSection:LongWord;//TRTLCriticalSection;
      {$else}
      aCriticalSection:QWord;
      {$endif}
      function GetCount: Integer;
      function GetStrings(Index: Integer): string;
      procedure SetStrings(Index: Integer; const AValue: string);
    public
      constructor Create;
      destructor Destroy; override;
      function LockList: TStringList;
      procedure UnlockList;
      function Add(const aValue: string): Integer;
      procedure Delete(Index: Integer);
      property Count: Integer read GetCount;
      property Strings[Index: Integer]: string read GetStrings write SetStrings; default;
    end;

implementation

{ TThreadStringList }

constructor TThreadStringList.Create;
begin
  inherited Create;
  InitializeCriticalSection(aCriticalSection);
  FStringList:=TStringList.Create;
end;

destructor TThreadStringList.Destroy;
begin
  LockList;
  try
    FStringList.Free;
  finally
    UnlockList;
    DeleteCriticalSection(aCriticalSection);
  end;
  inherited Destroy;
end;

function TThreadStringList.LockList: TStringList;
begin
  EnterCriticalSection(aCriticalSection);
  Result:=FStringList;
end;

procedure TThreadStringList.UnlockList;
begin
  LeaveCriticalSection(aCriticalSection);
end;

function TThreadStringList.GetCount: Integer;
begin
  LockList;
  try
    Result:=FStringList.Count;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetStrings(Index: Integer): string;
begin
  LockList;
  try
    Result := FStringList.Strings[Index];
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetStrings(Index: Integer; const aValue: string);
begin
  LockList;
  try
    FStringList.Strings[Index]:=aValue;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.Add(const aValue: string): Integer;
begin
  Result:=-1;
  LockList;
  try
    Result:=FStringList.Add(aValue);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Delete(Index: Integer);
begin
  LockList;
  try
    FStringList.Delete(Index);
  finally
    UnlockList;
  end;
end;

end.

