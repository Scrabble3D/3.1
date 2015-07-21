{ Auto updater

  v3.1.3; 2015-Mar-01
  Copyleft (C) GPLv3: 1996-2015 Heiko Tietze heiko_tietze@web.de

  Copyright (C) GPLv3: 1996-2014 Heiko Tietze heiko_tietze@web.de

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

unit uupdater;

{$mode objfpc}{$H+}

interface

{$I conditions.inc}

uses
  Classes, SysUtils, StrUtils, Controls, ComCtrls, Dialogs,
  XMLConf, Inifiles, FileUtil, LCLIntf, DateUtils,
  HTTPSend, blcksock, //synapse
  uversion, zipper;

type

  EDownloadError=class(Exception);

  TUpdateItemType=(itDictionary=0,itLocalization=1,itDesign=2,itApplication=3);

  TUpdateItem=class
    English        : string;
    Native         : string;
    FileName       : string;
    ItemType       : TUpdateItemType;
    LocalVersion   : integer;
    RemoteVersion  : integer; //-1 = unknown
  end;

  { TDownload }
  TDownloadFinish=procedure(const aItem:TUpdateItem) of object;

  TDownload=class(TThread)
    private
      FHttp     : THttpSend;
      FItem     : TUpdateItem;
      FInSize   : Longword;
      FProgress : byte;
      FException  : Exception;
      FOnFinished : TDownLoadFinish;
      FDirectory,FFileName : string;
      procedure DoFinish(Sender: TObject);
      procedure DoHandleException;
      procedure DoFinished;
      function Download(aFileName:string):boolean;
      procedure SaveToFile(const aFileName:string);
      procedure SockCallBack(Sender: TObject; Reason: THookSocketReason; const Value:string);
      procedure SyncMethod;
    protected
      procedure Execute;override;
      property Http:THttpSend read FHttp;
    public
      constructor Create(const aItem:TUpdateItem);
      property OnFinished:TDownloadFinish read FOnFinished write FOnFinished;
    end;

  { TUpdater }
  TUpdateEvent = procedure(const aItem:TUpdateItem) of object;

  TUpdater=class
    private
      FUpdateItems: TList;
      FLastCheck: double;
      FOnUpdateItem: TUpdateEvent;
      FDownloadCount: byte;
      function GetLastUpdate: TDateTime;
      procedure SetLastUpdate(AValue: TDateTime);
      procedure UpdateConfig(const aFileName:string);
      procedure CheckLocalFiles;
      procedure RefreshItemList;
      function GetVersionFromTextFile(aFileName: string): integer;
      function GetVersionFromZipFile(aFileName: string): integer;
      procedure DoAfterDownload(const aItem:TUpdateItem);
      function UnzipFile(const aFileName:string):boolean;
      function MoveFile(const aFileName: string): boolean;
    protected
      function ExtractFiles(const aZipFileName,aFileName:string;aOutPath:string=''):boolean;
    public
      constructor Create;
      destructor Destroy;override;
      procedure ItemsByIdentifier(const aIdentifier:TUpdateItemType; constref Result: TListItems);
      function ItemByFileName(const aFileName:string):TUpdateItem;
      procedure CheckForUpdates;
      procedure DownloadFile(const aItem:TUpdateItem);
      property LastUpdate:TDateTime read GetLastUpdate write SetLastUpdate;
      property OnUpdateItem:TUpdateEvent read FOnUpdateItem write FOnUpdateItem;
    end;

var
  Updater : TUpdater;

implementation

uses
  ugameoptions, utypes, uconfig, ulanguage;

const
  cSection:array[TUpdateItemType] of string=(rUpdaterDic,rUpdaterLocalization,rUpdaterDesign,rUpdaterApplication);
  cVersionsInfo='version.conf';

{ TDownload }

procedure TDownload.DoFinish(Sender: TObject);
begin
  FHttp.Free;
end;

procedure TDownload.DoHandleException;
begin
  SysUtils.ShowException(FException,nil);
end;

procedure TDownload.DoFinished;
begin
  if assigned(OnProgress) then
    OnProgress(self,101); //clear progress bar
  if assigned(FOnFinished) then
    FOnFinished(FItem);
end;

function TDownload.Download(aFileName: string):boolean;
const
  aURLPath='http://downloads.sourceforge.net/project/scrabble/';
var
  aURL : string;
  i,j,iter  : integer;
begin
  FInSize:=0;
  aURL:=aURLPath+aFileName;
  FHttp.Sock.OnStatus:=@SockCallBack;
  Result:=false;
  iter:=0;
  while not Result do
  begin
    FHttp.HTTPMethod('GET', aURL);
    inc(iter);
    case FHttp.Resultcode of
      301,302,307 :
        for i:=0 to FHttp.Headers.Count-1 do
         if (FindPart('Location: ',FHttp.Headers.Strings[i])>0) or
            (FindPart('location: ',FHttp.Headers.Strings[i])>0) then
         begin
           j:=Pos('use_mirror=',FHttp.Headers.Strings[i]);
           if j>0 then
             aURL:='http://'+RightStr(FHttp.Headers.Strings[i],length(FHttp.Headers.Strings[i])-j-10)+'.dl.sourceforge.net/project/scrabble/'+aFileName else
             aURl:=StringReplace(FHttp.Headers.Strings[i],'Location: ','',[]);
           FHttp.Clear;//httpsend
           break;
         end;
      100..200 :
        Result:=true;
      500:
        raise EDownloadError.Create('No internet connection available');//Internal Server Error ('+aURL+')');
      else
        raise EDownloadError.Create('Download failed with error code '+inttostr(FHttp.ResultCode)+' ('+FHttp.ResultString+')');
    end;//case
    if iter>10 then
      raise EDownloadError.Create('Download failed (>10 iterations)');
  end;//while
  Result:=true;
end;

procedure TDownload.SaveToFile(const aFileName: string);
begin
  with TFileStream.Create(aFileName,fmCreate or fmOpenWrite) do
  try
    Seek(0, soFromBeginning);
    CopyFrom(FHttp.Document, 0);
  finally
    Free; //TFileStream
  end;
end;

procedure TDownload.SockCallBack(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  if (Reason=HR_Readcount) and (FHttp.DownloadSize>0) then
  begin
    {$Hints off}
    inc(FInSize,StrToInt(Value));
    {$Hints on}
    if FProgress<>round((FInSize/FHttp.DownloadSize)*100) then
    begin
      FProgress:=round((FInSize/FHttp.DownloadSize)*100) ;
      Synchronize(@SyncMethod);
    end;
  end;
end;

procedure TDownload.SyncMethod;
begin
  if assigned(OnProgress) then
    OnProgress(self,FProgress);
end;

constructor TDownload.Create(const aItem:TUpdateItem);
begin
  inherited Create(true);
  FItem:=aItem;
  if FItem<>nil then
  case FItem.ItemType of
   itDictionary:
    begin
      FDirectory:='Dictionaries/';
      FFileName:=ChangeFileExt(aItem.FileName,'.zip');
    end;
   itLocalization:
    begin
      FDirectory:='Localization/';
      FFileName:=aItem.FileName;//ChangeFileExt(aItem.FileName,'.zip');
    end;
   itDesign:
    begin
      FDirectory:='Designs/';
      FFileName:=aItem.FileName;
    end;
   itApplication:
    begin
      FDirectory:=aItem.Native;
      FFileName:=aItem.FileName;
    end;
  end else //case
  begin
    FFileName:=cVersionsInfo;
    FDirectory:='';
  end; //FItem=nil
  FHttp:=THttpSend.Create;
  FHttp.KeepAliveTimeout:=30;
  OnTerminate:=@DoFinish;
  FreeOnTerminate:=true;
end;

procedure TDownload.Execute;
begin
  if FFileName<>'' then
  try
    Download(FDirectory+FFileName);
    SaveToFile(GetTempDir+FFileName);
    Synchronize(@DoFinished);
  except
    FException:=Exception(ExceptObject);
    Synchronize(@DoHandleException);
  end;
end;

{ TUpdater }

constructor TUpdater.Create;
begin
  inherited;
  FUpdateItems:=TList.Create;
  FDownloadCount:=0;
  CheckLocalFiles;
  RefreshItemList;
end;

destructor TUpdater.Destroy;
var
  i:integer;
begin
  for i:=0 to FUpdateItems.Count-1 do
    TUpdateItem(FUpdateItems[i]).Free;
  FUpdateItems.Clear;
  FUpdateItems.Free;
  inherited Destroy;
end;

procedure TUpdater.CheckForUpdates;
begin
  OnMessage(smDebug,'Download of version info from Sourceforge.net...');
  DownloadFile(nil);
end;

procedure TUpdater.ItemsByIdentifier(const aIdentifier: TUpdateItemType; constref Result: TListItems);
const
  MinDiff=5;//seconds between last check of local files
var
  i:integer;
begin
  if (FDownloadCount=0) and                          //do not clear list when items are being updated
     (SecondsBetween(Now,FLastCheck)>MinDiff) then   //do not update multiple times on load
  begin
    CheckLocalFiles;
    RefreshItemList;
  end;

  for i:=0 to FUpdateItems.Count-1 do
  with TUpdateItem(FUpdateItems[i]) do
   if ItemType=aIdentifier then
   with Result.Add do
   begin
     Caption:=English;
     SubItems.Add(Native);
     SubItems.Add(VersionToString(LocalVersion));
     SubItems.Add(VersionToString(RemoteVersion));
     SubItems.Add(FileName);
   end;
end;

function TUpdater.ItemByFileName(const aFileName:string): TUpdateItem;
var
  i:integer;
begin
  for i:=0 to FUpdateItems.Count-1 do
   if TUpdateItem(FUpdateItems[i]).FileName=aFileName then
    exit(TUpdateItem(FUpdateItems[i]));
  Result:=nil;
end;

procedure TUpdater.DownloadFile(const aItem: TUpdateItem);
begin
  with TDownload.Create(aItem) do
  begin
    if fmGameOptions.cbProxy.Checked then
    begin
      Http.ProxyHost:=fmGameOptions.edProxyIP.Text;
      Http.ProxyPort:=fmGameOptions.edProxyPort.Text;
      Http.ProxyUser:=fmGameOptions.edProxyUser.Text;
      Http.ProxyPass:=fmGameOptions.edProxyPass.Text;
    end;
    if aItem<>nil then
      OnMessage(smPanelOnly,Language.Format(rUpdater_DownloadRunning,[aItem.FileName]));
    OnFinished:=@DoAfterDownload;
    inc(FDownloadCount);
    Start;
  end;
end;

function TUpdater.UnzipFile(const aFileName: string): boolean;
var
  aItem:TUpdateItem;
begin
  with TUnzipper.Create do
  try
    OutputPath:=GetTempDir;
    UnZipAllFiles(GetTempDir+ChangeFileExt(aFileName,'.zip'));
    DeleteFileUTF8(GetTempDir+ChangeFileExt(aFileName,'.zip'));
    aItem:=ItemByFileName(aFileName);
    if aItem=nil then
      exit(false);
    with TIniFile.Create(GetTempDir+aFileName) do
    try
      if StringToVersion(VersionToString(ReadInteger('Header','Version',0)))=aItem.RemoteVersion then
      begin
        aItem.LocalVersion:=aItem.RemoteVersion;
        Result:=true;
      end else
      begin
        OnMessage(smWarning,Language.Format(rUpdater_InfoError,[aFileName])+' ('+VersionToString(ReadInteger('Header','Version',0))+'<>'+VersionToString(aItem.RemoteVersion)+')');
        DeleteFileUTF8(GetTempDir+aFileName);
        Result:=false;
      end;
    finally
      Free; //inifile
    end;
  finally
    Free; //unzipper
  end;
end;

function TUpdater.MoveFile(const aFileName: string): boolean;
begin
  Result:=false;
  DeleteFileUTF8(Config.Path+aFileName);
  CopyFile(GetTempDir+aFileName,Config.Path+aFileName);
  DeleteFileUTF8(GetTempDir+aFileName);
  Result:=true;
end;

function TUpdater.ExtractFiles(const aZipFileName,aFileName:string;aOutPath:string=''):boolean;
var
  sl:TStringList;
begin
  Result:=false;
  sl:=TStringList.Create;
  try
    sl.Add(aFileName);
    with TUnzipper.Create do
    try
      FileName:=aZipFileName;
      OutputPath:=aOutPath;
      UnZipFiles(sl);
      Result:=true;
    finally
      Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure TUpdater.UpdateConfig(const aFileName:string);
var
  i,j,z:integer;
  s,aSection:string;
  aSFConfig:TXMLConfig;
begin
  aSFConfig:=TXMLConfig.Create(nil);
  try
    aSFConfig.FileName:=aFileName;
    //server
    aSection:='Network/Servers/';
    z:=aSFConfig.GetValue(aSection+'Count',0);
    Config.Write(aSection+'Count',z);
    for i:=0 to z-1 do
    begin
      s:=aSFConfig.GetValue(aSection+'f'+inttostr(i)+'/Name','');
      Config.Write(aSection+'f'+inttostr(i)+'/Name',s);
      s:=aSFConfig.GetValue(aSection+'f'+inttostr(i)+'/Address','');
      Config.Write(aSection+'f'+inttostr(i)+'/Address',s);
    end;

    //links
    aSection:='Network/Links';
    Config.Delete(aSection);
    s:=aSFConfig.GetValue(aSection+'/News','http://twitter.com/scrabble3d');
    Config.Write(aSection+'/News',s);
    s:=aSFConfig.GetValue(aSection+'/Forum','http://17085.homepagemodules.de');
    Config.Write(aSection+'/Forum',s);
    s:=aSFConfig.GetValue(aSection+'/SocialMedia','http://www.facebook.com/pages/Scrabble3D/189119127784666');
    Config.Write(aSection+'/SocialMedia',s);
    s:=aSFConfig.GetValue(aSection+'/Wiki','http://sourceforge.net/apps/mediawiki/scrabble');
    Config.Write(aSection+'/Wiki',s);
    s:=aSFConfig.GetValue(aSection+'/Localization','https://www.transifex.com/projects/p/scrabble3d/');
    Config.Write(aSection+'/Localization',s);
    s:=aSFConfig.GetValue(aSection+'/Contact','heiko_tietze@web.de');
    Config.Write(aSection+'/Contact',s);

    //files
    for i:=0 to length(cSection)-1 do
    begin
      aSection:='Network/Files/'+cSection[TUpdateItemType(i)];
      z:=aSFConfig.GetValue(aSection+'/Count',0);
      Config.Write(aSection+'/Count',z);
      for j:=0 to z-1 do
      begin
        s:=aSFConfig.GetValue(aSection+'/f'+inttostr(j)+'/FileName','');
        Config.Write(aSection+'/f'+inttostr(j)+'/FileName',s);
        s:=aSFConfig.GetValue(aSection+'/f'+inttostr(j)+'/English','');
        Config.Write(aSection+'/f'+inttostr(j)+'/English',s);
        s:=UTF8Encode(aSFConfig.GetValue(aSection+'/f'+inttostr(j)+'/Native',''));
        Config.Write(aSection+'/f'+inttostr(j)+'/Native',s);
        s:=aSFConfig.GetValue(aSection+'/f'+inttostr(j)+'/RemoteVersion','');
        Config.Write(aSection+'/f'+inttostr(j)+'/RemoteVersion',s);
      end;
    end;
  finally
    aSFConfig.Free;
  end;
end;

procedure TUpdater.DoAfterDownload(const aItem:TUpdateItem);
var
  i:integer;
  s:string;
  b:boolean;
  uItem:TUpdateItem;
begin
  dec(FDownloadCount);
  if aItem=nil then //version.conf
  begin
    UpdateConfig(UTF8ToSys(GetTempDir+cVersionsInfo));//ConfigPath+'version.conf';//
    DeleteFileUTF8(GetTempDir+cVersionsInfo);
    FLastCheck:=0; //reset last check time
    //local files
    OnMessage(smDebug,'Update of version info for local files...');

    if assigned(FOnUpdateItem) then
      FOnUpdateItem(nil);//refresh lists of external files in config
    //info
    LastUpdate:=Now;
    //updates
    b:=false;
    for i:=0 to FUpdateItems.Count-1 do
    begin
      uItem:=TUpdateItem(FUpdateItems[i]);
      if (uItem.LocalVersion>-1) and (uItem.RemoteVersion>-1) then
      begin
        if uItem.LocalVersion=uItem.RemoteVersion then s:='=' else
        if uItem.LocalVersion<uItem.RemoteVersion then s:='<' else
                                                       s:='>';
        OnMessage(smDebug,cSection[uItem.ItemType]+' '+uItem.English+': '+VersionToString(uItem.LocalVersion)+s+VersionToString(uItem.RemoteVersion));
        if (uItem.LocalVersion<uItem.RemoteVersion) then
        begin
          if (MessageDlg(Language.Format(rUpdater_NewRelease,[cSection[uItem.ItemType],uItem.Native,
                                VersionToString(uItem.LocalVersion),VersionToString(uItem.RemoteVersion)]),
                                 mtConfirmation,[mbYes,mbNo],0)=mrYes) then
          begin
            DownloadFile(uItem);
            b:=true;
          end;
        end;
      end;//count
    end; //for to count
    if not b then
      OnMessage(smInformation,rUpdater_AllUpToData);
  end else //normal file
  begin
    OnMessage(smPanelOnly,Language.Format(rUpdater_DownloadFinished,[aItem.FileName]));
    case aItem.ItemType of
     itDictionary:
      begin
        if UnzipFile(aItem.FileName) then
         if MoveFile(aItem.FileName) then
          FOnUpdateItem(aItem);
      end;
     itLocalization,itDesign:
      if MoveFile(aItem.FileName) then
        FOnUpdateItem(aItem);
     itApplication:
       OpenDocument(GetTempDir+aItem.FileName);
    end;
  end;
end;

function TUpdater.GetLastUpdate: TDateTime;
begin
  Result:=StrToDateTimeDef(Config.Read('General/Files/LastUpdate',DateTimeToStr(0)),0);
end;

procedure TUpdater.SetLastUpdate(AValue: TDateTime);
begin
  Config.Write('General/Files/LastUpdate',DateTimeToStr(Now));
end;

function TUpdater.GetVersionFromTextFile(aFileName: string): integer;
var
  z1,z2:integer;
  s:string;
  f:Textfile;
begin
  Result:=cUnknownVersion;
  AssignFile(f,aFileName);
  Reset(f);
  try
    while not eof(f) do
    begin
      ReadLn(f,s);
      if Pos('Version=',s)>0 then
      begin
        s:=copy(s,9,length(s));
        if not TryStrToInt(s,Result) then
          Result:=StringToVersion(s);
        exit;
      end;
      z1:=pos('"Project-Id-Version: ',s);
      if z1>0 then
      begin
        z2:=pos('\n',copy(s,z1,length(s)));
        s:=copy(s,z1+length('"Project-Id-Version: '),z2-length('"Project-Id-Version: ')-1);
        if not TryStrToInt(s,Result) then
          Result:=StringToVersion(s);
        exit;
      end;
    end;
  finally
    CloseFile(f);
  end;
end;

function TUpdater.GetVersionFromZipFile(aFileName: string): integer;
var
  st:TStream;
begin
  st:=TStream.Create;
  try
    if ExtractFiles(aFileName,'content.xml',GetTempDir) then
    with TXMLConfig.Create(nil) do
    try
      FileName:=GetTempDir+'/content.xml';
      if GetValue('General/MaxVers/Value',0)=cDesign then
        Result:=StringToVersion(GetValue('General/Version/Value','0')) else
        Result:=-1;
    finally
      Free;//TStringList
    end;
    DeleteFileUTF8(GetTempDir+'/content.xml');
  finally
    st.Free;
  end;
end;

procedure TUpdater.CheckLocalFiles;
 function FindKey(const aSection,aItem: string): string;
 var
   i,z:integer;
   s:string;
 begin
   z:=Config.Read(aSection+'/Count',0);
   for i:=0 to z-1 do
   begin
     s:=Config.Read(aSection+'/f'+inttostr(i)+'/FileName','');
     if s=aItem then
       exit(aSection+'/f'+inttostr(i));
   end;
   z:=Config.Read(aSection+'/LocalCount',0);
   Config.Write(aSection+'/LocalCount',z+1);
   Config.Write(aSection+'/l'+inttostr(z)+'/FileName',aItem);
   Result:=aSection+'/l'+inttostr(z);
 end;
const
  cExtension:array[TUpdateItemType] of string=('*.dic','*.lang','*.dsgn','');
var
  i,j,z:integer;
  s,aSection:string;
  sr:TSearchRec;
begin
  for i:=0 to length(cSection)-1 do
  if TUpdateItemType(i)<>itApplication then
  begin
    aSection:='Network/Files/'+cSection[TUpdateItemType(i)];
    //clear non-existing local files
    z:=Config.Read(aSection+'/Count',0);
    for j:=0 to z-1 do
    begin
      s:=Config.Read(aSection+'/f'+inttostr(j)+'/FileName','');
      if not FileExistsUTF8(Config.Path+s) then
        Config.Write(aSection+'/f'+inttostr(j)+'/LocalVersion','');
    end;
    //clear local file info
    z:=Config.Read(aSection+'/LocalCount',0);
    for j:=0 to z-1 do
      Config.Delete(aSection+'/l'+inttostr(j));
    Config.Write(aSection+'/LocalCount',0);

    //check local files
    if FindFirstUTF8(Config.Path+cExtension[TUpdateItemType(i)], faAnyFile, sr)=0 then
    repeat
      s:=ExtractFileName(sr.Name);
      if s='default.lang' then Continue;
      if TUpdateItemType(i)=itDesign then
        j:=GetVersionFromZipFile(Config.Path+sr.Name) else
        j:=GetVersionFromTextFile(Config.Path+sr.Name);
      if j>-1 then
        Config.Write(FindKey(aSection,s)+'/LocalVersion',VersionToString(j));
    until FindNextUTF8(sr)<>0;
    FindCloseUTF8(sr);
  end else
  begin
    {$ifdef Windows}
     {$ifdef Portable}
      Config.Write('Network/Files/Application/f0/Native','Main_Program/Portable/');
      {$ifdef Win32} Config.Write('Network/Files/Application/f0/FileName','Scrabble3D-win32.zip'); {$endif}
      {$ifdef Win64} Config.Write('Network/Files/Application/f0/FileName','Scrabble3D-win64.zip'); {$endif}
     {$else}
      Config.Write('Network/Files/Application/f0/Native','Main_Program/Windows/');
      {$ifdef Win32} Config.Write('Network/Files/Application/f0/FileName','Scrabble3D-win32.msi'); {$endif}
      {$ifdef Win64} Config.Write('Network/Files/Application/f0/FileName','Scrabble3D-win64.msi'); {$endif}
     {$endif}
    {$endif}
    {$ifdef Darwin}
     Config.Write('Network/Files/Application/f0/Native','Main_Program/MacOS/');
     {$ifdef Portable}
      Config.Write('Network/Files/Application/f0/FileName','Scrabble3D-darwin.dmg');
     {$else}
      Config.Write('Network/Files/Application/f0/FileName','Scrabble3D-darwin.pkg');
     {$endif}
    {$endif}
    {$ifdef Linux}
     {$IFDEF LCLQt}
      {$ifdef CPU32} s:='QScrabble3D-i386';//dev-libs not installed
      {$else}        s:='QScrabble3D-x86_64';{$endif}
     {$ELSE}
      {$ifdef CPU32} s:='Scrabble3D-i386';
      {$else}        s:='Scrabble3D-x86_64';{$endif}
     {$ENDIF}
     {$ifdef Portable}
      Config.Write('Network/Files/Application/f0/Native','Main_Program/Portable/');
      Config.Write('Network/Files/Application/f0/FileName',s+'.tar.gz');
     {$else}
      Config.Write('Network/Files/Application/f0/Native','Main_Program/Linux/');
      {$Note Installer: rpm/deb}
      {$define deb}//two version are built, for rpm or deb
      {$ifdef deb}
      Config.Write('Network/Files/Application/f0/FileName',s+'.deb');
      {$else}
      Config.Write('Network/Files/Application/f0/FileName',s+'.rpm');
      {$endif}
     {$endif}
    {$endif}
    Config.Write('Network/Files/Application/f0/LocalVersion',cVersion);
  end;
  FLastCheck:=Now;
end;

procedure TUpdater.RefreshItemList;
var
  i,j,z:integer;
  aSection:string;
  aUpdateItem:TUpdateItem;
begin
  for i:=0 to FUpdateItems.Count-1 do
    TUpdateItem(FUpdateItems[i]).Free;
  FUpdateItems.Clear;

  for i:=0 to length(cSection)-1 do
  begin
    aSection:='Network/Files/'+cSection[TUpdateItemType(i)];
    z:=Config.Read(aSection+'/Count',0);
    for j:=0 to z-1 do
    begin
      aUpdateItem:=TUpdateItem.Create;
      aUpdateItem.ItemType:=TUpdateItemType(i);
      aUpdateItem.FileName:=Config.Read(aSection+'/f'+inttostr(j)+'/FileName','');
      aUpdateItem.English:=Config.Read(aSection+'/f'+inttostr(j)+'/English','');
      aUpdateItem.Native:=Config.Read(aSection+'/f'+inttostr(j)+'/Native','');
      aUpdateItem.RemoteVersion:=StringToVersion(Config.Read(aSection+'/f'+inttostr(j)+'/RemoteVersion',''));
      aUpdateItem.LocalVersion:=StringToVersion(Config.Read(aSection+'/f'+inttostr(j)+'/LocalVersion',''));
      FUpdateItems.Add(aUpdateItem);
    end;
    z:=Config.Read(aSection+'/LocalCount',0);
    for j:=0 to z-1 do
    begin
      aUpdateItem:=TUpdateItem.Create;
      aUpdateItem.ItemType:=TUpdateItemType(i);
      aUpdateItem.FileName:=Config.Read(aSection+'/l'+inttostr(j)+'/FileName','');
      aUpdateItem.English:=aUpdateItem.FileName;
      aUpdateItem.RemoteVersion:=cUnknownVersion;
      aUpdateItem.LocalVersion:=StringToVersion(Config.Read(aSection+'/l'+inttostr(j)+'/LocalVersion',''));
      FUpdateItems.Add(aUpdateItem);
    end;
  end;
end;

initialization
  Updater:=TUpdater.Create;
finalization
  Updater.Free;
end.

