{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit LogViewer.Receivers.FileSystem;

{ Monitors FileSystem changes for a given path (directory or file). }

interface

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  LogViewer.Interfaces, LogViewer.Receivers.Base,
  LogViewer.Receivers.FileSystem.Settings;

type
  TFileSystemChannelReceiver = class(TChannelReceiver, IChannelReceiver,
                                                       IFileSystem
  )
  private
    FPath : string;
    function GetSettings: TFileSystemSettings;

    procedure SettingsChanged(Sender: TObject);

  protected
    function CreateSubscriber(
      ASourceId         : UInt32;
      AThreadId         : UInt32;
      const ASourceName : string
    ): ISubscriber; override;

  public
    constructor Create(
      AManager    : ILogViewerManager;
      const APath : string;
      const AName : string
    ); reintroduce; virtual;
    procedure AfterConstruction; override;

    property Settings: TFileSystemSettings
      read GetSettings;

  end;

implementation

uses
  System.SysUtils,
  Vcl.Forms,

  LogViewer.Subscribers.FileSystem;

{$REGION 'construction and destruction'}
procedure TFileSystemChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  PollTimer.Interval := 1000;
  PollTimer.Enabled  := True;
  PollTimer.OnTimer  := PollTimerTimer;
  Settings.OnChanged.Add(SettingsChanged);
end;

constructor TFileSystemChannelReceiver.Create(AManager: ILogViewerManager;
  const APath, AName: string);
begin
  inherited Create(AManager, AName);
  if APath.IsEmpty then
    FPath := ExtractFilePath(Application.ExeName)
  else
    FPath := APath;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TFileSystemChannelReceiver.GetSettings: TFileSystemSettings;
begin
  Result := Manager.Settings.FileSystemSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TFileSystemChannelReceiver.SettingsChanged(Sender: TObject);
begin
  Enabled := Settings.Enabled;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TFileSystemChannelReceiver.CreateSubscriber(ASourceId,
  AThreadId: UInt32; const ASourceName: string): ISubscriber;
begin
  //Settings.Port := ASourceName;
//  if ASourceName <> '' then
//  begin
//    Result := TFileSystemSubscriber.Create(
//      Self,
//      ASourceId,
//      ASourceName,
//      ASourceName,
//      False
//    );
//  end
//  else
//  begin
//    Result := nil;
//  end;
end;
{$ENDREGION}

end.
