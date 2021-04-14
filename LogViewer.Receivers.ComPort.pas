{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Receivers.ComPort;

{ COM-port channel receiver. }

interface

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  synaser,

  LogViewer.Interfaces, LogViewer.Receivers.Base,
  LogViewer.Receivers.ComPort.Settings;

{$REGION 'documentation'}
{ Receives data from a serial port. The data is queued as a TLogMessage
  compatible stream.

  For now the serial data is handled as:
    - a watch message if the text has a format like '%s=%s' (name-value pair
      with '=' as delimiter)
    - a text message otherwise
}
{$ENDREGION}

type
  TComPortChannelReceiver = class(TChannelReceiver, IChannelReceiver, IComPort)
  private
    procedure SettingsChanged(Sender: TObject);

  protected
    {$REGION 'property access methods'}
    function GetSettings: TComPortSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure UpdateSubscribers;

    property Settings: TComPortSettings
      read GetSettings;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function CreateSubscriber(
      ASourceId         : UInt32;
      AThreadId         : UInt32;
      const ASourceName : string
    ): ISubscriber;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Logger,

  LogViewer.Subscribers.ComPort;

{$REGION 'construction and destruction'}
procedure TComPortChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  Settings.OnChanged.Add(SettingsChanged);
  PollTimer.Interval := Settings.PollingInterval;
  PollTimer.Enabled  := False;
  PollTimer.OnTimer  := PollTimerTimer;
end;

destructor TComPortChannelReceiver.Destroy;
begin
  PollTimer.Enabled  := False;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TComPortChannelReceiver.SetEnabled(const Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Value then
  begin
    UpdateSubscribers;
    PollTimer.Enabled := True;
  end
  else
  begin
    PollTimer.Enabled := False;
  end;
end;

function TComPortChannelReceiver.GetSettings: TComPortSettings;
begin
  Result := Manager.Settings.ComPortSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TComPortChannelReceiver.SettingsChanged(Sender: TObject);
begin
  PollTimer.Interval := Settings.PollingInterval;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TComPortChannelReceiver.UpdateSubscribers;
var
  I  : Integer;
  SL : Shared<TStringList>;
  S  : ISubscriber;
begin
  Logger.Track(Self, 'UpdateSubscribers');
  SL := TStringList.Create;
  SL.Value.CommaText := GetSerialPortNames;
  for I := 0 to SL.Value.Count - 1 do
  begin
    S := CreateSubscriber(I, 0, SL.Value[I]);
    if Assigned(S) then
      SubscriberList.AddOrSetValue(I, S);
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TComPortChannelReceiver.CreateSubscriber(ASourceId, AThreadId: UInt32;
  const ASourceName: string): ISubscriber;
begin
  Logger.Track(Self, 'CreateSubscriber');
  Settings.Port := ASourceName;
  if ASourceName <> '' then
  begin
    Result := TComPortSubscriber.Create(
      Self,
      ASourceId,
      ASourceName,
      ASourceName,
      False,
      Settings
    );
  end
  else
  begin
    Result := nil;
  end;
end;
{$ENDREGION}

end.
