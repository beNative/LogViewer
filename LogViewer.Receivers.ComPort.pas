{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

{ COM port channel receiver. }

interface

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  synaser,

  LogViewer.Interfaces, LogViewer.Receivers.Base, LogViewer.ComPort.Settings;

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
  TComPortChannelReceiver = class(TChannelReceiver, IChannelReceiver)
  private
    FPollTimer  : TTimer;
    FSettings   : TComPortSettings;

    function GetSettings: TComPortSettings;

    procedure FPollTimerTimer(Sender: TObject);

  protected
    {$REGION 'property access methods'}
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    property Settings: TComPortSettings
      read GetSettings;

  public
    constructor Create(
      AManager    : ILogViewerManager;
      const AName : string;
      ASettings   : TComPortSettings
    ); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function ToString: string; override;
  end;

implementation

uses
  System.SysUtils, System.AnsiStrings,

  DDuce.Logger.Interfaces,

  LogViewer.Subscribers.ComPort;

{$REGION 'construction and destruction'}
constructor TComPortChannelReceiver.Create(AManager: ILogViewerManager;
  const AName : string; ASettings: TComPortSettings);
begin
  inherited Create(AManager, AName);
  Guard.CheckNotNull(ASettings, 'ASettings');
  FSettings := TComPortSettings.Create;
  FSettings.Assign(ASettings);
end;

procedure TComPortChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FPollTimer := TTimer.Create(nil);
  FPollTimer.Interval := 100;
  FPollTimer.Enabled := False;
  FPollTimer.OnTimer := FPollTimerTimer;
  FSettings.OnChanged.Add(SettingsChanged);
end;

procedure TComPortChannelReceiver.BeforeDestruction;
begin
  FSettings.OnChanged.Remove(SettingsChanged);
  FPollTimer.Free;
  FSettings.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TComPortChannelReceiver.SetEnabled(const Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Value then
  begin
    FPollTimer.Enabled := True;
  end
  else
  begin
    FPollTimer.Enabled := False;
  end;
end;

function TComPortChannelReceiver.GetSettings: TComPortSettings;
begin
  Result := FSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TComPortChannelReceiver.FPollTimerTimer(Sender: TObject);
var
  LSubscriber : ISubscriber;
begin
  FPollTimer.Enabled := False;
  for LSubscriber in SubscriberList.Values do
  begin
    LSubscriber.Poll;
  end;
  FPollTimer.Enabled := True;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TComPortChannelReceiver.ToString: string;
begin
//
end;
{$ENDREGION}

end.
