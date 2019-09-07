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

unit LogViewer.Receivers.Winipc;

{ Winipc channel receiver. }

interface

{$REGION 'documentation'}
{ Receives logmessages through WinIPC (WM_COPYDATA) messages.

  The communication with the message source is synchronous, so when the source
  application sends a message, it blocks until it is received by the receiver.
  This is because WM_COPYDATA messages are always sent and not posted.

  To avoid that the processing of these messages blocks the main thread, these
  are handled by a background thread (TWinipcBroker) and queued using a inproc
  ZeroMQ publisher socket. In the main thread a corresponding subscriber socket
  is created which polls for incoming messages.

  REMARK:
   - the sending application and the logviewer need to be started with the same
     windows user credentials. This is required to be able to exchange
     WM_COPYDATA messages between applications.
}
{$ENDREGION}

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring, Spring.Collections,

  ZeroMQ.API, ZeroMQ,

  LogViewer.Receivers.Base, LogViewer.Interfaces,
  LogViewer.Receivers.Winipc.Settings, LogViewer.WinipcBroker;

type
  TWinipcChannelReceiver = class(TChannelReceiver, IChannelReceiver, IWinipc)
  private
    FZmq    : IZeroMQ;
    FBroker : TWinipcBroker;

  protected
    {$REGION 'property access methods'}
    function GetSettings: TWinipcSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure SettingsChanged(Sender: TObject);
    procedure SubscriberListChanged(
      Sender     : TObject;
      const Item : ISubscriber;
      Action     : TCollectionChangedAction
    );
    procedure FBrokerAddSubscriber(
      Sender             : TObject;
      const AEndpoint    : string;
      AProcessId         : UInt32;
      const AProcessName : string
    );

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Settings: TWinipcSettings
      read GetSettings;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,

  DDuce.Utils.Winapi, DDuce.Logger,

  LogViewer.Subscribers.Winipc;

{$REGION 'construction and destruction'}
procedure TWinipcChannelReceiver.AfterConstruction;
begin
  Logger.Track(Self, 'AfterConstruction');
  inherited AfterConstruction;
  FZmq := TZeroMQ.Create;


  Settings.OnChanged.Add(SettingsChanged);
  SubscriberList.OnValueChanged.Add(SubscriberListChanged);

  PollTimer.Interval := 10;
  PollTimer.Enabled  := True;
end;

destructor TWinipcChannelReceiver.Destroy;
var
  LSubscriber : ISubscriber;
begin
  PollTimer.Enabled := False;
  Settings.OnChanged.RemoveAll(Self);
  for LSubscriber in SubscriberList.Values do
  begin
    // if we don't close all subscribers, the ZeroMQ library will prevent
    // the application from closing.
    LSubscriber.Close;
  end;
  if Assigned(FBroker) then
  begin
    FBroker.OnAddSubscriber := nil;
    FBroker.Terminate;
    FBroker.WaitFor;
    FreeAndNIl(FBroker);
  end;

  SubscriberList.OnValueChanged.RemoveAll(Self);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TWinipcChannelReceiver.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    inherited SetEnabled(Value);
    if Value then
    begin
      if not Assigned(FBroker) then
      begin
        FBroker := TWinipcBroker.Create(FZmq);
        FBroker.OnAddSubscriber := FBrokerAddSubscriber;
        FBroker.FreeOnTerminate := False;
        FBroker.Start;
      end
    end
    else
    begin
      if Assigned(FBroker) then
      begin
        FBroker.OnAddSubscriber := nil;
        FBroker.Terminate;
        FBroker.WaitFor;
        FreeAndNIl(FBroker);
      end;
    end;
  end;
  PollTimer.Enabled := Value;
end;

function TWinipcChannelReceiver.GetSettings: TWinipcSettings;
begin
  Result := Manager.Settings.WinIPCSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TWinipcChannelReceiver.SettingsChanged(Sender: TObject);
begin
  Enabled := Settings.Enabled;
  PollTimer.Enabled := True;
end;

procedure TWinipcChannelReceiver.SubscriberListChanged(Sender: TObject;
  const Item: ISubscriber; Action: TCollectionChangedAction);
begin
  if Action = caRemoved then
  begin
    Item.Close;
  end;
end;

procedure TWinipcChannelReceiver.FBrokerAddSubscriber(Sender: TObject;
  const AEndpoint: string; AProcessId: UInt32; const AProcessName : string);
var
  LSubscriber  : ISubscriber;
  LProcessName : string;
begin
  if not SubscriberList.TryGetValue(AProcessId, LSubscriber) then
  begin
    Logger.Info('Create subscriber %s', [AEndPoint]);
    LSubscriber := TWinipcSubscriber.Create(
      Self, FZmq,  AEndPoint, AProcessId, '', AProcessName, True
    );
    SubscriberList.AddOrSetValue(AProcessId, LSubscriber);
    if not Processes.TryGetValue(AProcessId, LProcessName) then
    begin
      LProcessName := GetExenameForProcess(AProcessId);
      Processes.AddOrSetValue(AProcessId, LProcessName);
    end;
  end;
end;
{$ENDREGION}

end.
