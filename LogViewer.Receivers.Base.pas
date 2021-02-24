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

unit LogViewer.Receivers.Base;

{ Base class for all channel receivers (implementing IChannelReceiver). }

interface

{$REGION 'documentation'}
{ A IChannelReceiver instance maintains a dictionary of subscribers
  (ISubscriber instances) with SourceId as the key.

  TChannelReceiver has a class property Processes that keeps track of monitored
  (Windows) ProcessId/ProcessName pairs.
}
{$ENDREGION}

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring, Spring.Collections,

  LogViewer.Interfaces;

type
  TChannelReceiver = class(TInterfacedObject, IChannelReceiver)
  private
    FEnabled        : Boolean;
    FName           : string;
    FSubscriberList : IDictionary<UInt32, ISubscriber>;
    FManager        : ILogViewerManager;
    FPollTimer      : Lazy<TTimer>;
    FOnChange       : Event<TNotifyEvent>;

  class var
    FProcesses : Lazy<IDictionary<UInt32, string>>;

    class function GetProcesses: IDictionary<UInt32, string>; static;

  protected
    {$REGION 'property access methods'}
    function GetPollTimer: TTimer;
    function GetManager: ILogViewerManager;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean); virtual;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetSubscriberList: IDictionary<UInt32, ISubscriber>;
    function GetOnChange: IEvent<TNotifyEvent>;
    {$ENDREGION}

    procedure PollTimerTimer(Sender: TObject);
    procedure DoChange; virtual;

    property PollTimer: TTimer
      read GetPollTimer;

  public
    class constructor Create;
    class destructor Destroy;

    class property Processes: IDictionary<UInt32, string>
      read GetProcesses;

    constructor Create(
      AManager    : ILogViewerManager;
      const AName : string
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function ToString: string; override;

    property Name: string
      read GetName write SetName;

    property Manager: ILogViewerManager
      read GetManager;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property SubscriberList: IDictionary<UInt32, ISubscriber>
      read GetSubscriberList;

    property OnChange: IEvent<TNotifyEvent>
      read GetOnChange;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Utils.Winapi, DDuce.Logger;

{$REGION 'class construction and destruction'}
class constructor TChannelReceiver.Create;
begin
  FProcesses.Create(function: IDictionary<UInt32, string>
    begin
      Result := TCollections.CreateDictionary<UInt32, string>;
    end
  );
end;

class destructor TChannelReceiver.Destroy;
begin
  FProcesses := nil;
end;
{$ENDREGION}

{$REGION 'class property access methods'}
class function TChannelReceiver.GetProcesses: IDictionary<UInt32, string>;
begin
  Result := FProcesses.Value;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TChannelReceiver.Create(AManager: ILogViewerManager;
  const AName: string);
begin
  Guard.CheckNotNull(AManager, 'AManager');
  FManager := AManager;
  if AName = '' then
  begin
    Name := Copy(ClassName, 2, Length(ClassName));
  end
  else
    Name := AName;
end;

procedure TChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FSubscriberList := TCollections.CreateDictionary<UInt32, ISubscriber>;
  FPollTimer.Create(function: TTimer
    begin
      Result := TTimer.Create(nil);
      Result.Interval := 100;
      Result.OnTimer  := PollTimerTimer;
    end
  );
end;

destructor TChannelReceiver.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  if FPollTimer.IsValueCreated then
    FPollTimer.Value.Free;
  FSubscriberList.OnChanged.Clear;
  FSubscriberList.OnValueChanged.Clear;
  FSubscriberList.OnKeyChanged.Clear;
  FSubscriberList.Clear;
  FSubscriberList := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TChannelReceiver.DoChange;
begin
  FOnChange.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TChannelReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TChannelReceiver.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    Logger.Send(Name, FEnabled);
    DoChange;
  end;
end;

function TChannelReceiver.GetManager: ILogViewerManager;
begin
  Result := FManager;
end;

function TChannelReceiver.GetName: string;
begin
  Result := FName;
end;

procedure TChannelReceiver.SetName(const Value: string);
begin
  if Value <> Name then
  begin
    FName := Value;
    DoChange;
  end;
end;

function TChannelReceiver.GetOnChange: IEvent<TNotifyEvent>;
begin
  Result := FOnChange;
end;

function TChannelReceiver.GetPollTimer: TTimer;
begin
  Result := FPollTimer.Value;
end;

function TChannelReceiver.GetSubscriberList: IDictionary<UInt32, ISubscriber>;
begin
  Result := FSubscriberList;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TChannelReceiver.PollTimerTimer(Sender: TObject);
var
  LSubscriber : ISubscriber;
begin
  for LSubscriber in SubscriberList.Values do
  begin
    if LSubscriber.Enabled then
      LSubscriber.Poll;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TChannelReceiver.ToString: string;
begin
  Result := Name;
end;
{$ENDREGION}

end.
