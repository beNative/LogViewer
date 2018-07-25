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

unit LogViewer.Receivers.Base;

interface

uses
  System.Classes,

  Spring, Spring.Collections,

  DDuce.DynamicRecord,

  LogViewer.Interfaces;

type
  TChannelReceiver = class(TInterfacedObject, IChannelReceiver)
  private
    FEnabled        : Boolean;
    FName           : string;
    FLogQueueList   : IDictionary<Integer, ILogQueue>;
    FSubscriberList : IList<ISubscriber>;
    FManager        : ILogViewerManager;

  protected
    {$REGION 'property access methods'}
    function GetManager: ILogViewerManager;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean); virtual;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetLogQueueList: IDictionary<Integer, ILogQueue>;
    function GetSubscriberList: IList<ISubscriber>;
    {$ENDREGION}

    procedure SettingsChanged(Sender: TObject); virtual;
    procedure DoReceiveMessage(
      AStream           : TStream;
      ASourceId         : Integer = 0;
      AThreadId         : Integer = 0;
      const ASourceName : string = ''
    ); virtual;

  public
    constructor Create(
      AManager    : ILogViewerManager;
      const AName : string
    ); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddSubscriber(AKeyValues : IDynamicRecord): Boolean; virtual;

    function ToString: string; override;

    property Name: string
      read GetName write SetName;

    property Manager: ILogViewerManager
      read GetManager;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property LogQueueList: IDictionary<Integer, ILogQueue>
      read GetLogQueueList;

    property SubscriberList: IList<ISubscriber>
      read GetSubscriberList;

  end;

implementation

uses
  System.SysUtils,

  DDuce.Utils.Winapi,

  LogViewer.Factories, LogViewer.Receivers.ZeroMQ.Subscriber;

{$REGION 'construction and destruction'}
procedure TChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FLogQueueList   := TCollections.CreateDictionary<Integer, ILogQueue>;
  FSubscriberList := TCollections.CreateInterfaceList<ISubscriber>;
end;

procedure TChannelReceiver.BeforeDestruction;
begin
  FLogQueueList.Clear;
  FLogQueueList := nil;
  FSubscriberList.Clear;
  FSubscriberList := nil;
  inherited BeforeDestruction;
end;

constructor TChannelReceiver.Create(AManager: ILogViewerManager;
  const AName: string);
begin
  FManager := AManager;
  if AName = '' then
  begin
    Name := Copy(ClassName, 2, Length(ClassName));
  end
  else
    Name := AName;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TChannelReceiver.DoReceiveMessage(AStream: TStream; ASourceId: Integer;
  AThreadId: Integer; const ASourceName : string);
var
  LLogQueue : ILogQueue;
begin
  if not FLogQueueList.TryGetValue(ASourceId, LLogQueue) then
  begin
    LLogQueue := TLogViewerFactories.CreateLogQueue(Self, ASourceId, ASourceName);
    FLogQueueList.AddOrSetValue(ASourceId, LLogQueue);
  end;
  LLogQueue.DoReceiveMessage(AStream);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TChannelReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TChannelReceiver.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
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
  FName := Value;
end;

function TChannelReceiver.GetLogQueueList: IDictionary<Integer, ILogQueue>;
begin
  Result := FLogQueueList;
end;

function TChannelReceiver.GetSubscriberList: IList<ISubscriber>;
begin
  Result := FSubscriberList;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TChannelReceiver.SettingsChanged(Sender: TObject);
begin
//
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TChannelReceiver.AddSubscriber(AKeyValues: IDynamicRecord): Boolean;
begin
  Result := False;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TChannelReceiver.ToString: string;
begin
  Result := Name;
end;
{$ENDREGION}

end.
