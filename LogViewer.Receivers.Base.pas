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

{ Base class for all channel receivers (implementing IChannelReceiver). }

{ A IChannelReceiver instance maintains a dictionary of subscribers
  (ISubscriber) with SourceId as the key. }

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
    FSubscriberList : IDictionary<Integer, ISubscriber>;
    FManager        : ILogViewerManager;

  protected
    {$REGION 'property access methods'}
    function GetManager: ILogViewerManager;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean); virtual;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetSubscriberList: IDictionary<Integer, ISubscriber>;
    {$ENDREGION}

    function CreateSubscriber(
      ASourceId         : Integer;
      AThreadId         : Integer;
      const ASourceName : string
    ): ISubscriber; virtual;
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
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function ToString: string; override;

    property Name: string
      read GetName write SetName;

    property Manager: ILogViewerManager
      read GetManager;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property SubscriberList: IDictionary<Integer, ISubscriber>
      read GetSubscriberList;

  end;

implementation

uses
  System.SysUtils,

  DDuce.Utils.Winapi, DDuce.Logger,

  LogViewer.Factories;

{$REGION 'construction and destruction'}
procedure TChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FSubscriberList := TCollections.CreateDictionary<Integer, ISubscriber>;
end;

procedure TChannelReceiver.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  Logger.AddCheckPoint;
  FSubscriberList.OnChanged.Clear;
  FSubscriberList.OnValueChanged.Clear;
  FSubscriberList.OnKeyChanged.Clear;
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

function TChannelReceiver.CreateSubscriber(ASourceId: Integer; AThreadId: Integer;
  const ASourceName : string): ISubscriber;
begin
  Result := nil;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TChannelReceiver.DoReceiveMessage(AStream: TStream; ASourceId: Integer;
  AThreadId: Integer; const ASourceName : string);
var
  LSubscriber : ISubscriber;
begin
  if not FSubscriberList.TryGetValue(ASourceId, LSubscriber) then
  begin
    Logger.Info('CreateSubscriber(%d, %d, %s)', [ASourceId, AThreadId, ASourceName]);
    LSubscriber := CreateSubscriber(ASourceId, AThreadId, ASourceName);
    FSubscriberList.AddOrSetValue(ASourceId, LSubscriber);
  end;
  LSubscriber.DoReceiveMessage(AStream);
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

function TChannelReceiver.GetSubscriberList: IDictionary<Integer, ISubscriber>;
begin
  Result := FSubscriberList;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TChannelReceiver.ToString: string;
begin
  Result := Name;
end;
{$ENDREGION}

end.
