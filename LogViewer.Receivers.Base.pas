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

  LogViewer.Interfaces;

type
  TChannelReceiver = class(TInterfacedObject, IChannelReceiver)
  private
    FEnabled       : Boolean;
    FName          : string;
    FQueueList     : IDictionary<Integer, ILogQueue>;
    FOnNewLogQueue : Event<TLogQueueEvent>;

  protected
    {$REGION 'property access methods'}
    function GetOnNewLogQueue: IEvent<TLogQueueEvent>;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean); virtual;
    function GetName: string;
    procedure SetName(const Value: string);
    {$ENDREGION}

  public
    constructor Create(const AName: string); reintroduce;

    procedure DoReceiveMessage(
      ASourceId : Integer;
      AStream   : TStream
    ); virtual;

    function ToString: string; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Name: string
      read GetName write SetName;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property OnNewLogQueue: IEvent<TLogQueueEvent>
      read GetOnNewLogQueue;
  end;

implementation

uses
  System.SysUtils,

  LogViewer.Factories;

{$REGION 'construction and destruction'}
procedure TChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FQueueList := TCollections.CreateDictionary<Integer, ILogQueue>;
end;

procedure TChannelReceiver.BeforeDestruction;
begin
  FQueueList.Clear;
  FQueueList := nil;
  inherited BeforeDestruction;
end;

constructor TChannelReceiver.Create(const AName: string);
begin
  if AName = '' then
  begin
    Name := Copy(ClassName, 2, Length(ClassName));
  end
  else
    Name := AName;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TChannelReceiver.DoReceiveMessage(ASourceId: Integer;
  AStream: TStream);
var
  LLogQueue: ILogQueue;
begin
  if not FQueueList.TryGetValue(ASourceId, LLogQueue) then
  begin
    LLogQueue := TLogViewerFactories.CreateLogQueue(ASourceId, Self);
    FQueueList.AddOrSetValue(ASourceId, LLogQueue);
    FOnNewLogQueue.Invoke(Self, LLogQueue);
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

function TChannelReceiver.GetName: string;
begin
  Result := FName;
end;

function TChannelReceiver.GetOnNewLogQueue: IEvent<TLogQueueEvent>;
begin
  Result := FOnNewLogQueue;
end;

procedure TChannelReceiver.SetName(const Value: string);
begin
  FName := Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TChannelReceiver.ToString: string;
begin
  Result := Name;
end;
{$ENDREGION}

end.
