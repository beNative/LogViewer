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

unit LogViewer.Subscribers.Base;

{ Base class for subscribers implementing ISubscriber. }

interface

uses
  System.Classes,

  Spring,

  LogViewer.Interfaces;

type
  TSubscriber = class(TInterfacedObject, ISubscriber)
  private
    FReceiver         : Weak<IChannelReceiver>; // weak reference
    FOnReceiveMessage : Event<TReceiveMessageEvent>;
    FEnabled          : Boolean;
    FMessageCount     : Int64;
    FSourceId         : UInt32;
    FSourceName       : string;
    FKey              : string;

  protected
    {$REGION 'property access methods'}
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
    function GetKey: string; virtual;
    function GetMessageCount: Int64; virtual;
    function GetReceiver: IChannelReceiver;
    function GetEnabled: Boolean; virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    function GetSourceId: UInt32;
    function GetSourceName: string;
    {$ENDREGION}

    procedure Poll; virtual;
    procedure DoReceiveMessage(AStream: TStream); virtual;

    property Key: string
      read GetKey;

    property Receiver: IChannelReceiver
      read GetReceiver;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property MessageCount: Int64
      read GetMessageCount;

    property SourceId: UInt32
      read GetSourceId;

    property SourceName: string
      read GetSourceName;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;

  public
    constructor Create(
      AReceiver         : IChannelReceiver;
      ASourceId         : Integer;
      const AKey        : string;
      const ASourceName : string;
      AEnabled          : Boolean
    );
    procedure BeforeDestruction; override;

  end;

implementation

uses
  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TSubscriber.Create(AReceiver: IChannelReceiver; ASourceId: Integer;
  const AKey, ASourceName: string; AEnabled: Boolean);
begin
  inherited Create;
  Guard.CheckNotNull(AReceiver, 'AReceiver');
  FReceiver   := AReceiver;
  FSourceId   := ASourceId;
  FKey        := AKey;
  FSourceName := ASourceName;
  Enabled     := AEnabled;
end;

procedure TSubscriber.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  Logger.Send('SourceId', SourceId);
  Logger.Send('SourceName', SourceName);
  FOnReceiveMessage.Clear;
  FReceiver := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TSubscriber.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TSubscriber.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TSubscriber.GetKey: string;
begin
  Result := FKey;
end;

function TSubscriber.GetMessageCount: Int64;
begin
  Result := FMessageCount;
end;

function TSubscriber.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
  Result := FOnReceiveMessage;
end;

function TSubscriber.GetReceiver: IChannelReceiver;
begin
  Result := FReceiver.Target;
end;

function TSubscriber.GetSourceId: UInt32;
begin
  Result := FSourceId;
end;

function TSubscriber.GetSourceName: string;
begin
  Result := FSourceName;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TSubscriber.Poll;
begin
  // override in descendants
end;

procedure TSubscriber.DoReceiveMessage(AStream: TStream);
begin
  if Enabled then
  begin
    Inc(FMessageCount);
    FOnReceiveMessage.Invoke(Self, AStream);
  end;
end;
{$ENDREGION}

end.
