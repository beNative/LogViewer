{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

  DDuce.Logger.Interfaces,

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
    FOnChange         : Event<TNotifyEvent>;
    FLogMessageLevels : TLogMessageLevels;
    FLogMessageTypes  : TLogMessageTypes;

  protected
    {$REGION 'property access methods'}
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
    function GetKey: string; virtual;
    function GetMessageCount: Int64; virtual;
    function GetReceiver: IChannelReceiver;
    function GetEnabled: Boolean; virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    function GetSourceId: UInt32; virtual;
    function GetSourceName: string;
    function GetOnChange: IEvent<TNotifyEvent>;
    function GetLogMessageLevels: TLogMessageLevels;
    function GetLogMessageTypes: TLogMessageTypes;
    procedure SetLogMessageLevels(const Value: TLogMessageLevels); virtual;
    procedure SetLogMessageTypes(const Value: TLogMessageTypes); virtual;
    {$ENDREGION}

    procedure Poll; virtual;
    procedure Reset; virtual;
    procedure Close; virtual;

    procedure DoReceiveMessage(AStream: TStream); virtual;
    procedure DoChange; virtual;

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

    property LogMessageTypes: TLogMessageTypes
      read GetLogMessageTypes write SetLogMessageTypes;

    property LogMessageLevels: TLogMessageLevels
      read GetLogMessageLevels write SetLogMessageLevels;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;

    property OnChange: IEvent<TNotifyEvent>
      read GetOnChange;

  public
    constructor Create(
      const AReceiver   : IChannelReceiver;
      ASourceId         : Integer;
      const AKey        : string;
      const ASourceName : string;
      AEnabled          : Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TSubscriber.Create(const AReceiver: IChannelReceiver;
  ASourceId: Integer; const AKey, ASourceName: string; AEnabled: Boolean);
begin
  inherited Create;
  Guard.CheckNotNull(AReceiver, 'AReceiver');
  FLogMessageTypes  := AllMessages;
  FLogMessageLevels := AllLevels;
  FReceiver   := AReceiver;
  FSourceId   := ASourceId;
  FKey        := AKey;
  FSourceName := ASourceName;
  Enabled     := AEnabled;
end;

destructor TSubscriber.Destroy;
begin
  FOnReceiveMessage.Clear;
  FReceiver := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TSubscriber.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TSubscriber.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    DoChange;
  end;
end;

function TSubscriber.GetLogMessageLevels: TLogMessageLevels;
begin
  Result := FLogMessageLevels;
end;

procedure TSubscriber.SetLogMessageLevels(const Value: TLogMessageLevels);
begin
  if Value <> LogMessageLevels then
  begin
    FLogMessageLevels := Value;
    DoChange;
  end;
end;

function TSubscriber.GetLogMessageTypes: TLogMessageTypes;
begin
  Result := FLogMessageTypes;
end;

procedure TSubscriber.SetLogMessageTypes(const Value: TLogMessageTypes);
begin
  if Value <> LogMessageTypes then
  begin
    FLogMessageTypes := Value;
    DoChange;
  end;
end;

function TSubscriber.GetKey: string;
begin
  Result := FKey;
end;

function TSubscriber.GetMessageCount: Int64;
begin
  Result := FMessageCount;
end;

function TSubscriber.GetOnChange: IEvent<TNotifyEvent>;
begin
  Result := FOnChange;
end;

function TSubscriber.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
  Result := FOnReceiveMessage;
end;

function TSubscriber.GetReceiver: IChannelReceiver;
begin
  Exit(FReceiver);
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

{$REGION 'event dispatch methods'}
procedure TSubscriber.DoChange;
begin
  FOnChange.Invoke(Self);
end;

procedure TSubscriber.DoReceiveMessage(AStream: TStream);
begin
  Guard.CheckNotNull(AStream, 'AStream');
  if Enabled then
  begin
    Inc(FMessageCount);
    FOnReceiveMessage.Invoke(Self, AStream);
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TSubscriber.Close;
begin
// implemented in descendants
end;

procedure TSubscriber.Poll;
begin
// implemented in descendants
end;

procedure TSubscriber.Reset;
begin
  FMessageCount := 0;
  DoChange;
end;
{$ENDREGION}

end.
