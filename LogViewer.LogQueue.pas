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

unit LogViewer.LogQueue;

{ Queues messages dispatched by a channel receiver instance. }

interface

uses
  System.Classes,

  Spring,

  LogViewer.Interfaces;

type
  TLogQueue = class(TInterfacedObject, ILogQueue)
  private
    FReceiver         : Weak<IChannelReceiver>; // weak reference!
    FOnReceiveMessage : Event<TReceiveMessageEvent>;
    FSourceId         : Integer;
    FSourceName       : string;
    FEnabled          : Boolean;

    {$REGION 'property access methods'}
    function GetSourceId: Integer;
    function GetReceiver: IChannelReceiver;
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetSourceName: string;
    {$ENDREGION}

  public
    constructor Create(
      const AReceiver   : IChannelReceiver;
      ASourceId         : Integer;
      const ASourceName : string
    );
    procedure BeforeDestruction; override;

    procedure DoReceiveMessage(AStream : TStream);

    property SourceId: Integer
      read GetSourceId;

    property SourceName: string
      read GetSourceName;

    property Receiver : IChannelReceiver
      read GetReceiver;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TLogQueue.Create(const AReceiver: IChannelReceiver;
  ASourceId: Integer; const ASourceName: string);
begin
  inherited Create;
  FOnReceiveMessage.UseFreeNotification := False;
  FReceiver := AReceiver;
  FSourceId := ASourceId;
  FSourceName := ASourceName;
  FEnabled  := True;
end;

procedure TLogQueue.BeforeDestruction;
begin
  FOnReceiveMessage.Clear;
  FOnReceiveMessage := nil;
  FReceiver := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogQueue.GetSourceId: Integer;
begin
  Result := FSourceId;
end;

function TLogQueue.GetSourceName: string;
begin
  Result := FSourceName;
end;

function TLogQueue.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TLogQueue.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TLogQueue.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
  Result := FOnReceiveMessage;
end;

function TLogQueue.GetReceiver: IChannelReceiver;
begin
  Result := FReceiver.Target;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TLogQueue.DoReceiveMessage(AStream: TStream);
begin
  if Enabled then
    FOnReceiveMessage.Invoke(Self, AStream);
end;
{$ENDREGION}

end.
