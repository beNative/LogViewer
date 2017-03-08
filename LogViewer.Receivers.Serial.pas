{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Receivers.Serial;

{ Receives data from a serial port. The data is queued as a TLogMessage
  compatible stream. }

interface

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  synaser,

  LogViewer.Interfaces;

type
  TSerialPortReceiver = class(TInterfacedObject, IChannelReceiver)
  private
    FEnabled          : Boolean;
    FOnReceiveMessage : Event<TReceiveMessageEvent>;
    FComPort          : TBlockSerial;

  protected
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;

    procedure DoReceiveMessage(AStream : TStream);

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;

  end;

implementation

{$REGION 'property access methods'}
function TSerialPortReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TSerialPortReceiver.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TSerialPortReceiver.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
  Result := FOnReceiveMessage;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TSerialPortReceiver.DoReceiveMessage(AStream: TStream);
begin
  FOnReceiveMessage.Invoke(Self, AStream);
end;
{$ENDREGION}

end.
