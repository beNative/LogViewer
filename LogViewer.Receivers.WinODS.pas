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

unit LogViewer.Receivers.WinODS;

interface

{ Receives messages posted by the OutputDebugString Windows API routine. }

uses
  LogViewer.Interfaces,

  Spring;

type
  TWinODShannelReceiver = class(TInterfacedObject, IChannelReceiver)
  private
     FEnabled   : Boolean;

  protected
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;


    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TWinODShannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TWinODShannelReceiver.BeforeDestruction;
begin

  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWinODShannelReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TWinODShannelReceiver.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
//
end;

procedure TWinODShannelReceiver.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
  end;
end;
{$ENDREGION}

end.
