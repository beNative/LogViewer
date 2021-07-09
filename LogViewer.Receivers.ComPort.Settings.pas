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

unit LogViewer.Receivers.ComPort.Settings;

{ Persistable settings for ComPort subscriber. }

interface

uses
  System.Classes,

  Spring;

type
  TComPortSettings = class(TPersistent)
  const
    //DEFAULT_BAUDRATE         = 9600;
    DEFAULT_BAUDRATE         = 115200;
    DEFAULT_POLLING_INTERVAL = 50;

  private
    FBaudRate        : Integer;
    FDataBits        : Integer;
    FStopBits        : Integer;
    FParity          : Char;
    FPort            : string;
    FPollingInterval : Integer;
    FOnChanged       : Event<TNotifyEvent>;

    {$REGION 'property access methods'}
    function GetBaudRate: Integer;
    procedure SetBaudRate(const Value: Integer);
    function GetPort: string;
    procedure SetPort(const Value: string);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetPollingInterval: Integer;
    procedure SetPollingInterval(const Value: Integer);
    function GetDataBits: Integer;
    procedure SetDataBits(const Value: Integer);
    function GetStopBits: Integer;
    procedure SetStopBits(const Value: Integer);
    function GetParity: Char;
    procedure SetParity(const Value: Char);
    {$ENDREGION}

  protected
    procedure Changed;

  public
    procedure AfterConstruction; override;

    procedure Assign(Source: TPersistent); override;

    property BaudRate: Integer
      read GetBaudRate write SetBaudRate default DEFAULT_BAUDRATE;

    property DataBits: Integer
      read GetDataBits write SetDataBits;

    property StopBits: Integer
      read GetStopBits write SetStopBits;

    property Parity: Char
      read GetParity write SetParity;

    property Port: string
      read GetPort write SetPort;

    property PollingInterval: Integer // in ms
      read GetPollingInterval write SetPollingInterval
      default DEFAULT_POLLING_INTERVAL;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TComPortSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FBaudRate        := DEFAULT_BAUDRATE;
  FPollingInterval := DEFAULT_POLLING_INTERVAL;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TComPortSettings.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

procedure TComPortSettings.SetBaudRate(const Value: Integer);
begin
  if Value <> BaudRate then
  begin
    FBaudRate := Value;
    Changed;
  end;
end;

function TComPortSettings.GetDataBits: Integer;
begin
  Result := FDataBits;
end;

procedure TComPortSettings.SetDataBits(const Value: Integer);
begin
  if Value <> DataBits then
  begin
    FDataBits := Value;
    Changed;
  end;
end;

function TComPortSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TComPortSettings.GetPollingInterval: Integer;
begin
  Result := FPollingInterval;
end;

function TComPortSettings.GetParity: Char;
begin
  Result := FParity;
end;

procedure TComPortSettings.SetParity(const Value: Char);
begin
  if Value <> Parity then
  begin
    FParity := Value;
    Changed;
  end;
end;

procedure TComPortSettings.SetPollingInterval(const Value: Integer);
begin
  if Value <> PollingInterval then
  begin
    FPollingInterval := Value;
    Changed;
  end;
end;

function TComPortSettings.GetPort: string;
begin
  Result := FPort;
end;

procedure TComPortSettings.SetPort(const Value: string);
begin
  if Value <> Port then
  begin
    FPort := Value;
    Changed;
  end;
end;

function TComPortSettings.GetStopBits: Integer;
begin
  Result := FStopBits;
end;

procedure TComPortSettings.SetStopBits(const Value: Integer);
begin
  if Value <> StopBits then
  begin
    FStopBits := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TComPortSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TComPortSettings.Assign(Source: TPersistent);
var
  CPS: TComPortSettings;
begin
  if Source is TComPortSettings then
  begin
    CPS := TComPortSettings(Source);
    FBaudRate        := CPS.BaudRate;
    FPort            := CPS.Port;
    FPollingInterval := CPS.PollingInterval;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
