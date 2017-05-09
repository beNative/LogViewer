{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.ComPort.Settings;

interface

uses
  System.Classes,

  Spring,

  synaser;

const
  DEFAULT_PARITY    = 'N';
  DEFAULT_BAUDRATE  = 115200;
  DEFAULT_DATA_BITS = 8;
  DEFAULT_STOP_BITS = SB1;

type
  TComPortSettings = class(TPersistent)
  private
    FBaudRate  : Integer;
    FDataBits  : Integer;
    FStopBits  : Integer;
    FParity    : Char;
    FPort      : string;
    FOnChanged : Event<TNotifyEvent>;

    function GetBaudRate: Integer;
    procedure SetBaudRate(const Value: Integer);
    function GetDataBits: Integer;
    function GetPort: string;
    function GetStopBits: Integer;
    procedure SetDataBits(const Value: Integer);
    procedure SetPort(const Value: string);
    procedure SetStopBits(const Value: Integer);
    function GetParity: Char;
    procedure SetParity(const Value: Char);
    function GetOnChanged: IEvent<TNotifyEvent>;

  protected
    procedure Changed;

    procedure Assign(Source: TPersistent); override;

  public
    procedure AfterConstruction; override;

    property BaudRate: Integer
      read GetBaudRate write SetBaudRate default DEFAULT_BAUDRATE;

    property Port: string
      read GetPort write SetPort;

    property DataBits: Integer
      read GetDataBits write SetDataBits default DEFAULT_DATA_BITS;

    { SB1, SB15, SB2 }
    property StopBits: Integer
      read GetStopBits write SetStopBits default DEFAULT_STOP_BITS;

    { N - None, O - Odd, E - Even, M - Mark or S - Space.}
    property Parity: Char
      read GetParity write SetParity default DEFAULT_PARITY;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;
  end;

implementation

{$REGION 'property access methods'}
procedure TComPortSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FParity   := DEFAULT_PARITY;
  FBaudRate := DEFAULT_BAUDRATE;
  FDataBits := DEFAULT_DATA_BITS;
  FStopBits := DEFAULT_STOP_BITS;
end;

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

function TComPortSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

procedure TComPortSettings.SetDataBits(const Value: Integer);
begin
  if Value <> DataBits then
  begin
    FDataBits := Value;
    Changed;
  end;
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

{$REGION 'protected methods'}
procedure TComPortSettings.Assign(Source: TPersistent);
var
  CPS: TComPortSettings;
begin
  if Source is TComPortSettings then
  begin
    CPS := TComPortSettings(Source);
    FBaudRate := CPS.BaudRate;
    FDataBits := CPS.DataBits;
    FStopBits := CPS.StopBits;
    FParity   := CPS.Parity;
    FPort     := CPS.Port;
  end
  else
    inherited Assign(Source);
end;

procedure TComPortSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

end.
