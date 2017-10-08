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

unit LogViewer.Receivers.ComPort;

{ Receives data from a serial port. The data is queued as a TLogMessage
  compatible stream.

  For now the serial data is handled as:
    - a watch message if the text has a format like '%s=%s' (name-value pair
      with '=' as delimiter)
    - a text message
}

interface

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  synaser,

  LogViewer.Interfaces, LogViewer.ComPort.Settings;

type
  TComPortChannelReceiver = class(TInterfacedObject, IChannelReceiver)
  private class var
    FCounter : Integer;
  private
    FEnabled          : Boolean;
    FOnReceiveMessage : Event<TReceiveMessageEvent>;
    FSerialPort       : TBlockSerial;
    FPollTimer        : TTimer;
    FSettings         : TComPortSettings;
    FBuffer           : TMemoryStream;
    FName             : string;

    procedure FSerialPortStatus(
      Sender      : TObject;
      Reason      : THookSerialReason;
      const Value : string
    );
    procedure FPollTimerTimer(Sender: TObject);
    function GetSettings: TComPortSettings;
    function GetName: string;
    procedure SetName(const Value: string);

  protected
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;

    procedure DoReceiveMessage(AStream : TStream);
    procedure DoStringReceived(const AString: AnsiString);

    procedure FSettingsChanged(Sender: TObject);

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property Settings: TComPortSettings
      read GetSettings;

    property Name: string
      read GetName write SetName;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;

  public
    constructor Create(
      const AName : string;
      ASettings   : TComPortSettings
    ); reintroduce;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.SysUtils, System.AnsiStrings,

  DDuce.Logger.Interfaces;

{$REGION 'construction and destruction'}
constructor TComPortChannelReceiver.Create(const AName : string;
  ASettings: TComPortSettings);
begin
  inherited Create;
  if AName = '' then
  begin
    FName := Copy(ClassName, 2, Length(ClassName)) + IntToStr(FCounter);
  end
  else
    FName := AName;
  FSettings := TComPortSettings.Create;
  FSettings.Assign(ASettings);
end;

procedure TComPortChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  Inc(FCounter);
  FBuffer := TMemoryStream.Create;
  FPollTimer := TTimer.Create(nil);
  FPollTimer.Interval := 1;
  FPollTimer.Enabled := False;
  FPollTimer.OnTimer := FPollTimerTimer;
  FSerialPort := TBlockSerial.Create;
  FSerialPort.OnStatus := FSerialPortStatus;
  FSettings.OnChanged.Add(FSettingsChanged);
end;

procedure TComPortChannelReceiver.BeforeDestruction;
begin
  FSerialPort.Free;
  FBuffer.Free;
  FPollTimer.Free;
  FSettings.OnChanged.Clear;
  FSettings.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TComPortChannelReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TComPortChannelReceiver.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    if Value then
    begin
      FSerialPort.Connect(FSettings.Port);
      FSerialPort.Config(
        FSettings.BaudRate,
        FSettings.DataBits,
        FSettings.Parity,
        FSettings.StopBits,
        False,
        True
      );
      FPollTimer.Enabled := True;
    end
    else
    begin
      if FSerialPort.InstanceActive then
        FSerialPort.CloseSocket;
      FPollTimer.Enabled := False;
    end;
  end;
end;

function TComPortChannelReceiver.GetName: string;
begin
  Result := FName;
end;

procedure TComPortChannelReceiver.SetName(const Value: string);
begin
  FName := Value;
end;

function TComPortChannelReceiver.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
  Result := FOnReceiveMessage;
end;

function TComPortChannelReceiver.GetSettings: TComPortSettings;
begin
  Result := FSettings;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TComPortChannelReceiver.DoReceiveMessage(AStream: TStream);
begin
  FOnReceiveMessage.Invoke(Self, AStream);
end;

procedure TComPortChannelReceiver.DoStringReceived(const AString: AnsiString);
const
  LZero : Integer = 0;
var
  LMsgType   : Integer;
  LTimeStamp : TDateTime;
  LTextSize  : Integer;
  LString    : UTF8String;
begin
  FBuffer.Clear;
  if ContainsStr(AString, '=') then
  begin
    LMsgType := Integer(lmtWatch);
  end
  else
    LMsgType := Integer(lmtText);
  LString    := UTF8String(AString);
  LTextSize  := Length(LString);
  LTimeStamp := Now;
  FBuffer.Seek(0, soFromBeginning);
  FBuffer.WriteBuffer(LMsgType, SizeOf(Integer));
  FBuffer.WriteBuffer(LTimeStamp, SizeOf(TDateTime));
  FBuffer.WriteBuffer(LTextSize, SizeOf(Integer));
  FBuffer.WriteBuffer(LString[1], LTextSize);
  FBuffer.WriteBuffer(LZero, SizeOf(Integer));
  DoReceiveMessage(FBuffer);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TComPortChannelReceiver.FPollTimerTimer(Sender: TObject);
var
  S : AnsiString;
begin
  while FSerialPort.WaitingDataEx <> 0 do
  begin
    S := Trim(FSerialPort.RecvTerminated(10, #10));
    DoStringReceived(S);
  end;
end;

procedure TComPortChannelReceiver.FSerialPortStatus(Sender: TObject;
  Reason: THookSerialReason; const Value: string);
var
  S: string;
begin
  case Reason of
    HR_SerialClose: S := 'Serial Close';
    HR_Connect:     S := 'Connect';
    HR_CanRead:     S := 'CanRead';
    HR_CanWrite:    S := 'CanWrite';
    HR_ReadCount:   S := 'ReadCount';
    HR_WriteCount:  S := 'WriteCount';
    HR_Wait:        S := 'Wait';
  end;
  if S <> '' then
  begin
//    sbrMain.SimpleText := Format('%s %s', [S, Value]);
//    FLogIn.LogFmt(
//      '%s %s',
//      [S, Value]
//    );
  end;
end;

procedure TComPortChannelReceiver.FSettingsChanged(Sender: TObject);
begin
  if FSerialPort.Device <> FSettings.Port then
  begin
    if FSerialPort.InstanceActive then
    begin
      FSerialPort.CloseSocket;
      FSerialPort.Connect(FSettings.Port);
    end;
  end;
  FSerialPort.Config(
    FSettings.BaudRate,
    FSettings.DataBits,
    FSettings.Parity,
    FSettings.StopBits,
    False,
    True
  );
end;
{$ENDREGION}

end.
