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

  LogViewer.Interfaces, LogViewer.ComPort.Settings;

type
  TSerialPortReceiver = class(TInterfacedObject, IChannelReceiver, IComPortSettings)
  private
    FEnabled          : Boolean;
    FOnReceiveMessage : Event<TReceiveMessageEvent>;
    FSerialPort       : TBlockSerial;
    FPollTimer        : TTimer;
    FSettings         : TComPortSettings;
    FBuffer           : TMemoryStream;

    procedure FSerialPortStatus(
      Sender      : TObject;
      Reason      : THookSerialReason;
      const Value : string
    );
    procedure FPollTimerTimer(Sender: TObject);
    function GetSettings: TComPortSettings;

  protected
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;

    procedure DoReceiveMessage(AStream : TStream);
    procedure DoStringReceived(const AString: RawByteString);

    procedure FSettingsChanged(Sender: TObject);

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property Settings: TComPortSettings
      read GetSettings;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.SysUtils, System.StrUtils,

  DDuce.Logger.Interfaces;

{$REGION 'construction and destruction'}
procedure TSerialPortReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings := TComPortSettings.Create;
  FSettings.Port := 'COM3';
  FBuffer := TMemoryStream.Create;
  FPollTimer := TTimer.Create(nil);
  FPollTimer.Interval := 1;
  FPollTimer.Enabled := False;
  FPollTimer.OnTimer := FPollTimerTimer;
  FSerialPort := TBlockSerial.Create;
  FSerialPort.OnStatus := FSerialPortStatus;
  FSettings.OnChanged.Add(FSettingsChanged);
end;

procedure TSerialPortReceiver.BeforeDestruction;
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
function TSerialPortReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TSerialPortReceiver.SetEnabled(const Value: Boolean);
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
      FPollTimer.Enabled := False;
    end;
  end;
end;

function TSerialPortReceiver.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
  Result := FOnReceiveMessage;
end;
function TSerialPortReceiver.GetSettings: TComPortSettings;
begin
  //Result :=
end;

{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TSerialPortReceiver.DoReceiveMessage(AStream: TStream);
begin
  FOnReceiveMessage.Invoke(Self, AStream);
end;

procedure TSerialPortReceiver.DoStringReceived(const AString: RawByteString);
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
    LMsgType := Integer(lmtValue);
  end
  else
    LMsgType := Integer(lmtText);

  LString := AString;
  LTextSize := Length(LString);
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
procedure TSerialPortReceiver.FPollTimerTimer(Sender: TObject);
var
  SS : TStringStream;
begin
  while FSerialPort.WaitingDataEx <> 0 do
  begin
    //DoStringReceived(FSerialPort.RecvPacket(0));
    DoStringReceived(FSerialPort.RecvTerminated(10, #13#10));
  end;
end;

procedure TSerialPortReceiver.FSerialPortStatus(Sender: TObject;
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

procedure TSerialPortReceiver.FSettingsChanged(Sender: TObject);
var
  B : Boolean;
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
