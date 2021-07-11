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

unit LogBroker.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.WinXCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls,

  ZeroMQ.API, ZeroMQ,

  DDuce.Logger.Channels.ZeroMQ, DDuce.Logger.Interfaces, DDuce.WinIPC.Server;

type
  TfrmMain = class(TForm)
    chkIPCAutoPort : TCheckBox;
    edtIPCPort     : TLabeledEdit;
    lblIPC         : TLabel;
    shpStat        : TShape;
    tsIPC          : TToggleSwitch;

    procedure chkIPCAutoPortClick(Sender: TObject);
    procedure tsIPCClick(Sender: TObject);

  private
    FIPCServer : TWinIPCServer;
    FZMQ       : IZeroMQ;
    FPublisher : IZMQPair;
    FEndPoint  : string;
    FBuffer    : TStringStream;

    procedure FIPCServerMessage(
      Sender    : TObject;
      ASourceId : UInt32;
      AData     : TStream
    );

    function Connect: Boolean;
    function Disconnect: Boolean;
    procedure LoadSettings;
    procedure SaveSettings;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

var
  frmMain: TfrmMain;

implementation

uses
  LogBroker.Settings,

  Spring, Spring.SystemUtils;

{$R *.dfm}

{$REGION 'non-interfaced routines'}
procedure EnsureZMQLibExists;
const
  LIBZMQ = 'libzmq';
var
  LResStream  : TResourceStream;
  LFileStream : TFileStream;
  LPath       : string;
begin
  LPath := Format('%s\%s.dll', [ExtractFileDir(ParamStr(0)), LIBZMQ]);
  if not FileExists(LPath) then
  begin
    LResStream := TResourceStream.Create(HInstance, LIBZMQ, RT_RCDATA);
    try
      LFileStream := TFileStream.Create(LPath, fmCreate);
      try
        LFileStream.CopyFrom(LResStream, 0);
      finally
        LFileStream.Free;
      end;
    finally
      LResStream.Free;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FEndPoint := 'tcp://*:5555';
  FIPCServer := TWinIPCServer.Create;
  FIPCServer.OnMessage := FIPCServerMessage;
  FIPCServer.Active    := True;
  FZMQ := TZeroMQ.Create;
  FBuffer := TStringStream.Create('', TEncoding.ANSI);
  LoadSettings;
end;

destructor TfrmMain.Destroy;
begin
  SaveSettings;
  FIPCServer.Free;
  if Assigned(FPublisher) then
  begin
    FPublisher.Close;
    FPublisher := nil;
  end;
  FZMQ := nil;
  FBuffer.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.FIPCServerMessage(Sender: TObject; ASourceId: UInt32;
  AData: TStream);
begin
  if Assigned(FPublisher) then
  begin
    shpStat.Brush.Color := $004080FF;
    AData.Position := 0;
    FBuffer.Clear;
    FBuffer.LoadFromStream(AData);
    if FPublisher.SendString(FBuffer.DataString) > 0 then
    begin
      shpStat.Brush.Color := clGreen;
    end;
  end;
end;

procedure TfrmMain.tsIPCClick(Sender: TObject);
begin
  if tsIPC.State = tssOn then
  begin
    Connect;
  end
  else
  begin
    Disconnect;
  end;
end;

procedure TfrmMain.chkIPCAutoPortClick(Sender: TObject);
begin
  edtIPCPort.Enabled := not (Sender as TCheckBox).Checked;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TfrmMain.Connect: Boolean;
const
  ENDPOINT = 'tcp://*:%s';
var
  S : string;
  A : TStringDynArray;
begin
  if Assigned(FPublisher) then
    FPublisher.Close;

  FPublisher := FZMQ.Start(ZMQSocket.Publisher);
  if chkIPCAutoPort.Checked then
  begin
    Result := FPublisher.Bind(Format(ENDPOINT, ['*'])) = 0;
    edtIPCPort.Text := FPublisher.LastEndPoint;
  end
  else
  begin
    Result := FPublisher.Bind(Format(ENDPOINT, [edtIPCPort.Text])) = 0;
  end;
  // Physical connection is always a bit after Connected reports True.
  // https://stackoverflow.com/questions/11634830/zeromq-always-loses-the-first-message
  Sleep(100);
  if Result then
  begin
    S := FPublisher.LastEndPoint;
    A := SplitString(S, [':']);
    edtIPCPort.Text := A[High(A)];
  end;
end;

function TfrmMain.Disconnect: Boolean;
begin
  Result := FPublisher.Close;
end;
procedure TfrmMain.LoadSettings;
begin
  chkIPCAutoPort.Checked := Settings.ReadBool('', 'IPCAutoPort');
  edtIPCPort.Text        := Settings.ReadString('', 'IPCPort', '5555');
  if Settings.ReadBool('', 'IPCEnabled') then
    tsIPC.State := tssOn
  else
    tsIPC.State := tssOff;

  edtIPCPort.Enabled := not chkIPCAutoPort.Checked;
end;

procedure TfrmMain.SaveSettings;
begin
  Settings.WriteBool('', 'IPCAutoPort', chkIPCAutoPort.Checked);
  Settings.WriteString('', 'IPCPort', edtIPCPort.Text);
  Settings.WriteBool('', 'IPCEnabled', tsIPC.State = tssOn);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  if tsIPC.State = tssOn then
  begin
    tsIPC.ThumbColor := clGreen;
    tsIPC.FrameColor := clGreen;
  end
  else
  begin
    tsIPC.ThumbColor := clBlack;
    tsIPC.FrameColor := clBlack;
  end;
  shpStat.Brush.Color := clWhite;
end;
{$ENDREGION}

initialization
  EnsureZMQLibExists;

end.
