{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Dashboard.Data;

interface

uses
  System.Classes,
  Vcl.Forms,

  LogViewer.Interfaces;

type
  TDashboardData = class
  private
    FReceiver   : IChannelReceiver;
    FSubscriber : ISubscriber;
    FCaption    : string;

  protected
    {$REGION 'property access methods'}
    function GetSubscriber: ISubscriber;
    procedure SetSubscriber(const Value: ISubscriber);
    function GetReceiver: IChannelReceiver;
    procedure SetReceiver(const Value: IChannelReceiver);
    function GetCaption: string;
    {$ENDREGION}

  public
    constructor Create(
      AReceiver   : IChannelReceiver;
      ASubscriber : ISubscriber = nil
    );
    procedure BeforeDestruction; override;

    property Caption: string
      read GetCaption;

    property Receiver: IChannelReceiver
      read GetReceiver write SetReceiver;

    property Subscriber: ISubscriber
      read GetSubscriber write SetSubscriber;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Logger,

  LogViewer.Resources;

{$REGION 'construction and destruction'}
constructor TDashboardData.Create(AReceiver: IChannelReceiver;
  ASubscriber: ISubscriber);
begin
  if not Assigned(ASubscriber) then
  begin
    Receiver := AReceiver;
    if Supports(Receiver, IWinIPC) then
    begin
      FCaption := SReceiverCaptionWinIPC;
    end
    else if Supports(Receiver, IZMQ) then
    begin
      FCaption := SReceiverCaptionZeroMQ;
    end
    else if Supports(Receiver, IMQTT) then
    begin
      FCaption := SReceiverCaptionMQTT;
    end
    else if Supports(Receiver, IWinODS) then
    begin
      FCaption := SReceiverCaptionWinODS;
    end
    else if Supports(Receiver, IComPort) then
    begin
      FCaption := SReceiverCaptionComPort;
    end
    else if Supports(Receiver, IFileSystem) then
    begin
      FCaption := SReceiverCaptionFileSystem;
    end;
  end;
  Subscriber := ASubscriber;
  if Assigned(Subscriber) then
  begin
    FCaption := Subscriber.SourceName;
  end;
end;

procedure TDashboardData.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FReceiver   := nil;
  FSubscriber := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TDashboardData.GetCaption: string;
begin
  Result := FCaption;
end;

function TDashboardData.GetReceiver: IChannelReceiver;
begin
  Result := FReceiver;
end;

procedure TDashboardData.SetReceiver(const Value: IChannelReceiver);
begin
  FReceiver := Value;
end;

function TDashboardData.GetSubscriber: ISubscriber;
begin
  Result := FSubscriber;
end;

procedure TDashboardData.SetSubscriber(const Value: ISubscriber);
begin
  FSubscriber := Value;
end;
{$ENDREGION}

end.

