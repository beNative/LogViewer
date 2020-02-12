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

unit LogViewer.Dashboard.Data;

{ Data structure holding the data displayed in the dashboard treeview. }

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
    destructor Destroy; override;

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

  LogViewer.Resources;

{$REGION 'construction and destruction'}
constructor TDashboardData.Create(AReceiver: IChannelReceiver;
  ASubscriber: ISubscriber);
begin
  if not Assigned(ASubscriber) then
  begin
    Receiver := AReceiver;
    if Supports(Receiver, IWinIpc) then
    begin
      FCaption := SReceiverCaptionWinIPC;
    end
    else if Supports(Receiver, IZmq) then
    begin
      FCaption := SReceiverCaptionZeroMQ;
    end
    else if Supports(Receiver, IWinOds) then
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
    end
  end;
  Subscriber := ASubscriber;
  if Assigned(Subscriber) then
  begin
    FCaption := Subscriber.SourceName;
  end;
end;

destructor TDashboardData.Destroy;
begin
  FReceiver   := nil;
  FSubscriber := nil;
  inherited Destroy;
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

