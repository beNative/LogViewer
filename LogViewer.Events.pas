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

unit LogViewer.Events;

{ Dispatches and handles events on the active view. }

interface

uses
  Spring,

  LogViewer.Interfaces;

type
  TLogViewerEvents = class(TInterfaceBase, ILogViewerEvents) // no refcount
  private
    FManager        : ILogViewerManager;
    FOnAddLogViewer : Event<TLogViewerEvent>;
    FOnAddReceiver  : Event<TChannelReceiverEvent>;

  protected
    {$REGION 'property access methods'}
    function GetOnAddLogViewer: IEvent<TLogViewerEvent>;
    function GetOnAddReceiver: IEvent<TChannelReceiverEvent>;
    {$ENDREGION}

    procedure DoAddLogViewer(ALogViewer: ILogViewer); virtual;
    procedure DoAddReceiver(AReceiver: IChannelReceiver); virtual;

  public
    constructor Create(AManager: ILogViewerManager);
    procedure BeforeDestruction; override;

    property OnAddLogViewer: IEvent<TLogViewerEvent>
      read GetOnAddLogViewer;

    property OnAddReceiver: IEvent<TChannelReceiverEvent>
      read GetOnAddReceiver;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TLogViewerEvents.Create(AManager: ILogViewerManager);
begin
  FManager := AManager;
end;

procedure TLogViewerEvents.BeforeDestruction;
begin
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogViewerEvents.GetOnAddLogViewer: IEvent<TLogViewerEvent>;
begin
  Result := FOnAddLogViewer;
end;

function TLogViewerEvents.GetOnAddReceiver: IEvent<TChannelReceiverEvent>;
begin
  Result := FOnAddReceiver;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TLogViewerEvents.DoAddLogViewer(ALogViewer: ILogViewer);
begin
  FOnAddLogViewer.Invoke(Self, ALogViewer);
end;

procedure TLogViewerEvents.DoAddReceiver(AReceiver: IChannelReceiver);
begin
  FOnAddReceiver.Invoke(Self, AReceiver);
end;
{$ENDREGION}

end.
