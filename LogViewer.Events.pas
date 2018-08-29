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
    FManager            : ILogViewerManager;
    FOnAddLogViewer     : Event<TLogViewerEvent>;
    FOnAddReceiver      : Event<TChannelReceiverEvent>;
    FOnActiveViewChange : Event<TLogViewerEvent>;

  protected
    {$REGION 'property access methods'}
    function GetOnActiveViewChange: IEvent<TLogViewerEvent>;
    function GetOnAddLogViewer: IEvent<TLogViewerEvent>;
    function GetOnAddReceiver: IEvent<TChannelReceiverEvent>;
    {$ENDREGION}

    {$REGION 'event dispatch methods'}
    procedure DoAddLogViewer(ALogViewer: ILogViewer); virtual;
    procedure DoActiveViewChange(ALogViewer: ILogViewer); virtual;
    procedure DoAddReceiver(AReceiver: IChannelReceiver); virtual;
    {$ENDREGION}

    procedure Clear;

  public
    constructor Create(AManager: ILogViewerManager);
    procedure BeforeDestruction; override;

    property OnActiveViewChange: IEvent<TLogViewerEvent>
      read GetOnActiveViewChange;

    property OnAddLogViewer: IEvent<TLogViewerEvent>
      read GetOnAddLogViewer;

    property OnAddReceiver: IEvent<TChannelReceiverEvent>
      read GetOnAddReceiver;
  end;

implementation

uses
  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TLogViewerEvents.Create(AManager: ILogViewerManager);
begin
  Guard.CheckNotNull(AManager, 'AManager');
  FManager := AManager;
end;

procedure TLogViewerEvents.BeforeDestruction;
begin
  Logger.Track('TLogViewerEvents.BeforeDestruction');
  Clear;
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogViewerEvents.GetOnActiveViewChange: IEvent<TLogViewerEvent>;
begin
  Result := FOnActiveViewChange;
end;

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
procedure TLogViewerEvents.DoActiveViewChange(ALogViewer: ILogViewer);
begin
  FOnActiveViewChange.Invoke(Self, ALogViewer);
end;

procedure TLogViewerEvents.DoAddLogViewer(ALogViewer: ILogViewer);
begin
  FOnAddLogViewer.Invoke(Self, ALogViewer);
end;

procedure TLogViewerEvents.DoAddReceiver(AReceiver: IChannelReceiver);
begin
  FOnAddReceiver.Invoke(Self, AReceiver);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TLogViewerEvents.Clear;
begin
  FOnAddLogViewer.Clear;
  FOnAddReceiver.Clear;
  FOnActiveViewChange.Clear;
end;
{$ENDREGION}

end.
