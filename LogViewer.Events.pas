{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

{ Dispatches events on the active view. }

interface

uses
  System.Classes,

  Spring,

  LogViewer.Interfaces;

type
  TLogViewerEvents = class(TInterfaceBase, ILogViewerEvents) // no refcount
  private
    FManager            : ILogViewerManager;
    FOnAddLogViewer     : Event<TLogViewerEvent>;
    FOnDeleteLogViewer  : Event<TLogViewerEvent>;
    FOnAddReceiver      : Event<TChannelReceiverEvent>;
    FOnActiveViewChange : Event<TLogViewerEvent>;
    FOnShowDashboard    : Event<TNotifyEvent>;

  protected
    {$REGION 'property access methods'}
    function GetOnActiveViewChange: IEvent<TLogViewerEvent>;
    function GetOnAddLogViewer: IEvent<TLogViewerEvent>;
    function GetOnDeleteLogViewer: IEvent<TLogViewerEvent>;
    function GetOnAddReceiver: IEvent<TChannelReceiverEvent>;
    function GetOnShowDashboard: IEvent<TNotifyEvent>;
    {$ENDREGION}

    {$REGION 'event dispatch methods'}
    procedure DoAddLogViewer(ALogViewer: ILogViewer); virtual;
    procedure DoDeleteLogViewer(ALogViewer: ILogViewer); virtual;
    procedure DoActiveViewChange(ALogViewer: ILogViewer); virtual;
    procedure DoAddReceiver(AReceiver: IChannelReceiver); virtual;
    procedure DoShowDashboard; virtual;
    {$ENDREGION}

    procedure Clear;

  public
    constructor Create(AManager: ILogViewerManager);
    destructor Destroy; override;

    property OnActiveViewChange: IEvent<TLogViewerEvent>
      read GetOnActiveViewChange;

    property OnAddLogViewer: IEvent<TLogViewerEvent>
      read GetOnAddLogViewer;

    property OnDeleteLogViewer: IEvent<TLogViewerEvent>
      read GetOnDeleteLogViewer;

    property OnAddReceiver: IEvent<TChannelReceiverEvent>
      read GetOnAddReceiver;

    property OnShowDashboard: IEvent<TNotifyEvent>
      read GetOnShowDashboard;
  end;

implementation

uses
  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TLogViewerEvents.Create(AManager: ILogViewerManager);
begin
  Guard.CheckNotNull(AManager, 'AManager');
  FManager := AManager;
  FOnAddLogViewer.UseFreeNotification     := False;
  FOnDeleteLogViewer.UseFreeNotification  := False;
  FOnAddReceiver.UseFreeNotification      := False;
  FOnActiveViewChange.UseFreeNotification := False;
end;

destructor TLogViewerEvents.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  Clear;
  FManager := nil;
  inherited Destroy;
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

function TLogViewerEvents.GetOnDeleteLogViewer: IEvent<TLogViewerEvent>;
begin
  Result := FOnDeleteLogViewer;
end;

function TLogViewerEvents.GetOnShowDashboard: IEvent<TNotifyEvent>;
begin
  Result := FOnShowDashboard;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TLogViewerEvents.DoActiveViewChange(ALogViewer: ILogViewer);
begin
  Logger.Track(Self, 'DoActiveViewChange');
  FOnActiveViewChange.Invoke(Self, ALogViewer);
end;

procedure TLogViewerEvents.DoAddLogViewer(ALogViewer: ILogViewer);
begin
  Logger.Track(Self, 'DoAddLogViewer');
  FOnAddLogViewer.Invoke(Self, ALogViewer);
end;

procedure TLogViewerEvents.DoAddReceiver(AReceiver: IChannelReceiver);
begin
  Logger.Track(Self, 'DoAddReceiver');
  FOnAddReceiver.Invoke(Self, AReceiver);
end;

procedure TLogViewerEvents.DoDeleteLogViewer(ALogViewer: ILogViewer);
begin
  Logger.Track(Self, 'DoDeleteLogViewer');
  FOnDeleteLogViewer.Invoke(Self, ALogViewer);
end;

procedure TLogViewerEvents.DoShowDashboard;
begin
  Logger.Track(Self, 'DoShowDashboard');
  FOnShowDashboard.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TLogViewerEvents.Clear;
begin
  FOnAddLogViewer.Clear;
  FOnDeleteLogViewer.Clear;
  FOnAddReceiver.Clear;
  FOnActiveViewChange.Clear;
  FOnShowDashboard.Clear;
end;
{$ENDREGION}

end.
