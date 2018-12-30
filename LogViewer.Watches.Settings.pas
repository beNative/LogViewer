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

unit LogViewer.Watches.Settings;

interface

uses
  System.Classes,

  Spring;

const
  DEFAULT_WATCH_HISTORY_PANEL_HEIGHT = 100;
  DEFAULT_HEIGHT                     = 300;

type
  TWatchSettings = class(TPersistent)
  private
    FWatchHistoryPanelHeight : Integer;
    FOnChanged               : Event<TNotifyEvent>;
    FOnlyTrackChanges        : Boolean;
    FHeight                  : Integer;

  protected
    {$REGION 'property access methods'}
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    function GetWatchHistoryPanelHeight: Integer;
    procedure SetWatchHistoryPanelHeight(const Value: Integer);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetOnlyTrackChanges: Boolean;
    procedure SetOnlyTrackChanges(const Value: Boolean);
    {$ENDREGION}

    procedure Changed;

  public
    procedure AfterConstruction; override;

    procedure Assign(Source: TPersistent); override;

    property WatchHistoryPanelHeight: Integer
      read GetWatchHistoryPanelHeight write SetWatchHistoryPanelHeight
      default DEFAULT_WATCH_HISTORY_PANEL_HEIGHT;

    property Height: Integer
      read GetHeight write SetHeight default DEFAULT_HEIGHT;


    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property OnlyTrackChanges: Boolean
      read GetOnlyTrackChanges write SetOnlyTrackChanges;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TWatchSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FHeight                  := DEFAULT_HEIGHT;
  FWatchHistoryPanelHeight := DEFAULT_WATCH_HISTORY_PANEL_HEIGHT;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWatchSettings.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TWatchSettings.SetHeight(const Value: Integer);
begin
  if Value <> Height then
  begin
    FHeight := Value;
    Changed;
  end;
end;

function TWatchSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TWatchSettings.GetOnlyTrackChanges: Boolean;
begin
  Result := FOnlyTrackChanges;
end;


procedure TWatchSettings.SetOnlyTrackChanges(const Value: Boolean);
begin
  if Value <> OnlyTrackChanges then
  begin
    FOnlyTrackChanges := Value;
    Changed;
  end;
end;

function TWatchSettings.GetWatchHistoryPanelHeight: Integer;
begin
  Result := FWatchHistoryPanelHeight;
end;

procedure TWatchSettings.SetWatchHistoryPanelHeight(const Value: Integer);
begin
  if Value <> WatchHistoryPanelHeight then
  begin
    FWatchHistoryPanelHeight := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TWatchSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWatchSettings.Assign(Source: TPersistent);
var
  LSettings: TWatchSettings;
begin
  if Source is TWatchSettings then
  begin
    LSettings               := TWatchSettings(Source);
    OnlyTrackChanges        := LSettings.OnlyTrackChanges;
    WatchHistoryPanelHeight := LSettings.WatchHistoryPanelHeight;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
