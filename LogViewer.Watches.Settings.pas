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

type
  TWatchSettings = class(TPersistent)
  private
    FOnChanged            : Event<TNotifyEvent>;
    FOnlyTrackChanges     : Boolean;
    FWatchHistoryVisible  : Boolean;
    FSyncWithSelection    : Boolean;
    FColumnHeadersVisible : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetSyncWithSelection: Boolean;
    procedure SetSyncWithSelection(const Value: Boolean);
    function GetOnlyTrackChanges: Boolean;
    procedure SetOnlyTrackChanges(const Value: Boolean);
    function GetWatchHistoryVisible: Boolean;
    procedure SetWatchHistoryVisible(const Value: Boolean);
    function GetColumnHeadersVisible: Boolean;
    procedure SetColumnHeadersVisible(const Value: Boolean);
    {$ENDREGION}

    procedure Changed;

  public
    procedure AfterConstruction; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property ColumnHeadersVisible: Boolean
      read GetColumnHeadersVisible write SetColumnHeadersVisible;

    property OnlyTrackChanges: Boolean
      read GetOnlyTrackChanges write SetOnlyTrackChanges;

    property WatchHistoryVisible: Boolean
      read GetWatchHistoryVisible write SetWatchHistoryVisible;

    property SyncWithSelection: Boolean
      read GetSyncWithSelection write SetSyncWithSelection;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TWatchSettings.AfterConstruction;
begin
  inherited AfterConstruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWatchSettings.GetColumnHeadersVisible: Boolean;
begin
  Result := FColumnHeadersVisible;
end;

procedure TWatchSettings.SetColumnHeadersVisible(const Value: Boolean);
begin
  if Value <> ColumnHeadersVisible then
  begin
    FColumnHeadersVisible := Value;
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

function TWatchSettings.GetSyncWithSelection: Boolean;
begin
  Result := FSyncWithSelection;
end;

procedure TWatchSettings.SetSyncWithSelection(const Value: Boolean);
begin
  if Value <> SyncWithSelection then
  begin
    FSyncWithSelection := Value;
    Changed;
  end;
end;

function TWatchSettings.GetWatchHistoryVisible: Boolean;
begin
  Result := FWatchHistoryVisible;
end;

procedure TWatchSettings.SetWatchHistoryVisible(const Value: Boolean);
begin
  FWatchHistoryVisible := Value;
  Changed;
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
    LSettings            := TWatchSettings(Source);
    OnlyTrackChanges     := LSettings.OnlyTrackChanges;
    WatchHistoryVisible  := LSettings.WatchHistoryVisible;
    SyncWithSelection    := LSettings.SyncWithSelection;
    ColumnHeadersVisible := LSettings.ColumnHeadersVisible;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
