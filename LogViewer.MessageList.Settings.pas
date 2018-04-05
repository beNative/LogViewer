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

{  Settings associated with the messagelist viewer }

unit LogViewer.MessageList.Settings;

{ TODO:
   - column settings for
        - TimeStamp columns
        - Name columns
        - Value columns
   - default filtered messages

 }

interface

uses
  System.Classes,

  Spring,

  DDuce.Logger.Interfaces,

  LogViewer.Resources, LogViewer.Watches.Settings;

type
  TLogMessageTypes = set of TLogMessageType;

  TMessageListSettings = class(TPersistent)
  private
    FAutoScrollMessages  : Boolean;
    FOnChanged           : Event<TNotifyEvent>;
    FVisibleMessageTypes : TLogMessageTypes;
    FWatchSettings       : TWatchSettings;

    function GetAutoScrollMessages: Boolean;
    procedure SetAutoScrollMessages(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetVisibleMessageTypes: TLogMessageTypes;
    procedure SetVisibleMessageTypes(const Value: TLogMessageTypes);

  protected
    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property VisibleMessageTypes: TLogMessageTypes
      read GetVisibleMessageTypes write SetVisibleMessageTypes;

    property WatchSettings: TWatchSettings
      read FWatchSettings;

  published
    property AutoScrollMessages: Boolean
      read GetAutoScrollMessages write SetAutoScrollMessages;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TMessageListSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FVisibleMessageTypes := ALL_MESSAGES;
  FWatchSettings := TWatchSettings.Create;
end;

procedure TMessageListSettings.BeforeDestruction;
begin
  FWatchSettings.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TMessageListSettings.GetAutoScrollMessages: Boolean;
begin
  Result := FAutoScrollMessages;
end;

procedure TMessageListSettings.SetAutoScrollMessages(const Value: Boolean);
begin
  if Value <> AutoScrollMessages then
  begin
    FAutoScrollMessages := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetVisibleMessageTypes: TLogMessageTypes;
begin
  Result := FVisibleMessageTypes;
end;

procedure TMessageListSettings.SetVisibleMessageTypes(
  const Value: TLogMessageTypes);
begin
  if Value <> VisibleMessageTypes then
  begin
    FVisibleMessageTypes := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TMessageListSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TMessageListSettings.Assign(Source: TPersistent);
var
  LSettings: TMessageListSettings;
begin
  if Source is TMessageListSettings then
  begin
    LSettings := TMessageListSettings(Source);
    AutoScrollMessages  := LSettings.AutoScrollMessages;
    VisibleMessageTypes := LSettings.VisibleMessageTypes;
    WatchSettings.Assign(LSettings.WatchSettings);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
