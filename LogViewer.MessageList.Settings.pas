{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

  LogViewer.Resources;

type
  TLogMessageTypes = set of TLogMessageType;

  TMessageListSettings = class(TPersistent)
  private
    FAutoScrollMessages  : Boolean;
    FOnChanged           : Event<TNotifyEvent>;
    FVisibleMessageTypes : TLogMessageTypes;

    function GetAutoScrollMessages: Boolean;
    procedure SetAutoScrollMessages(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetVisibleMessageTypes: TLogMessageTypes;
    procedure SetVisibleMessageTypes(const Value: TLogMessageTypes);

  protected
    procedure Changed;

  public


    procedure Assign(Source: TPersistent); override;
    procedure AfterConstruction; override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property VisibleMessageTypes: TLogMessageTypes
      read GetVisibleMessageTypes write SetVisibleMessageTypes;

  published
    property AutoScrollMessages: Boolean
      read GetAutoScrollMessages write SetAutoScrollMessages;

  end;

implementation

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

function TMessageListSettings.GetVisibleMessageTypes: TLogMessageTypes;
begin
  Result := FVisibleMessageTypes;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TMessageListSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TMessageListSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FVisibleMessageTypes := ALL_MESSAGES;
end;

procedure TMessageListSettings.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

end;
{$ENDREGION}

end.
