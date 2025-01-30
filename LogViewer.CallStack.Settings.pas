{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.CallStack.Settings;

{ Persistable settings for callstack display. }

interface

uses
  System.Classes,

  Spring;

type
  TCallStackSettings = class(TPersistent)
  private
    FOnChanged            : Event<TNotifyEvent>;
    FColumnHeadersVisible : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetColumnHeadersVisible: Boolean;
    procedure SetColumnHeadersVisible(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    {$ENDREGION}

    procedure Changed;

  public
    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property ColumnHeadersVisible: Boolean
      read GetColumnHeadersVisible write SetColumnHeadersVisible;

  end;

implementation

{$REGION 'property access methods'}
function TCallStackSettings.GetColumnHeadersVisible: Boolean;
begin
  Result := FColumnHeadersVisible;
end;

procedure TCallStackSettings.SetColumnHeadersVisible(const Value: Boolean);
begin
  if Value <> ColumnHeadersVisible then
  begin
    FColumnHeadersVisible := Value;
    Changed;
  end;
end;

function TCallStackSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TCallStackSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TCallStackSettings.Assign(Source: TPersistent);
var
  LSettings: TCallStackSettings;
begin
  if Source is TCallStackSettings then
  begin
    LSettings := TCallStackSettings(Source);
    ColumnHeadersVisible := LSettings.ColumnHeadersVisible;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
