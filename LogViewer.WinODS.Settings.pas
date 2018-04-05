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

unit LogViewer.WinODS.Settings;

interface

uses
  System.Classes,

  Spring;

type
  TWinODSSettings = class(TPersistent)
  private
    FOnChanged   : Event<TNotifyEvent>;
    FProcessId   : Integer;
    FProcessName : string;

    function GetOnChanged: IEvent<TNotifyEvent>;

  protected
    procedure Changed;

  public
    procedure Assign(Source: TPersistent); override;

    property ProcessName: string
      read FProcessName write FProcessName;

    property ProcessId: Integer
      read FProcessId write FProcessId;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;
  end;

implementation

{$REGION 'property access methods'}
function TWinODSSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TWinODSSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWinODSSettings.Assign(Source: TPersistent);
var
  LSettings: TWinODSSettings;
begin
  if Source is TWinODSSettings then
  begin
    LSettings := TWinODSSettings(Source);

  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}
end.
