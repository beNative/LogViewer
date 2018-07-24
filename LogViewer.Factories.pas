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

unit LogViewer.Factories;

{ Factories for all modules used in the application:
    - Callstack view
    - Watches view
    - Main message view
    - Manager instance
    - Settings
}

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.ComCtrls, Vcl.Menus,

  Spring.Collections,

  LogViewer.CallStack.View, LogViewer.Watches.View,
  LogViewer.Watches.Data, LogViewer.Messages.Data, LogViewer.MessageList.View,
  LogViewer.Interfaces, LogViewer.Manager, LogViewer.Settings,
  LogViewer.ComPort.Settings, LogViewer.DisplayValues.Settings,
  LogViewer.Watches.Settings;

type
  TLogViewerFactories = class sealed

  private
     { forces drop down menu to be shown on pressing a toolbarbutton with
       Style tbsDropDown }
     class procedure OnDropdownMenuButtonClick(Sender: TObject);

  public
    class function CreateCallStackView(
      AOwner                 : TComponent;
      AParent                : TWinControl;
      AData                  : IObjectList;
      ADisplayValuesSettings : TDisplayValuesSettings
    ): TfrmCallStackView;

    class function CreateWatchesView(
      AOwner                 : TComponent;
      AParent                : TWinControl;
      AData                  : TWatchList;
      ASettings              : TWatchSettings;
      ADisplayValuesSettings : TDisplayValuesSettings
    ): TfrmWatchesView;

    class function CreateLogViewer(
      AManager  : ILogViewerManager;
      ALogQueue : ILogQueue;
      AParent   : TWinControl = nil
    ): TfrmMessageList;

    class function CreateManager(
      AOwner    : TComponent;
      ASettings : TLogViewerSettings
    ): ILogViewerManager;

    class function CreateSettings(
    ): TLogViewerSettings;

    class function CreateMainToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : ILogViewerActions;
      AMenus   : ILogViewerMenus
    ): TToolbar;

    class function CreateComPortChannelReceiver(
      AManager  : ILogViewerManager;
      ASettings : TComPortSettings
    ): IChannelReceiver;

    class function CreateWinIPCChannelReceiver(
      AManager : ILogViewerManager
    ): IChannelReceiver;

    class function CreateWinODSChannelReceiver(
      AManager : ILogViewerManager
    ): IChannelReceiver;

    class function CreateZeroMQChannelReceiver(
      AManager : ILogViewerManager
    ): IChannelReceiver;

    class function CreateLogQueue(
      AReceiver         : IChannelReceiver;
      ASourceId         : Integer;
      const ASourceName : string
    ): ILogQueue;
  end;

implementation

uses
  Vcl.Forms,

  Spring,

  LogViewer.Factories.Toolbars,
  LogViewer.Receivers.WinIPC, LogViewer.Receivers.WinODS,
  LogViewer.Receivers.ZeroMQ, LogViewer.Receivers.ComPort,
  LogViewer.LogQueue;

{$REGION 'private class methods'}
class procedure TLogViewerFactories.OnDropdownMenuButtonClick(Sender: TObject);
begin
  (Sender as TToolButton).CheckMenuDropdown;
end;
{$ENDREGION}

{$REGION 'public class methods'}
class function TLogViewerFactories.CreateCallStackView(AOwner: TComponent;
  AParent: TWinControl; AData: IObjectList; ADisplayValuesSettings:
  TDisplayValuesSettings): TfrmCallStackView;
begin
  Result := TfrmCallStackView.Create(AOwner, AData, ADisplayValuesSettings);
  Result.Parent      := AParent;
  Result.Align       := alClient;
  Result.BorderStyle := bsNone;
  Result.Visible     := True;
end;

class function TLogViewerFactories.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: ILogViewerActions;
  AMenus: ILogViewerMenus): TToolbar;
var
  TBF : ILogViewerToolbarsFactory;
  I   : Integer;
begin
  Guard.CheckNotNull(AActions, 'AActions');
  Guard.CheckNotNull(AMenus, 'AMenus');
  TBF := TLogViewerToolbarsFactory.Create(AActions, AMenus);
  Result := TBF.CreateMainToolbar(AOwner, AParent);
  for I := 0 to Result.ButtonCount - 1 do
  begin
    if Result.Buttons[I].Style = tbsDropDown then
      Result.Buttons[I].OnClick := OnDropdownMenuButtonClick;
  end;
end;

class function TLogViewerFactories.CreateManager(
  AOwner: TComponent; ASettings: TLogViewerSettings): ILogViewerManager;
begin
  Result := TdmManager.Create(AOwner, ASettings);
end;

class function TLogViewerFactories.CreateLogQueue(AReceiver: IChannelReceiver;
  ASourceId: Integer; const ASourceName : string): ILogQueue;
begin
  Result := TLogQueue.Create(AReceiver, ASourceId, ASourceName);
end;

class function TLogViewerFactories.CreateLogViewer(AManager: ILogViewerManager;
  ALogQueue: ILogQueue; AParent: TWinControl): TfrmMessageList;
begin
  Result := TfrmMessageList.Create(
    Application,
    AManager,
    ALogQueue,
    AManager.Settings.MessageListSettings
  );
  Result.Parent      := AParent;
  Result.Align       := alClient;
  Result.BorderStyle := bsNone;
end;

class function TLogViewerFactories.CreateSettings: TLogViewerSettings;
begin
  Result := TLogViewerSettings.Create;
end;

class function TLogViewerFactories.CreateWatchesView(AOwner: TComponent;
  AParent: TWinControl; AData: TWatchList; ASettings: TWatchSettings;
  ADisplayValuesSettings: TDisplayValuesSettings): TfrmWatchesView;
begin
  Result := TfrmWatchesView.Create(
    AOwner, AData, ASettings, ADisplayValuesSettings
  );
  Result.Parent      := AParent;
  Result.Align       := alClient;
  Result.BorderStyle := bsNone;
  Result.Visible     := True;
end;

class function TLogViewerFactories.CreateWinIPCChannelReceiver(
  AManager: ILogViewerManager): IChannelReceiver;
begin
  Result := TWinIPChannelReceiver.Create(AManager, '');
end;

class function TLogViewerFactories.CreateWinODSChannelReceiver(
  AManager: ILogViewerManager): IChannelReceiver;
begin
  Result := TWinODSChannelReceiver.Create(AManager, '');
end;

class function TLogViewerFactories.CreateZeroMQChannelReceiver(
  AManager: ILogViewerManager): IChannelReceiver;
begin
  Result := TZeroMQChannelReceiver.Create(AManager, '');
end;

class function TLogViewerFactories.CreateComPortChannelReceiver(
  AManager: ILogViewerManager; ASettings: TComPortSettings): IChannelReceiver;
begin
  Result := TComPortChannelReceiver.Create(AManager, '', ASettings);
end;
{$ENDREGION}

end.
