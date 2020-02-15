{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

{ Factories for main application modules. }

interface

{$REGION 'documentation'}
{ Factories for all core modules used in the application:
    - Callstack view
    - Watches view
    - Main message view
    - Manager instance
    - Settings
}
{$ENDREGION}

uses
  System.Classes,
  Vcl.Controls, Vcl.ComCtrls, Vcl.Menus,

  Spring.Collections,

  ZeroMQ,

  LogViewer.CallStack.View, LogViewer.Watches.View,
  LogViewer.Watches.Data, LogViewer.MessageList.View, LogViewer.Interfaces,
  LogViewer.Manager, LogViewer.Settings, LogViewer.Receivers.ComPort.Settings,
  LogViewer.DisplayValues.Settings, LogViewer.Watches.Settings,
  LogViewer.CallStack.Settings;

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
      ASettings              : TCallStackSettings;
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
      AManager    : ILogViewerManager;
      ASubscriber : ISubscriber;
      AParent     : TWinControl = nil
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
    ): TToolBar;

    class function CreateComPortReceiver(
      AManager  : ILogViewerManager;
      ASettings : TComPortSettings
    ): IChannelReceiver;

    class function CreateWinIpcReceiver(
      AManager : ILogViewerManager
    ): IChannelReceiver;

    class function CreateWinOdsReceiver(
      AManager : ILogViewerManager
    ): IChannelReceiver;

    class function CreateZeroMQReceiver(
      AManager : ILogViewerManager;
      AZMQ     : IZeroMQ
    ): IChannelReceiver;

    class function CreateFileSystemReceiver(
      AManager    : ILogViewerManager;
      const APath : string
    ): IChannelReceiver;
  end;

implementation

uses
  Vcl.Forms,

  Spring,

  LogViewer.Resources, LogViewer.Factories.Toolbars,
  LogViewer.Receivers.Winipc, LogViewer.Receivers.Winods,
  LogViewer.Receivers.ZeroMQ, LogViewer.Receivers.ComPort,
  LogViewer.Receivers.FileSystem;

{$REGION 'private class methods'}
class procedure TLogViewerFactories.OnDropdownMenuButtonClick(Sender: TObject);
begin
  (Sender as TToolButton).CheckMenuDropdown;
end;
{$ENDREGION}

{$REGION 'public class methods'}
class function TLogViewerFactories.CreateCallStackView(AOwner: TComponent;
  AParent: TWinControl; AData: IObjectList; ASettings: TCallStackSettings;
  ADisplayValuesSettings: TDisplayValuesSettings): TfrmCallStackView;
begin
  Result :=
    TfrmCallStackView.Create(AOwner, AData, ASettings, ADisplayValuesSettings);
  Result.Parent      := AParent;
  Result.Align       := alClient;
  Result.BorderStyle := bsNone;
  Result.Visible     := True;
end;

class function TLogViewerFactories.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl; AActions: ILogViewerActions;
  AMenus: ILogViewerMenus): TToolBar;
var
  TBF : ILogViewerToolbarsFactory;
  I   : Integer;
  TB  : TToolBar;
begin
  Guard.CheckNotNull(AActions, 'AActions');
  Guard.CheckNotNull(AMenus, 'AMenus');
  TBF := TLogViewerToolbarsFactory.Create(AActions, AMenus);
  TB := TBF.CreateRightTopToolbar(AOwner, AParent);
  TB.Align := alRight;
  TB.AutoSize := True;
  TB := TBF.CreateMainToolbar(AOwner, AParent);
  for I := 0 to TB.ButtonCount - 1 do
  begin
    if TB.Buttons[I].Style = tbsDropDown then
      TB.Buttons[I].OnClick := OnDropdownMenuButtonClick;
  end;
  TB.Align := alClient;
  Result := TB;
end;

class function TLogViewerFactories.CreateManager(
  AOwner: TComponent; ASettings: TLogViewerSettings): ILogViewerManager;
begin
  Result := TdmManager.Create(AOwner, ASettings);
end;

class function TLogViewerFactories.CreateLogViewer(AManager: ILogViewerManager;
  ASubscriber: ISubscriber; AParent: TWinControl): TfrmMessageList;
begin
  Result := TfrmMessageList.Create(
    AParent,
    AManager,
    ASubscriber,
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

class function TLogViewerFactories.CreateWinIpcReceiver(
  AManager: ILogViewerManager): IChannelReceiver;
begin
  Result := TWinIpcChannelReceiver.Create(AManager, RECEIVERNAME_WINIPC);
end;

class function TLogViewerFactories.CreateWinOdsReceiver(
  AManager: ILogViewerManager): IChannelReceiver;
begin
  Result := TWinOdsChannelReceiver.Create(AManager, RECEIVERNAME_WINODS);
end;

class function TLogViewerFactories.CreateZeroMQReceiver(
  AManager: ILogViewerManager; AZMQ: IZeroMQ): IChannelReceiver;
begin
  Result := TZeroMQChannelReceiver.Create(AManager, AZMQ, RECEIVERNAME_ZEROMQ);
end;

class function TLogViewerFactories.CreateComPortReceiver(
  AManager: ILogViewerManager; ASettings: TComPortSettings): IChannelReceiver;
begin
  Result := TComPortChannelReceiver.Create(AManager, RECEIVERNAME_COMPORT);
end;

class function TLogViewerFactories.CreateFileSystemReceiver(
  AManager: ILogViewerManager; const APath: string): IChannelReceiver;
begin
  Result := TFileSystemChannelReceiver.Create(
    AManager, APath, RECEIVERNAME_FILESYSTEM
  );
end;
{$ENDREGION}

end.
