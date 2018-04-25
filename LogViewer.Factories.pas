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
  LogViewer.ComPort.Settings;

type
  TLogViewerFactories = class sealed
  public
    class function CreateCallStackView(
      AOwner  : TComponent;
      AParent : TWinControl;
      AData   : IObjectList
    ): TfrmCallStackView;

    class function CreateWatchesView(
      AOwner  : TComponent;
      AParent : TWinControl;
      AData   : TWatchList
    ): TfrmWatchesView;

    class function CreateLogViewer(
      AManager  : ILogViewerManager;
      AReceiver : IChannelReceiver;
      AParent   : TWinControl = nil
    ): TfrmMessageList;

    class function CreateManager(
      AOwner    : TComponent;
      ASettings : TLogViewerSettings
    ): TdmManager;

    class function CreateSettings(
    ): TLogViewerSettings;

    class function CreateMainToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AActions : ILogViewerActions;
      AMenus   : ILogViewerMenus
    ): TToolbar;

    class function CreateComPortChannelReceiver(
      ASettings: TComPortSettings
    ): IChannelReceiver;

    class function CreateWinIPCChannelReceiver(
    ): IChannelReceiver;

    class function CreateWinODSChannelReceiver(
    ): IChannelReceiver;

    class function CreateZeroMQChannelReceiver(
    ): IChannelReceiver;
  end;

implementation

uses
  Vcl.Forms,

  Spring,

  LogViewer.Factories.Toolbars,
  LogViewer.Receivers.WinIPC, LogViewer.Receivers.WinODS,
  LogViewer.Receivers.ZeroMQ, LogViewer.Receivers.ComPort;

{$REGION 'public class methods'}
class function TLogViewerFactories.CreateCallStackView(AOwner: TComponent;
  AParent: TWinControl; AData: IObjectList): TfrmCallStackView;
begin
  Result := TfrmCallStackView.Create(AOwner, AData);
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
begin
  Guard.CheckNotNull(AActions, 'AActions');
  Guard.CheckNotNull(AMenus, 'AMenus');
  TBF := TLogViewerToolbarsFactory.Create(AActions, AMenus);
  Result := TBF.CreateMainToolbar(AOwner, AParent);
end;

class function TLogViewerFactories.CreateManager(
  AOwner: TComponent; ASettings: TLogViewerSettings): TdmManager;
begin
  Result := TdmManager.Create(AOwner, ASettings);
end;

class function TLogViewerFactories.CreateLogViewer(AManager: ILogViewerManager;
  AReceiver: IChannelReceiver; AParent: TWinControl): TfrmMessageList;
begin
  Result := TfrmMessageList.Create(
    Application,
    AManager,
    AReceiver,
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
  AParent: TWinControl; AData: TWatchList): TfrmWatchesView;
begin
  Result := TfrmWatchesView.Create(AOwner, AData);
  Result.Parent      := AParent;
  Result.Align       := alClient;
  Result.BorderStyle := bsNone;
  Result.Visible     := True;
end;

class function TLogViewerFactories.CreateWinIPCChannelReceiver: IChannelReceiver;
begin
  Result := TWinIPChannelReceiver.Create('');
end;

class function TLogViewerFactories.CreateWinODSChannelReceiver: IChannelReceiver;
begin
  Result := TWinODSChannelReceiver.Create('');
end;

class function TLogViewerFactories.CreateZeroMQChannelReceiver: IChannelReceiver;
begin
  Result := TZeroMQChannelReceiver.Create('');
end;

class function TLogViewerFactories.CreateComPortChannelReceiver(
  ASettings: TComPortSettings): IChannelReceiver;
begin
  Result := TComPortChannelReceiver.Create('', ASettings);
end;
{$ENDREGION}

end.
