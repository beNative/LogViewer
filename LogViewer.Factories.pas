{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

interface

uses
  System.Classes,
  Vcl.Controls,

  Spring.Collections,

  LogViewer.CallStack.View, LogViewer.Watches.View, LogViewer.Messages.View,
  LogViewer.CallStack.Data, LogViewer.Watches.Data, LogViewer.Messages.Data,
  LogViewer.Interfaces, LogViewer.Manager, LogViewer.Settings;

type
  TLogViewerFactories = class sealed
    class function CreateCallStackView(
      AOwner  : TComponent;
      AParent : TWinControl;
      AData   : IObjectList
    ): TfrmCallStackView;

    class function CreateWatchesView(
      AOwner    : TComponent;
      AParent   : TWinControl;
      AData     : TWatchList;
      AMessages : IList<TLogMessageData>
    ): TfrmWatchesView;

    class function CreateMessageView(
      AOwner    : TComponent;
      AParent   : TWinControl;
      AReceiver : IChannelReceiver
    ): TfrmMessagesView;

    class function CreateManager(
      AOwner : TComponent
    ): TdmManager;

    class function CreateSettings(
    ): TLogViewerSettings;
  end;

implementation

uses
  Vcl.Forms;

class function TLogViewerFactories.CreateCallStackView(AOwner: TComponent;
  AParent: TWinControl; AData: IObjectList): TfrmCallStackView;
begin
  Result := TfrmCallStackView.Create(AOwner, AData);
  Result.Parent      := AParent;
  Result.Align       := alClient;
  Result.BorderStyle := bsNone;
  Result.Visible     := True;
end;

class function TLogViewerFactories.CreateManager(
  AOwner: TComponent): TdmManager;
begin
  Result := TdmManager.Create(AOwner);
end;

class function TLogViewerFactories.CreateMessageView(AOwner: TComponent;
  AParent: TWinControl; AReceiver: IChannelReceiver): TfrmMessagesView;
begin
  Result := TfrmMessagesView.Create(AOwner, AReceiver);
  Result.Parent      := AParent;
  Result.Align       := alClient;
  Result.BorderStyle := bsNone;
  Result.Visible     := True;
end;

class function TLogViewerFactories.CreateSettings: TLogViewerSettings;
begin
  Result := TLogViewerSettings.Create;
end;

class function TLogViewerFactories.CreateWatchesView(AOwner: TComponent;
  AParent: TWinControl; AData: TWatchList; AMessages : IList<TLogMessageData>)
  : TfrmWatchesView;
begin
  Result := TfrmWatchesView.Create(AOwner, AData, AMessages);
  Result.Parent      := AParent;
  Result.Align       := alClient;
  Result.BorderStyle := bsNone;
  Result.Visible     := True;
end;

end.
