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

unit LogViewer.Interfaces;

{ Interfaces used by application. }

interface

uses
  System.Classes, System.Actions,
  Vcl.Controls, Vcl.ActnList, Vcl.ComCtrls,

  Spring,

  DDuce.Logger.Interfaces,

  LogViewer.Settings, LogViewer.ComPort.Settings;

type
  TLogMessageTypes = set of TLogMessageType;

  TReceiveMessageEvent = procedure(
    Sender  : TObject;
    AStream : TStream
  ) of object;

type
  IComPortSettings = interface
  ['{BFE46291-9932-4319-8387-9F926597F17F}']
    function GetSettings: TComPortSettings;

    property Settings: TComPortSettings
      read GetSettings;
  end;

  IChannelReceiver = interface
  ['{7C96D7BD-3D10-4A9A-90AF-43E755859B37}']
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;
  end;

  ILogViewerActions = interface
  ['{73B2BDA9-4098-49A3-95D7-E837EC129FE4}']
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;

    procedure UpdateActions;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;
  end;

  ILogViewerMenus = interface
  ['{B3F8FAFC-00FB-4233-890A-BBBC356B186E}']
  end;

  ILogViewerMessagesView = interface
  ['{C1DF2E26-4507-4B35-94E1-19A36775633F}']
    function GetReceiver: IChannelReceiver;

    procedure Clear;
    procedure UpdateView;
    procedure GotoFirst;
    procedure GotoLast;

    property Receiver: IChannelReceiver
      read GetReceiver;
  end;

  ILogViewerEvents = interface
  ['{3BD96AF8-654C-4E7C-9C73-EA6522330E88}']
  end;

  ILogViewerCommands = interface
  ['{70304CE3-9498-4738-9084-175B44104236}']
    procedure ClearMessages;
    procedure Start;
    procedure Stop;
    procedure CollapseAll;
    procedure ExpandAll;
    procedure GotoFirst;
    procedure GotoLast;
  end;

  ILogViewerManager = interface
  ['{3EC3A6B2-88B8-4B5E-9160-D267DBFB9C22}']
    function GetMenus: ILogViewerMenus;
    function GetActions: ILogViewerActions;
    function GetSettings: TLogViewerSettings;
    function GetVisibleMessageTypes: TLogMessageTypes;
    function GetActiveView: ILogViewerMessagesView;
    procedure SetActiveView(const Value: ILogViewerMessagesView);

    property ActiveView: ILogViewerMessagesView
      read GetActiveView write SetActiveView;

    property Menus: ILogViewerMenus
      read GetMenus;

    property Actions: ILogViewerActions
      read GetActions;

    property Settings: TLogViewerSettings
      read GetSettings;

    property VisibleMessageTypes: TLogMessageTypes
      read GetVisibleMessageTypes;
  end;

  ILogViewerToolbarsFactory = interface
  ['{1024A18F-56B5-4B28-A81F-4C016828D2A2}']
    function CreateMainToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;
  end;

implementation

end.
