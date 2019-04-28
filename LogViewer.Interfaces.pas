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

unit LogViewer.Interfaces;

{ Main application interfaces and common types. }

interface

uses
  System.Classes, System.Actions,
  Vcl.Controls, Vcl.ActnList, Vcl.ComCtrls, Vcl.Forms, Vcl.Menus,

  Spring, Spring.Collections,

  DDuce.Editor.Interfaces, DDuce.DynamicRecord,

  LogViewer.Settings, LogViewer.Receivers.ComPort.Settings;

type
  ILogViewer       = interface;
  IChannelReceiver = interface;

  TReceiveMessageEvent = procedure(
    Sender  : TObject;
    AStream : TStream
  ) of object;

  TLogViewerEvent = procedure(
    Sender     : TObject;
    ALogViewer : ILogViewer
  ) of object;

  TChannelReceiverEvent = procedure(
    Sender    : TObject;
    AReceiver : IChannelReceiver
  ) of object;

  IComPortSettings = interface
  ['{BFE46291-9932-4319-8387-9F926597F17F}']
    function GetSettings: TComPortSettings;

    property Settings: TComPortSettings
      read GetSettings;
  end;

  ISubscriber = interface
  ['{49CFDB5B-6A74-4352-9877-EF6E73776938}']
    {$REGION 'property access methods'}
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetKey: string;
    function GetReceiver: IChannelReceiver;
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
    function GetSourceId: UInt32;
    function GetSourceName: string;
    function GetMessageCount: Int64;
    function GetOnChange: IEvent<TNotifyEvent>;
    {$ENDREGION}
    procedure Poll;
    procedure DoReceiveMessage(AStream : TStream);

    property Key: string
      read GetKey;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;

    property MessageCount: Int64
      read GetMessageCount;

    property Receiver: IChannelReceiver
      read GetReceiver;

    property SourceId: UInt32
      read GetSourceId;

    property SourceName: string
      read GetSourceName;

    property OnChange: IEvent<TNotifyEvent>
      read GetOnChange;
  end;

  IChannelReceiver = interface
  ['{7C96D7BD-3D10-4A9A-90AF-43E755859B37}']
    {$REGION 'property access methods'}
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetSubscriberList: IDictionary<UInt32, ISubscriber>;
    function GetOnChange: IEvent<TNotifyEvent>;
    {$ENDREGION}

    function ToString: string;

    procedure DoReceiveMessage(
      AStream           : TStream;
      ASourceId         : UInt32 = 0;
      AThreadId         : UInt32 = 0;
      const ASourceName : string = ''
    );

    property Name: string
      read GetName write SetName;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property SubscriberList: IDictionary<UInt32, ISubscriber>
      read GetSubscriberList;

    property OnChange: IEvent<TNotifyEvent>
      read GetOnChange;
  end;

  IZMQ = interface
  ['{89F150A7-414B-4C81-BC08-40227768D317}']
  end;

  IMQTT = interface
  ['{4C00B52A-0EA5-4247-A88D-109E15D104DA}']
  end;

  IWinIPC = interface
  ['{CE3BF275-B51C-491E-8DBE-1CA0E8816035}']
  end;

  IWinODS = interface
  ['{71CED15C-E2E5-4708-A34F-5BAE4D918A3D}']
  end;

  IComPort = interface
  ['{456C6CFE-8BC8-4ED4-B087-1C8116B5A84A}']
  end;

  IFileSystem = interface
  ['{254D6F91-FE9F-4B80-B35C-CC00329319A9}']
  end;

  { Provides access to all actions that a ILogViewer instance can perform. }

  ILogViewerActions = interface
  ['{73B2BDA9-4098-49A3-95D7-E837EC129FE4}']
    {$REGION 'property access methods'}
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;
    {$ENDREGION}

    procedure UpdateActions;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;
  end;

  ILogViewerMenus = interface
  ['{807937AA-BA66-4302-BE92-D93E25865C97}']
    {$REGION 'property access methods'}
    function GetLogTreeViewerPopupMenu: TPopupMenu;
    function GetMessageTypesPopupMenu: TPopupMenu;
    {$ENDREGION}

    property LogTreeViewerPopupMenu: TPopupMenu
      read GetLogTreeViewerPopupMenu;

    property MessageTypesPopupMenu: TPopupMenu
      read GetMessageTypesPopupMenu;
  end;

  ILogViewer = interface
  ['{C1DF2E26-4507-4B35-94E1-19A36775633F}']
    {$REGION 'property access methods'}
    function GetSubscriber: ISubscriber;
    function GetForm: TCustomForm;
    function GetIsActiveView: Boolean;
    function GetMilliSecondsBetweenSelection: Integer;
    {$ENDREGION}

    procedure Clear;
    procedure UpdateView;
    procedure GotoFirst;
    procedure GotoLast;
    procedure CollapseAll;
    procedure ExpandAll;
    procedure SetFocusToFilter;
    procedure SelectAll;
    procedure ClearSelection;

    property IsActiveView: Boolean
      read GetIsActiveView;

    property Subscriber: ISubscriber
      read GetSubscriber;

    property Form: TCustomForm
      read GetForm;

     property MilliSecondsBetweenSelection: Integer
      read GetMilliSecondsBetweenSelection;
  end;

  ILogViewerEvents = interface
  ['{3BD96AF8-654C-4E7C-9C73-EA6522330E88}']
    {$REGION 'property access methods'}
    function GetOnAddLogViewer: IEvent<TLogViewerEvent>;
    function GetOnAddReceiver: IEvent<TChannelReceiverEvent>;
    function GetOnActiveViewChange: IEvent<TLogViewerEvent>;
    {$ENDREGION}

    procedure DoActiveViewChange(ALogViewer: ILogViewer);
    procedure DoAddLogViewer(ALogViewer: ILogViewer);
    procedure DoAddReceiver(AReceiver: IChannelReceiver);

    procedure Clear;

    property OnActiveViewChange: IEvent<TLogViewerEvent>
      read GetOnActiveViewChange;

    property OnAddReceiver: IEvent<TChannelReceiverEvent>
      read GetOnAddReceiver;

    property OnAddLogViewer: IEvent<TLogViewerEvent>
      read GetOnAddLogViewer;
  end;

  ILogViewerCommands = interface
  ['{70304CE3-9498-4738-9084-175B44104236}']
    procedure ClearMessages;
    procedure UpdateView;
    procedure Start;
    procedure Stop;
    procedure CollapseAll;
    procedure ExpandAll;
    procedure GotoFirst;
    procedure GotoLast;
    procedure SetFocusToFilter;
    procedure SelectAll;
    procedure ClearSelection;
  end;

  ILogViewerManager = interface
  ['{3EC3A6B2-88B8-4B5E-9160-D267DBFB9C22}']
    {$REGION 'property access methods'}
    function GetMenus: ILogViewerMenus;
    function GetActions: ILogViewerActions;
    function GetSettings: TLogViewerSettings;
    function GetActiveView: ILogViewer;
    procedure SetActiveView(const Value: ILogViewer);
    function GetViews: IList<ILogViewer>;
    function GetReceivers: IList<IChannelReceiver>;
    function GetCommands: ILogViewerCommands;
    function GetEvents: ILogViewerEvents;
    function GetEditorManager: IEditorManager;
    {$ENDREGION}

    procedure AddView(ALogViewer: ILogViewer);
    function DeleteView(AView: ILogViewer): Boolean;
    procedure AddReceiver(AReceiver: IChannelReceiver);

    function AsComponent: TComponent;

    property ActiveView: ILogViewer
      read GetActiveView write SetActiveView;

    property Menus: ILogViewerMenus
      read GetMenus;

    property Actions: ILogViewerActions
      read GetActions;

    property Commands: ILogViewerCommands
      read GetCommands;

    property Settings: TLogViewerSettings
      read GetSettings;

    property Events: ILogViewerEvents
      read GetEvents;

    property Views: IList<ILogViewer>
      read GetViews;

    property Receivers: IList<IChannelReceiver>
      read GetReceivers;

    property EditorManager: IEditorManager
      read GetEditorManager;
  end;

  ILogViewerToolbarsFactory = interface
  ['{1024A18F-56B5-4B28-A81F-4C016828D2A2}']
    function CreateMainToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;

    function CreateRightTopToolbar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolbar;
  end;

implementation

end.
