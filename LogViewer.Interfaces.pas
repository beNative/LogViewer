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

interface

uses
  System.Classes, System.Actions,
  Vcl.ActnList,

  Spring;

type
  TReceiveMessageEvent = procedure(
    Sender  : TObject;
    AStream : TStream
  ) of object;

type
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

  ILogViewerManager = interface
  ['{3EC3A6B2-88B8-4B5E-9160-D267DBFB9C22}']
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

implementation

end.
