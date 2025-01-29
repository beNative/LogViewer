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

unit LogViewer.Resources;

{ Application wide type definitions, constants and resource strings. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes,
  Vcl.Graphics,

  DDuce.Logger.Interfaces;

type
  TVKSet = set of Byte;

var
  VK_EDIT_KEYS : TVKSet = [
    VK_DELETE,
    VK_BACK,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    VK_SHIFT,
    VK_CONTROL,
    VK_SPACE,
    Ord('0')..Ord('9'),
    Ord('A')..Ord('Z'),
    VK_OEM_1..VK_OEM_102,
    VK_NUMPAD0..VK_DIVIDE
  ];

  VK_CTRL_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    Ord('C'),
    Ord('X'),
    Ord('V'),
    Ord('Z')
  ];

  VK_SHIFT_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];

resourcestring
  // message types
  SName        = 'Name';
  SType        = 'Type';
  SValue       = 'Value';
  STimestamp   = 'Timestamp';
  SProcessName = 'ProcessName';
  SColor       = 'Color';
  SBitmap      = 'Bitmap';
  SComponent   = 'Component';
  SObject      = 'Object';
  SPersistent  = 'Persistent';
  SInterface   = 'Interface';
  SStrings     = 'Strings';
  SCheckpoint  = 'Checkpoint';
  SDataSet     = 'DataSet';
  SScreenShot  = 'ScreenShot';
  SText        = 'Text';
  SAction      = 'Action';
  SInfo        = 'Info';
  SWarning     = 'Warning';
  SError       = 'Error';
  SException   = 'Exception';
  SCounter     = 'Counter';
  SEnter       = 'Enter';
  SLeave       = 'Leave';

  // dashboard columns
  SId           = 'Id';
  SMessageCount = 'Messagecount';

  // dashboard action captions
  SSubscribe = 'Subscribe';

  STextMessages         = 'Text messages';
  SNotificationMessages = 'Notification messages';
  SValueMessages        = 'Value messages';
  STraceMessages        = 'Trace messages';
  STrackMethod          = 'Track method';

  // settings dialog
  SViewSettings    = 'View settings';
  SDisplaySettings = 'Message display';
  SLogLevels       = 'Logging levels';
  SWatches         = 'Watches';
  SCallStack       = 'Callstack';
  SChannelSettings = 'Channel settings';
  SGeneralSettings = 'General settings';
  SAdvanced        = 'Advanced';

  SWinIPC  = 'WinIPC';
  SWinODS  = 'OutputDebugString API';
  SComPort = 'Serial port';
  SZeroMQ  = 'ZeroMQ';

  SLogLevelAlias = 'Loglevel alias';

  STimeStampFirst = 'Time first message';
  STimeStampLast  = 'Time last message';
  SBytesReceived  = 'Data received';

  // channel receiver names
const
  RECEIVERNAME_WINIPC     = 'WinIPC';
  RECEIVERNAME_WINODS     = 'WinODS';
  RECEIVERNAME_ZEROMQ     = 'ZeroMQ';
  RECEIVERNAME_COMPORT    = 'ComPort';
  RECEIVERNAME_FILESYSTEM = 'FileSystem';

resourcestring
  SReceiverCaptionWinIPC     = 'Windows IPC receiver';
  SReceiverCaptionWinODS     = 'Windows API OutputDebugString receiver';
  SReceiverCaptionZeroMQ     = 'ZeroMQ receiver';
  SReceiverCaptionComPort    = 'ComPort receiver';
  SReceiverCaptionFileSystem = 'File system receiver';

  // main form
  SReceivedCount    = 'Received: <b>%d</b>';
  SSubscriberSource = '<x=4>Source:</x> <b>%s</b> (%d)';

const
  LIBZMQ = 'libzmq';

  // message viewer
  COLUMN_LEVEL     = 0;
  COLUMN_MAIN      = 1; // make variable
  COLUMN_VALUENAME = 2;
  COLUMN_VALUE     = 3;
  COLUMN_VALUETYPE = 4;
  COLUMN_TIMESTAMP = 5;

  // column names
  COLUMNNAME_TIMESTAMP = 'TimeStamp';
  COLUMNNAME_NAME      = 'Name';
  COLUMNNAME_VALUE     = 'Value';
  COLUMNNAME_VALUETYPE = 'ValueType';
  COLUMNNAME_ID        = 'Id';

  // dashboard
  COLUMN_SOURCENAME      = 0;
  COLUMN_KEY             = 1;
  COLUMN_SOURCEID        = 2;
  COLUMN_MESSAGECOUNT    = 3;
  COLUMN_BYTES_RECEIVED  = 4;
  COLUMN_TIMESTAMP_FIRST = 5;
  COLUMN_TIMESTAMP_LAST  = 6;

  // loglevels
  COLUMN_LOGLEVEL      = 0;
  COLUMN_LOGCOLOR      = 1;
  COLUMN_LOGCOLORVALUE = 2;
  COLUMN_LOGALIAS      = 3;

  COLUMNNAME_LOGLEVEL     = 'Level';
  COLUMNNAME_LOGCOLOR     = 'Color';
  COLUMNNAME_LOGCOLORNAME = 'ColorName';
  COLUMNNAME_LOGALIAS     = 'Alias';

  { max. amount of characters allowed to be displayed in the value column of the
    logtree. }
  MAX_TEXTLENGTH_VALUECOLUMN = 200;

  { TCP port used for debugging another LogViewer instance using ZMQ. }
  LOGVIEWER_ZMQ_PORT : Integer = 42134;

implementation

end.
