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

unit LogViewer.Messages.Data;

interface

uses
  System.Classes,

  Spring.Collections,

  DDuce.Logger.Interfaces;

type
  TLogMessageData = class
  private
    FMsgType  : TLogMessageType;
    FMsgTime  : TDateTime;
    FMsgText  : string;
    FMsgData  : TStream;
    FMsgLevel : Integer;
    FIndex    : Integer;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Index: Integer
      read FIndex write FIndex;

    property MsgLevel: Integer
      read FMsgLevel write FMsgLevel;

    property MsgData: TStream
      read FMsgData;

    property MsgType : TLogMessageType
      read FMsgType write FMsgType;

    property MsgTime: TDateTime
      read FMsgTime write FMsgTime;

    property MsgText: string
      read FMsgText write FMsgText;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TLogMessageData.AfterConstruction;
begin
  inherited AfterConstruction;
  FMsgData := TMemoryStream.Create;
end;

procedure TLogMessageData.BeforeDestruction;
begin
  FMsgData.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
