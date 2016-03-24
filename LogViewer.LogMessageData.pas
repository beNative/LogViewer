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


unit LogViewer.LogMessageData;

interface

uses
  System.Classes;

type
  TLogMessageData = class
  private
    FMsgType : Integer;
    FMsgTime : TDateTime;
    FMsgText : AnsiString;
    FMsgData : TStream;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property MsgData: TStream
      read FMsgData;

    property MsgType : Integer
      read FMsgType write FMsgType;

    property MsgTime: TDateTime
      read FMsgTime write FMsgTime;

    property MsgText: AnsiString
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
