{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Subscribers.FileSystem;

interface

uses
  System.Classes,

  Spring, Spring.Helpers,

  LogViewer.Interfaces, LogViewer.Subscribers.Base;

type
  TFileSystemSubscriber = class(TSubscriber, ISubscriber, IFileSystem)
  private
    FPosition : Int64;
    FStream   : TFileStream;
    FBuffer   : TMemoryStream; // message buffer

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Poll; override;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Logger, DDuce.Logger.Interfaces;

{$REGION 'construction and destruction'}
procedure TFileSystemSubscriber.AfterConstruction;
begin
  inherited AfterConstruction;
  FStream   := TFileStream.Create(SourceName, fmOpenRead or fmShareDenyNone);
  FBuffer   := TMemoryStream.Create;
  FPosition := 0;
end;

procedure TFileSystemSubscriber.BeforeDestruction;
begin
  FStream.Free;
  FBuffer.Free;
  inherited BeforeDestruction;
end;

procedure TFileSystemSubscriber.Poll;
const
  ZERO_BUF : Integer = 0;
var
  LTextSize     : Integer;
  LStringStream : Shared<TStringStream>;
  LMsgType      : Byte;
  LDummy        : Byte;
begin
  if Enabled and (FPosition <> FStream.Size) then
  begin
    FStream.Seek(FPosition, soBeginning);
    LStringStream := TStringStream.Create;

    LStringStream.Value.CopyFrom(FStream, FStream.Size - FPosition);
    Logger.Send('Text', LStringStream.Value.DataString);

    FBuffer.Clear;
    LMsgType := Integer(lmtText);
    LTextSize := LStringStream.Value.Size;
    LDummy := 0;
    Logger.Send('LTextSize', LTextSize);
    FBuffer.Seek(0, soBeginning);
    FBuffer.WriteBuffer(LMsgType);
    FBuffer.WriteBuffer(LDummy);
    FBuffer.WriteBuffer(LDummy);
    FBuffer.WriteBuffer(LDummy);
    FBuffer.WriteBuffer(Now);
    FBuffer.WriteBuffer(LTextSize);
    FBuffer.WriteBuffer(LStringStream.Value.Bytes, LTextSize);
    FBuffer.WriteBuffer(ZERO_BUF);
    DoReceiveMessage(FBuffer);
    //Receiver.DoReceiveMessage(FBuffer, SourceId, 0, SourceName);
    FPosition := FStream.Size;
  end;
end;
{$ENDREGION}

end.
