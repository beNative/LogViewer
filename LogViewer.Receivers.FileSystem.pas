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

unit LogViewer.Receivers.FileSystem;

{ Monitors FileSystem changes for a given path (directory or file). }

interface

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  LogViewer.Interfaces, LogViewer.Receivers.Base;

{$REGION 'documentation'}
{$ENDREGION}

type
  TFileSystemChannelReceiver = class(TChannelReceiver, IChannelReceiver,
                                                       IFileSystem
  )
  private
    FPath : string;

  protected
    function CreateSubscriber(
      ASourceId         : UInt32;
      AThreadId         : UInt32;
      const ASourceName : string
    ): ISubscriber; override;

  public
    constructor Create(
      AManager    : ILogViewerManager;
      const APath : string;
      const AName : string
    ); reintroduce; virtual;
  end;

implementation

uses
  LogViewer.Subscribers.FileSystem;

{$REGION 'construction and destruction'}
constructor TFileSystemChannelReceiver.Create(AManager: ILogViewerManager;
  const APath, AName: string);
begin
  inherited Create(AManager, AName);
  FPath := APath;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TFileSystemChannelReceiver.CreateSubscriber(ASourceId,
  AThreadId: UInt32; const ASourceName: string): ISubscriber;
begin

end;
{$ENDREGION}

end.
