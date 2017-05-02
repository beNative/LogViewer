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

unit LogViewer.Commands;

interface

uses
  Spring,

  LogViewer.Interfaces;

type
  TLogViewerCommands = class(TInterfaceBase, ILogViewerCommands) // no refcount
  private
    FManager: ILogViewerManager;

  protected
    procedure ClearMessages;

  public
    constructor Create(AManager: ILogViewerManager);
    procedure BeforeDestruction; override;

  end;

implementation

{$REGION 'construction and destruction'}
constructor TLogViewerCommands.Create(AManager: ILogViewerManager);
begin
  FManager := AManager;
end;

procedure TLogViewerCommands.BeforeDestruction;
begin
  FManager := nil;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TLogViewerCommands.ClearMessages;
begin

end;
{$ENDREGION}

end.
