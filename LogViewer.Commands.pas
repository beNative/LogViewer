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

unit LogViewer.Commands;

{ Handles execution of user commands on the active view (called from actions). }

interface

uses
  Spring,

  LogViewer.Interfaces;

type
  TLogViewerCommands = class(TInterfaceBase, ILogViewerCommands) // no refcount
  private
    FManager: ILogViewerManager;

    {$REGION 'property access methods'}
    function GetActiveView: ILogViewer;
    function GetReceiver: IChannelReceiver;
    {$ENDREGION}

  protected
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

  public
    constructor Create(AManager: ILogViewerManager);
    procedure BeforeDestruction; override;

    property ActiveView: ILogViewer
      read GetActiveView;

    property Receiver: IChannelReceiver
      read GetReceiver;

  end;

implementation

uses
  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TLogViewerCommands.Create(AManager: ILogViewerManager);
begin
  inherited Create;
  Guard.CheckNotNull(AManager, 'AManager');
  FManager := AManager;
end;

procedure TLogViewerCommands.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogViewerCommands.GetActiveView: ILogViewer;
begin
  Result := FManager.ActiveView;
end;

function TLogViewerCommands.GetReceiver: IChannelReceiver;
begin
  if Assigned(ActiveView) then
    Result := ActiveView.Subscriber.Receiver
  else
    Result := nil;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TLogViewerCommands.ClearMessages;
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.Clear;
  end;
end;

procedure TLogViewerCommands.CollapseAll;
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.CollapseAll;
  end;
end;

procedure TLogViewerCommands.ExpandAll;
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.ExpandAll;
  end;
end;

procedure TLogViewerCommands.GotoFirst;
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.GotoFirst;
  end;
end;

procedure TLogViewerCommands.GotoLast;
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.GotoLast;
  end;
end;

procedure TLogViewerCommands.SelectAll;
begin
  if Assigned(ActiveView) then
  begin
//    ActiveView.SelectAll;
  end;
end;

procedure TLogViewerCommands.ClearSelection;
begin
  if Assigned(ActiveView) then
  begin
//    ActiveView.SelectNone;
  end;
end;

procedure TLogViewerCommands.SetFocusToFilter;
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.SetFocusToFilter;
  end;
end;

procedure TLogViewerCommands.Start;
begin
  if Assigned(ActiveView) and Assigned(ActiveView.Subscriber) then
  begin
    ActiveView.Subscriber.Enabled := True;
  end;
end;

procedure TLogViewerCommands.Stop;
begin
  if Assigned(ActiveView) and Assigned(ActiveView.Subscriber) then
  begin
    ActiveView.Subscriber.Enabled := False;
  end;
end;

procedure TLogViewerCommands.UpdateView;
begin
  if Assigned(ActiveView) then
  begin
    ActiveView.UpdateView;
  end;
end;
{$ENDREGION}

end.
