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

unit LogViewer.Messages.View;

interface

{ Message viewer that can be used to display all messages from an associated
  log channel (IChannelReceiver receiver instance) }

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates,

  DDuce.Editor.Interfaces,

  LogViewer.Messages.Data, LogViewer.Watches.Data, LogViewer.Watches.View,
  LogViewer.Interfaces, LogViewer.CallStack.Data, LogViewer.CallStack.View;

type
  TfrmMessagesView = class(TForm, ILogViewerMessagesView)
    {$REGION 'designer controls'}
    pnlRight          : TPanel;
    pgcMessageDetails : TPageControl;
    tsTextViewer      : TTabSheet;
    tsImageViewer     : TTabSheet;
    imgViewer         : TImage;
    tsInspector       : TTabSheet;
    tsHexEditor       : TTabSheet;
    pnlMessageContent : TPanel;
    splVertical       : TSplitter;
    splLeftVertical   : TSplitter;
    pnlLeft           : TPanel;
    pnlCallStackWatch : TPanel;
    splLeftHorizontal : TSplitter;
    pnlLeftBottom     : TPanel;
    pnlWatches        : TPanel;
    pnlCallStack      : TPanel;
    pnlCallStackTitle : TPanel;
    pnlMessages: TPanel;
    pnlFilter: TPanel;
    btnExpandAll: TSpeedButton;
    btnCollapseAll: TSpeedButton;
    edtMessageFilter: TLabeledEdit;
    btnFilterMessages: TButton;
    chkAutoFilter: TCheckBox;
    {$ENDREGION}

  private
    FMessages        : IList<TLogMessageData>;
    FCallStack       : IList<TCallStackData>;
    FWatches         : TWatchList;
    FLogTreeView     : TVirtualStringTree;
    FTVPMessages     : TTreeViewPresenter;
    FReceiver        : IChannelReceiver;
    FCallStackView   : TfrmCallStackView;
    FWatchesView     : TfrmWatchesView;
    FCurrentMessage  : TLogMessageData; // last received log message
    FFocusedMessage  : TLogMessageData;

    FEditorManager   : IEditorManager;
    FEditorSettings  : IEditorSettings;
    FEditorView      : IEditorView;

  protected
    procedure FReceiverReceiveMessage(Sender: TObject; AStream: TStream);
    procedure FTVPMessagesSelectionChanged(Sender: TObject);

    procedure ProcessMessage(AStream: TStream);

    procedure UpdateCallStack;

    procedure CreateLogTreeView;
    procedure CreateEditor;
    procedure CreateCallStackView;
    procedure CreateWatchesView;

  public
    constructor Create(
      AOwner    : TComponent;
      AReceiver : IChannelReceiver
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  Spring,

  DDuce.Factories,
  DDuce.Editor.Factories,
  DDuce.Logger.Interfaces,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  LogViewer.Messages.Templates, LogViewer.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmMessagesView.Create(AOwner: TComponent;
  AReceiver: IChannelReceiver);
begin
  inherited Create(AOwner);
  FReceiver := AReceiver;
  CreateEditor;
  CreateLogTreeView;
  CreateWatchesView;
  CreateCallStackView;
  FReceiver.OnReceiveMessage.Add(FReceiverReceiveMessage);
end;

procedure TfrmMessagesView.BeforeDestruction;
begin
  FCurrentMessage := nil;
  inherited BeforeDestruction;
end;

procedure TfrmMessagesView.CreateCallStackView;
begin
  FCallStack     := TCollections.CreateObjectList<TCallStackData>;
  FCallStackView := TLogViewerFactories.CreateCallStackView(
    Self,
    pnlCallStack,
    FCallStack as IObjectList
  );
end;

procedure TfrmMessagesView.CreateEditor;
begin
  FEditorSettings := TEditorFactories.CreateSettings(Self, 'settings.xml');
  FEditorManager  := TEditorFactories.CreateManager(Self, FEditorSettings);
  FEditorView     := TEditorFactories.CreateView(tsTextViewer, FEditorManager);
end;

procedure TfrmMessagesView.CreateLogTreeView;
begin
  FMessages := TCollections.CreateObjectList<TLogMessageData>;
  FLogTreeView := TFactories.CreateVirtualStringTree(Self, pnlMessages);
  FTVPMessages := TFactories.CreateTreeViewPresenter(
    Self,
    FLogTreeView,
    FMessages as IObjectList
  );
  FTVPMessages.View.ItemTemplate := TLogTemplate.Create(
    FTVPMessages.ColumnDefinitions,
    FMessages
  );
  FTVPMessages.OnSelectionChanged := FTVPMessagesSelectionChanged;
end;

procedure TfrmMessagesView.CreateWatchesView;
begin
  FWatches := TWatchList.Create;
  FWatchesView := TLogViewerFactories.CreateWatchesView(
    Self,
    pnlLeftBottom,
    FWatches,
    FMessages
  );
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMessagesView.FReceiverReceiveMessage(Sender: TObject;
  AStream: TStream);
begin
  ProcessMessage(AStream);
end;

procedure TfrmMessagesView.FTVPMessagesSelectionChanged(Sender: TObject);
begin
  FFocusedMessage := FTVPMessages.SelectedItem as TLogMessageData;
  FEditorView.Text := FFocusedMessage.Children.Count.ToString;
  UpdateCallStack;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMessagesView.ProcessMessage(AStream: TStream);
var
  LTextSize : Integer;
  LDataSize : Integer;
  LMD       : TLogMessageData;
  LTime     : TDateTime;
  LType     : Integer;
  LText     : AnsiString;
begin
  Guard.CheckNotNull(AStream, 'AStream');
  LMD := TLogMessageData.Create;
  LMD.Index := FMessages.Count;
  LTextSize := 0;
  LDataSize := 0;
  AStream.Seek(0, soFromBeginning);
  AStream.ReadBuffer(LType, SizeOf(Integer));
  LMD.MessageType := TLogMessageType(LType);
  AStream.ReadBuffer(LTime, SizeOf(TDateTime));
  LMD.TimeStamp := LTime;
  AStream.ReadBuffer(LTextSize, SizeOf(Integer));
  SetLength(LText, LTextSize);
  AStream.ReadBuffer(LText[1], LTextSize);
  LMD.Text := LText;
  AStream.ReadBuffer(LDataSize, SizeOf(Integer));
  if LDataSize > 0 then
  begin
    LMD.Data.CopyFrom(AStream, LDataSize);
  end;
  if LMD.MessageType = lmtLeaveMethod then
  begin
    LMD.Level  := FCurrentMessage.Level - 1;
    if Assigned(FCurrentMessage.Parent) then
      LMD.Parent := FCurrentMessage.Parent.Parent;
  end
  else
  begin
    if Assigned(FCurrentMessage) then
    begin
      if FCurrentMessage.MessageType = lmtEnterMethod then
      begin
        LMD.Level  := FCurrentMessage.Level + 1;
        LMD.Parent := FCurrentMessage;
        LMD.Parent.Children.Add(LMD);
      end
  //    else if LMD.MsgType = lmtLeaveMethod then
  //    begin
  //      LMD.MsgLevel := FCurrentMessage.MsgLevel - 1;
  //    end
      else
      begin
        LMD.Level  := FCurrentMessage.Level;
        LMD.Parent := FCurrentMessage.Parent;
        if Assigned(LMD.Parent) then
          LMD.Parent.Children.Add(LMD);
      end;
    end
    else
    begin
      LMD.Level := 0;
    end;
  end;
  FMessages.Add(LMD);
  FCurrentMessage := LMD;
end;

procedure TfrmMessagesView.UpdateCallStack;
var
  I        : Integer;
  CSD      : TCallStackData;
  LMessage : TLogMessageData;
begin
  FCallStack.Clear;
  if Assigned(FFocusedMessage) then
  begin
    I := FFocusedMessage.Level;
    LMessage := FFocusedMessage;
    while I > 0 do
    begin
      CSD := TCallStackData.Create;
      CSD.Title := LMessage.Parent.Text;
      CSD.Level := I;
      FCallStack.Add(CSD);
      LMessage := LMessage.Parent;
      Dec(I);
    end;
  end;
end;
{$ENDREGION}

end.
