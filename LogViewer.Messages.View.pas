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

unit LogViewer.Messages.View;

interface

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
  TfrmMessagesView = class(TForm)
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
    pnlMessages       : TPanel;
    pnlFilter         : TPanel;
    btnExpandAll      : TSpeedButton;
    btnCollapseAll    : TSpeedButton;
    edtMessageFilter  : TLabeledEdit;
    btnFilterMessages : TButton;
    chkAutoFilter     : TCheckBox;
    splLeftVertical   : TSplitter;
    pnlLeft           : TPanel;
    pnlCallStackWatch : TPanel;
    splLeftHorizontal : TSplitter;
    pnlLeftBottom     : TPanel;
    pnlWatches        : TPanel;
    pnlCallStack      : TPanel;
    pnlCallStackTitle : TPanel;
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

  LogViewer.Factories;

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
var
  CD : TColumnDefinition;
begin
  FMessages := TCollections.CreateObjectList<TLogMessageData>;
  FLogTreeView := TFactories.CreateVirtualStringTree(Self, pnlMessages);
  FTVPMessages := TFactories.CreateTreeViewPresenter(
    Self,
    FLogTreeView,
    FMessages as IObjectList
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
  LMD.MsgType := TLogMessageType(LType);
  AStream.ReadBuffer(LTime, SizeOf(TDateTime));
  LMD.MsgTime := LTime;
  AStream.ReadBuffer(LTextSize, SizeOf(Integer));
  SetLength(LText, LTextSize);
  AStream.ReadBuffer(LText[1], LTextSize);
  LMD.MsgText := LText;
  AStream.ReadBuffer(LDataSize, SizeOf(Integer));
  if LDataSize > 0 then
  begin
    LMD.MsgData.CopyFrom(AStream, LDataSize);
  end;

  if LMD.MsgType = lmtLeaveMethod then
  begin
    LMD.MsgLevel := FCurrentMessage.MsgLevel - 1
  end
  else
  begin
    if Assigned(FCurrentMessage) then
    begin
      if FCurrentMessage.MsgType = lmtEnterMethod then
      begin
        LMD.MsgLevel := FCurrentMessage.MsgLevel + 1;
      end
  //    else if LMD.MsgType = lmtLeaveMethod then
  //    begin
  //      LMD.MsgLevel := FCurrentMessage.MsgLevel - 1;
  //    end
      else
      begin
        LMD.MsgLevel := FCurrentMessage.MsgLevel
      end;
    end
    else
    begin
      LMD.MsgLevel := 0;
    end;
  end;
  FMessages.Add(LMD);
  FCurrentMessage := LMD;
end;


procedure TfrmMessagesView.UpdateCallStack;
var
  I   : Integer;
  CSD : TCallStackData;
begin
  FCallStack.Clear;
  if Assigned(FCurrentMessage) then
  begin
//    I := FCurrentMessage.MsgLevel;
//    while I > 0 do
//    begin
//      CSD := TCallStackData.Create;
//      CSD.Title := PNodeData(FLogTreeView.GetNodeData(ANode^.Parent))^.Title;
//      CSD.Level := I;
//      FCallStack.Add(CSD);
//      ANode := ANode^.Parent;
//      Dec(I);
//    end;
    //FTVPCallStack.TreeView.Header.AutoFitColumns;
  end;
end;

{$ENDREGION}

end.
