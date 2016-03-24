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

unit LogViewer.MessageViewer;

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

  LogViewer.LogMessageData, LogViewer.WatchList, LogViewer.CallStackData,
  LogViewer.Interfaces;

type
  TfrmMessageViewer = class(TForm)
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
    pgcWatches        : TPageControl;
    tsLatest          : TTabSheet;
    tsSelected        : TTabSheet;
    tsHistory         : TTabSheet;
    cbxWatchHistory   : TComboBox;
    pnlWatches        : TPanel;
    pnlCallStack      : TPanel;
    pnlCallStackTitle : TPanel;
    {$ENDREGION}

  private
    FMessages        : IList<TLogMessageData>;
    FLogTreeView     : TVirtualStringTree;
    FTVPMessages     : TTreeViewPresenter;
    FWatches         : TWatchList;
    FReceiver        : IChannelReceiver;

    FTVPCallStack    : TTreeViewPresenter;
    FVSTCallStack    : TVirtualStringTree;
    FCallStack       : IList<TCallStackData>;

  protected
    procedure FWatchesUpdate(const AVariable, AValue: string);
    procedure FWatchesNewVariable(const AVariable: string; AIndex: Integer);

    procedure ProcessMessage(AStream: TStream);

    procedure CreateLogTreeView;
    procedure CreateCallStackViewer;
    procedure CreateWatches;

  public
    constructor Create(
      AOwner    : TComponent;
      AReceiver : IChannelReceiver
    ); reintroduce; virtual;

  end;

implementation

uses
  Spring,

  DDuce.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmMessageViewer.Create(AOwner: TComponent;
  AReceiver: IChannelReceiver);
begin
  inherited Create(AOwner);
  FReceiver := AReceiver;
end;

procedure TfrmMessageViewer.CreateCallStackViewer;
begin
  FCallStack    := TCollections.CreateObjectList<TCallStackData>;
  FVSTCallStack := TFactories.CreateVirtualStringTree(Self, pnlCallStack);
  FTVPCallStack := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTCallStack,
    FCallStack as IObjectList
  );
  FTVPCallStack.ShowHeader := False;
end;

procedure TfrmMessageViewer.CreateLogTreeView;
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


end;

procedure TfrmMessageViewer.CreateWatches;
begin
  FWatches := TWatchList.Create;
  FWatches.OnUpdate := FWatchesUpdate;
  FWatches.OnNewVariable := FWatchesNewVariable;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMessageViewer.FWatchesNewVariable(const AVariable: string;
  AIndex: Integer);
begin
  //cbxWatchHistory.Items.Add(AVariable);
end;

procedure TfrmMessageViewer.FWatchesUpdate(const AVariable, AValue: string);
begin
//  FLatestWatchInspector.Rows.Count   := FWatches.Count;
//  FSelectedWatchInspector.Rows.Count := FWatches.Count;
//  FWatchHistoryInspector.Refresh;
//  FSelectedWatchInspector.Refresh;
//  FLatestWatchInspector.Refresh;
//  FWatchHistoryInspector.UpdateEditContents(False);
//  FSelectedWatchInspector.UpdateEditContents(False);
//  FLatestWatchInspector.UpdateEditContents(False);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMessageViewer.ProcessMessage(AStream: TStream);
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
  LTextSize := 0;
  LDataSize := 0;
  AStream.Seek(0, soFromBeginning);
  AStream.ReadBuffer(LType, SizeOf(Integer));
  LMD.MsgType := LType;
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
  FMessages.Add(LMD);
end;
{$ENDREGION}

end.
