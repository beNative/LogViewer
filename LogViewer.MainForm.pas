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

unit LogViewer.MainForm;

{ Application main form. }

{ REMARKS
   - Check Windows Defender firewall rules if no messages can be captured.
}

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.GDIPOBJ,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.Win.TaskbarCore, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Taskbar,
  Vcl.ExtCtrls, Vcl.ActnList, Vcl.ImgList, Vcl.Menus, Vcl.StdCtrls,
  Vcl.TitleBarCtrls,

  ChromeTabs, ChromeTabsClasses, ChromeTabsTypes,
  kcontrols, kprogress,
  Spring,

  DDuce.Utils,

  LogViewer.Interfaces, LogViewer.Factories, LogViewer.Settings,
  LogViewer.Dashboard.View, Vcl.VirtualImageList;

type
  TfrmMain = class(TForm)
    aclMain           : TActionList;
    actCenterToScreen : TAction;
    actShowVersion    : TAction;
    ctMain            : TChromeTabs;
    imlTabStates      : TImageList;
    pbrCPU            : TKPercentProgressBar;
    pnlDelta          : TPanel;
    pnlMain           : TPanel;
    pnlMainClient     : TPanel;
    pnlMemory         : TPanel;
    pnlMessageCount   : TPanel;
    pnlSourceName     : TPanel;
    pnlStatusBar      : TPanel;
    pnlTitleBar       : TTitleBarPanel;
    pnlTop            : TPanel;
    shpLine           : TShape;
    tbrMain           : TTaskbar;
    tmrPoll           : TTimer;
    imlMain           : TVirtualImageList;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actCenterToScreenExecute(Sender: TObject);
    procedure actShowVersionExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure ctMainButtonCloseTabClick(
      Sender    : TObject;
      ATab      : TChromeTab;
      var Close : Boolean
    );
    procedure ctMainNeedDragImageControl(
      Sender          : TObject;
      ATab            : TChromeTab;
      var DragControl : TWinControl
    );
    procedure ctMainActiveTabChanged(
      Sender : TObject;
      ATab   : TChromeTab
    );
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure tmrPollTimer(Sender: TObject);
    procedure pnlTitleBarCustomButtons0Click(Sender: TObject);
    {$ENDREGION}

  private
    FManager      : ILogViewerManager;
    FSettings     : TLogViewerSettings;
    FMainToolbar  : TToolBar;
    FDashboard    : TfrmDashboard;

    procedure SettingsChanged(Sender: TObject);

    procedure EventsAddLogViewer(
      Sender     : TObject;
      ALogViewer : ILogViewer
    );
    procedure EventsDeleteLogViewer(
      Sender     : TObject;
      ALogViewer : ILogViewer
    );
    procedure EventsActiveViewChange(
      Sender     : TObject;
      ALogViewer : ILogViewer
    );
    procedure EventsShowDashboard(Sender: TObject);

    procedure WMEnterSizeMove(var AMessage: TWMMove); message WM_ENTERSIZEMOVE;
    procedure WMExitSizeMove(var AMessage: TMessage); message WM_EXITSIZEMOVE;

  protected
    {$REGION 'property access methods'}
    function GetEvents: ILogViewerEvents;
    function GetActions: ILogViewerActions;
    function GetMenus: ILogViewerMenus;
    function GetManager: ILogViewerManager;
    {$ENDREGION}

    procedure CreateDashboardView;
    procedure DrawPanelText(
      APanel      : TPanel;
      const AText : string
    );
    procedure UpdateStatusBar;
    procedure UpdateActions; override;
    procedure UpdateTabStates;
    procedure OptimizeWidth(APanel: TPanel);
    procedure OptimizeStatusBarPanelWidths;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Manager: ILogViewerManager
      read GetManager;

    property Actions: ILogViewerActions
      read GetActions;

    { Menu components to use in the user interface. }
    property Menus: ILogViewerMenus
      read GetMenus;

    { Set of events where the user interface can respond to. }
    property Events: ILogViewerEvents
      read GetEvents;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  Spring.Utils,

  DDuce.Factories.VirtualTrees,
  DDuce.Logger, DDuce.Logger.Channels.Zmq, DDuce.Utils.Winapi,

  LogViewer.Resources;

{$REGION 'non-interfaced routines'}
{ Extracts the ZMQ library form resources if it does not exist. }

procedure EnsureZmqLibExists;
var
  LResStream  : TResourceStream;
  LFileStream : TFileStream;
  LPath       : string;
begin
  LPath := Format('%s\%s.dll', [ExtractFileDir(ParamStr(0)), LIBZMQ]);
  if not FileExists(LPath) then
  begin
    LResStream := TResourceStream.Create(HInstance, LIBZMQ, RT_RCDATA);
    try
      LFileStream := TFileStream.Create(LPath, fmCreate);
      try
        LFileStream.CopyFrom(LResStream, 0);
      finally
        LFileStream.Free;
      end;
    finally
      LResStream.Free;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
var
  FVI : TFileVersionInfo;
begin
  inherited AfterConstruction;
  TVirtualStringTreeFactory.DefaultTreeOptions.BorderStyle     := bsNone;
  TVirtualStringTreeFactory.DefaultGridOptions.BorderStyle     := bsNone;
  TVirtualStringTreeFactory.DefaultListOptions.BorderStyle     := bsNone;
  TVirtualStringTreeFactory.DefaultTreeGridOptions.BorderStyle := bsNone;
  TVirtualStringTreeFactory.DefaultTreeListOptions.BorderStyle := bsNone;
  FVI := TFileVersionInfo.GetVersionInfo(Application.ExeName);
  Caption := Format('%s %s', [FVI.ProductName, FVI.ProductVersion]);
  FSettings := TLogViewerFactories.CreateSettings;
  try
    FSettings.Load;
  except
    // ignore it and continue with defaults
  end;
  Logger.Enabled := FSettings.EmitLogMessages;
//  if Logger.Enabled then
//  begin
//    // setup logchannel for using a log LogViewer instance to debug itself.
//    Logger.Channels.Add(
//      TZmqChannel.Create(Format('tcp://*:%d', [LOGVIEWER_ZMQ_PORT]))
//    );
//  end;
  Logger.Clear;

  FManager := TLogViewerFactories.CreateManager(Self, FSettings);
  imlMain.ImageCollection := FManager.ImageCollection;
  Events.OnAddLogViewer.Add(EventsAddLogViewer);
  Events.OnDeleteLogViewer.Add(EventsDeleteLogViewer);
  Events.OnActiveViewChange.Add(EventsActiveViewChange);
  Events.OnShowDashboard.Add(EventsShowDashboard);
  Actions.ActionList.Images := imlMain;
  Menus.LogTreeViewerPopupMenu.Images := imlMain;
  FSettings.FormSettings.AssignTo(Self);
  FSettings.OnChanged.Add(SettingsChanged);
  FMainToolbar := TLogViewerFactories.CreateMainToolbar(
    Self,
    pnlTop,
    Actions,
    Menus
  );
  FMainToolbar.Images := imlMain;
  CreateDashboardView;
  ctMain.LookAndFeel.Tabs.NotActive.Style.StartColor := ColorToRGB(clBtnFace);
  ctMain.LookAndFeel.Tabs.NotActive.Style.StopColor  := ColorToRGB(clBtnFace);
  ctMain.LookAndFeel.Tabs.Active.Style.StartColor    := ColorToRGB(clWindowFrame);
  ctMain.LookAndFeel.Tabs.Active.Style.StopColor := ColorToRGB(clWindowFrame);
  ctMain.LookAndFeel.Tabs.Hot.Style.StartColor   := clSilver;
  ctMain.LookAndFeel.Tabs.Hot.Style.StopColor    := clSilver;
  ctMain.PopupMenu := Manager.Menus.SubscriberPopupMenu;
  tmrPoll.Enabled := True;
end;

destructor TfrmMain.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  tmrPoll.Enabled := False;
  Events.OnDeleteLogViewer.RemoveAll(Self);
  Events.OnAddLogViewer.RemoveAll(Self);
  Events.OnActiveViewChange.RemoveAll(Self);
  Events.OnShowDashboard.RemoveAll(Self);
  FSettings.FormSettings.Assign(Self);
  FManager := nil;
  FreeAndNil(FDashboard); // needs to be freed before manager!
  inherited Destroy; // will destroy manager object as the mainform is its owner
  FSettings.OnChanged.RemoveAll(Self);
  FreeAndNil(FSettings);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMain.actCenterToScreenExecute(Sender: TObject);
begin
  WindowState := wsMinimized;
  Top         := 0;
  Left        := 0;
  WindowState := wsMaximized;
end;

procedure TfrmMain.actShowVersionExecute(Sender: TObject);
const
  LIBZMQ = 'libzmq';
var
  FVI : TFileVersionInfo;
begin
  FVI := TFileVersionInfo.GetVersionInfo(
    Format('%s\%s.dll', [ExtractFileDir(ParamStr(0)), LIBZMQ])
  );
  ShowMessage(FVI.ToString);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.ctMainActiveTabChanged(Sender: TObject; ATab: TChromeTab);
var
  MV : ILogViewer;
begin
  Logger.Track(Self, 'ctMainActiveTabChanged');
  if Assigned(ATab.Data) then
  begin
    MV := ILogViewer(ATab.Data);
    MV.Form.Show;
    MV.Form.SetFocus;
    UpdateStatusBar;
    Manager.ActiveView := MV;
    OptimizeStatusBarPanelWidths;
  end;
end;

procedure TfrmMain.ctMainButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
  var Close: Boolean);
var
  V : ILogViewer;
begin
  Logger.Track(Self, 'ctMainButtonCloseTabClick');
  Guard.CheckNotNull(ATab.Data, 'ATab.Data');
  Close := False; // cleanup happens in EventsDeleteLogViewer
  V := ILogViewer(ATab.Data);
  Manager.DeleteView(V);
end;

procedure TfrmMain.ctMainNeedDragImageControl(Sender: TObject; ATab: TChromeTab;
  var DragControl: TWinControl);
begin
  DragControl := pnlMainClient;
end;

procedure TfrmMain.EventsActiveViewChange(Sender: TObject;
  ALogViewer: ILogViewer);
var
  CT : TChromeTab;
  I  : Integer;
begin
  Logger.Track(Self, 'EventsActiveViewChange');
  if Assigned(ALogViewer) then
  begin
    for I := 0 to ctMain.Tabs.Count - 1 do
    begin
      CT := ctMain.Tabs[I];
      if CT.Data = Pointer(ALogViewer) then
        ctMain.ActiveTab := CT;
    end;
    ALogViewer.Form.Show;
  end
  else
  begin
    FDashboard.Show;
  end;
end;

procedure TfrmMain.EventsAddLogViewer(Sender: TObject;
  ALogViewer: ILogViewer);
var
  S  : string;
  CT : TChromeTab;
begin
  Guard.CheckNotNull(ALogViewer, 'ALogViewer');
  ALogViewer.Subscriber.Enabled := True;
  ALogViewer.Form.Parent := pnlMainClient;
  S := ExtractFileName(ALogViewer.Subscriber.SourceName);
  ctMain.BeginUpdate;
  try
    CT := ctMain.Tabs.Add;
    CT.Data := Pointer(ALogViewer);
    CT.Caption :=
      Format('%s (%d, %s)', [
        S,
        ALogViewer.Subscriber.SourceId,
        ALogViewer.Subscriber.Receiver.ToString
      ]);
    ctMain.ActiveTabIndex := 0;
  finally
    ctMain.EndUpdate;
  end;
  UpdateStatusBar;
  OptimizeStatusBarPanelWidths;
end;

procedure TfrmMain.EventsDeleteLogViewer(Sender: TObject;
  ALogViewer: ILogViewer);
var
  I        : Integer;
  LDeleted : Boolean;
begin
  Guard.CheckNotNull(ALogViewer, 'ALogViewer');
  LDeleted := False;
  I := 0;
  while (I < ctMain.Tabs.Count) and not LDeleted do
  begin
    if ILogViewer(ctMain.Tabs[I].Data) = ALogViewer then
    begin
      ctMain.Tabs.Delete(I);
      LDeleted := True;
    end
    else
    begin
      Inc(I);
    end;
  end;
  ctMain.Resize;
end;

procedure TfrmMain.EventsShowDashboard(Sender: TObject);
begin
  FDashboard.Show;
end;

procedure TfrmMain.SettingsChanged(Sender: TObject);
begin
  FormStyle   := FSettings.FormSettings.FormStyle;
  WindowState := FSettings.FormSettings.WindowState;
end;

procedure TfrmMain.tmrPollTimer(Sender: TObject);
begin
  pbrCPU.Position   := Round(GetProcessCpuUsagePct(GetCurrentProcessId));
  pnlMemory.Caption := FormatBytes(MemoryUsed);
  UpdateTabStates;
end;

procedure TfrmMain.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  Handled := Manager.Actions.ActionList.IsShortCut(Msg);
end;
{$ENDREGION}

{$REGION 'message handlers'}
{ These handlers make resizing the main form more smoothly if many child
  forms are present. }

procedure TfrmMain.WMEnterSizeMove(var AMessage: TWMMove);
begin
  DisableAlign;
end;

procedure TfrmMain.WMExitSizeMove(var AMessage: TMessage);
begin
  EnableAlign;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMain.GetActions: ILogViewerActions;
begin
  Result := Manager.Actions;
end;

function TfrmMain.GetEvents: ILogViewerEvents;
begin
  Result := FManager as ILogViewerEvents;
end;

function TfrmMain.GetManager: ILogViewerManager;
begin
  Result := FManager as ILogViewerManager;
end;

function TfrmMain.GetMenus: ILogViewerMenus;
begin
  Result := Manager.Menus;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMain.CreateDashboardView;
begin
  FDashboard := TfrmDashboard.Create(Self, Manager);
  FDashboard.Parent      := pnlMainClient;
  FDashboard.Align       := alClient;
  FDashboard.BorderStyle := bsNone;
  FDashboard.Show;
end;

procedure TfrmMain.DrawPanelText(APanel: TPanel; const AText: string);
var
  LCanvas : Shared<TControlCanvas>;
  LBitmap : Shared<TBitmap>;
begin
  LBitmap := TBitmap.Create;
  LBitmap.Value.SetSize(APanel.Width, APanel.Height - 2);
  LBitmap.Value.Canvas.Brush.Color := APanel.Color;
  LBitmap.Value.Canvas.FillRect(LBitmap.Value.Canvas.ClipRect);
  DrawFormattedText(
    LBitmap.Value.Canvas.ClipRect,
    LBitmap.Value.Canvas,
    AText
  );
  LCanvas := TControlCanvas.Create;
  LCanvas.Value.Control := APanel;
  LCanvas.Value.Draw(0, 0, LBitmap);
end;

procedure TfrmMain.UpdateActions;
begin
  UpdateStatusBar;
  OptimizeStatusBarPanelWidths;
  if Assigned(Actions) then
    Actions.Items['actDashboard'].Checked := FDashboard.Focused;
  inherited UpdateActions;
end;

procedure TfrmMain.OptimizeStatusBarPanelWidths;
begin
//  OptimizeWidth(pnlSourceName);
//  OptimizeWidth(pnlMessageCount);
end;

procedure TfrmMain.OptimizeWidth(APanel: TPanel);
var
  S : string;
begin
  S := APanel.Caption;
  if Trim(S) <> '' then
  begin
    APanel.Width := GetTextWidth(APanel.Caption, APanel.Font) + 10;
    APanel.AlignWithMargins := True;
  end
  else
  begin
    APanel.Width := 0;
    APanel.AlignWithMargins := False;
  end;
end;

procedure TfrmMain.pnlTitleBarCustomButtons0Click(Sender: TObject);
begin
  ShowMessage('You pressed a hidden button!');
end;

procedure TfrmMain.UpdateStatusBar;
var
  N : Integer;
  S : string;
begin
  if Assigned(ctMain.ActiveTab) and Assigned(Manager)
    and Assigned(Manager.ActiveView)
    and Assigned(Manager.ActiveView.Subscriber) then
  begin
    S := Format(SSubscriberSource, [
      Manager.ActiveView.Subscriber.SourceName,
      Manager.ActiveView.Subscriber.SourceId
    ]);
    DrawPanelText(pnlSourceName, S);
    N := Manager.ActiveView.MilliSecondsBetweenSelection;
    if N <> -1 then
    begin
      pnlDelta.Caption := Format('Delta: %dms', [N]);
      pnlDelta.Color := clWhite;
    end
    else
    begin
      pnlDelta.Caption := '';
      pnlDelta.Color := clBtnFace;
    end;
    S := Format(SReceivedCount, [Manager.ActiveView.Subscriber.MessageCount]);
    DrawPanelText(pnlMessageCount, S);
  end
  else
  begin
    pnlSourceName.Caption   := '';
    pnlMessageCount.Caption := '';
    pnlDelta.Caption        := '';
  end;
  OptimizeStatusBarPanelWidths;
end;

procedure TfrmMain.UpdateTabStates;
var
  I  : Integer;
  S  : ISubscriber;
  LV : ILogViewer;
begin
  if ctMain.Tabs.Count > 0 then
  begin
    for I := 0 to ctMain.Tabs.Count - 1 do
    begin
      LV := ILogViewer(ctMain.Tabs[I].Data);
      S  := LV.Subscriber;
      if Assigned(S) and S.IsSourceActive then
      begin
        if S.Enabled then
          ctMain.Tabs[I].ImageIndex := 0 // Green
        else
        begin
          ctMain.Tabs[I].ImageIndex := 2 // Red
        end;
      end
      else
      begin
        ctMain.Tabs[I].ImageIndex := 1; // Off
      end;
    end;
  end;
end;
{$ENDREGION}

initialization
  EnsureZmqLibExists;

end.
