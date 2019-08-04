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

unit LogViewer.MainForm;

{ Application main form. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Taskbar, Vcl.ActnList,

  ChromeTabs, ChromeTabsClasses, ChromeTabsTypes,

  kcontrols, kprogress,

  Spring,

  DDuce.Utils,

  LogViewer.Interfaces, LogViewer.Factories,
  LogViewer.Settings,
  LogViewer.Dashboard.View, System.Win.TaskbarCore, System.ImageList,
  Vcl.ImgList;

type
  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain           : TActionList;
    actCenterToScreen : TAction;
    actShowVersion    : TAction;
    ctMain            : TChromeTabs;
    imlMain           : TImageList;
    pbrCPU            : TKPercentProgressBar;
    pnlDelta          : TPanel;
    pnlMainClient     : TPanel;
    pnlMemory         : TPanel;
    pnlMessageCount   : TPanel;
    pnlSourceName     : TPanel;
    pnlStatusBar      : TPanel;
    pnlTop            : TPanel;
    shpLine           : TShape;
    tmrPoll           : TTimer;
    tskbrMain         : TTaskbar;
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
    {$ENDREGION}

  private
    FManager     : ILogViewerManager;
    FSettings    : TLogViewerSettings;
    FMainToolbar : TToolBar;
    FDashboard   : TfrmDashboard;

    procedure SettingsChanged(Sender: TObject);

    procedure EventsAddLogViewer(
      Sender     : TObject;
      ALogViewer : ILogViewer
    );
    procedure EventsActiveViewChange(
      Sender     : TObject;
      ALogViewer : ILogViewer
    );

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

  DDuce.Logger, DDuce.Utils.Winapi;

{$REGION 'non-interfaced routines'}
{ Extracts the ZMQ library form resources if it does not exist. }

procedure EnsureZMQLibExists;
const
  LIBZMQ = 'libzmq';
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
  FManager := TLogViewerFactories.CreateManager(Self, FSettings);
  Events.OnAddLogViewer.Add(EventsAddLogViewer);
  Events.OnActiveViewChange.Add(EventsActiveViewChange);
  FSettings.FormSettings.AssignTo(Self);
  FSettings.OnChanged.Add(SettingsChanged);
  FMainToolbar := TLogViewerFactories.CreateMainToolbar(
    FManager.AsComponent,
    pnlTop,
    Actions,
    Menus
  );
  CreateDashboardView;
  ctMain.LookAndFeel.Tabs.NotActive.Style.StartColor := ColorToRGB(clBtnFace);
  ctMain.LookAndFeel.Tabs.NotActive.Style.StopColor  := ColorToRGB(clBtnFace);
  ctMain.LookAndFeel.Tabs.Active.Style.StartColor    := ColorToRGB(clWindowFrame);
  ctMain.LookAndFeel.Tabs.Active.Style.StopColor     := ColorToRGB(clWindowFrame);
  ctMain.LookAndFeel.Tabs.Hot.Style.StartColor       := clSilver;
  ctMain.LookAndFeel.Tabs.Hot.Style.StopColor        := clSilver;
end;

destructor TfrmMain.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  tmrPoll.Enabled := False;
  Manager.Receivers.Clear;
  FManager.Settings.FormSettings.Assign(Self);
  FManager.Settings.OnChanged.Remove(SettingsChanged);
  FManager := nil;
  FreeAndNil(FDashboard); // needs to be freed before manager!
  inherited Destroy;
  FSettings.Free;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMain.actCenterToScreenExecute(Sender: TObject);
begin
  WindowState := wsMinimized;
  Top  := 0;
  Left := 0;
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
  if ATab.DisplayName = 'Dashboard' then
  begin
    FDashboard.Show;
    FDashboard.SetFocus;
    pnlSourceName.Caption   := '';
    pnlMessageCount.Caption := '';
    pnlDelta.Caption        := '';
    Manager.ActiveView      := nil;
    OptimizeStatusBarPanelWidths;
  end
  else if Assigned(ATab.Data) then
  begin
    MV := ILogViewer(ATab.Data);
    MV.Form.Show;
    MV.Form.SetFocus;
    Manager.ActiveView := MV;
    UpdateStatusBar;
    OptimizeStatusBarPanelWidths;
  end;
end;

procedure TfrmMain.ctMainButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
  var Close: Boolean);
var
  V : ILogViewer;
begin
  Logger.Track(Self, 'ctMainButtonCloseTabClick');
  Close := ATab.DisplayName <> 'Dashboard';
  if Close then
  begin
    V := ILogViewer(ATab.Data);
    Manager.DeleteView(V);
  end;
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
  S : string;
begin
  ALogViewer.Subscriber.Enabled := True;
  ALogViewer.Form.Parent := pnlMainClient;
  S := ExtractFileName(ALogViewer.Subscriber.SourceName);
  ctMain.Tabs.Add;
  ctMain.ActiveTab.Data := Pointer(ALogViewer);
  ctMain.ActiveTab.Caption :=
    Format('%s (%s, %d)         . ', [
      ALogViewer.Subscriber.Receiver.ToString,
      S,
      ALogViewer.Subscriber.SourceId
    ]);
  ALogViewer.Form.Show;
  UpdateStatusBar;
  OptimizeStatusBarPanelWidths;
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
end;

procedure TfrmMain.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  Handled := Manager.Actions.ActionList.IsShortCut(Msg);
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

  ctMain.Tabs.Add;
  ctMain.ActiveTab.Data        := Pointer(FDashboard);
  ctMain.ActiveTab.Caption     := 'Dashboard';
  ctMain.ActiveTab.DisplayName := 'Dashboard';
  ctMain.ActiveTab.Pinned      := True;
  FDashboard.Show;
end;

procedure TfrmMain.DrawPanelText(APanel: TPanel; const AText: string);
var
  LCanvas : Shared<TControlCanvas>;
  LBitmap : Shared<TBitmap>;
  LWidth  : Integer;
  X       : Integer;
begin
  LBitmap := TBitmap.Create;
  LBitmap.Value.SetSize(APanel.Width, APanel.Height);
  LBitmap.Value.Canvas.Brush.Color := APanel.Color;
  LBitmap.Value.Canvas.FillRect(LBitmap.Value.Canvas.ClipRect);
  LWidth := DrawFormattedText(
    LBitmap.Value.Canvas.ClipRect,
    LBitmap.Value.Canvas,
    AText
  );
  X := (APanel.Width - LWidth) div 2;
  LCanvas := TControlCanvas.Create;
  LCanvas.Value.Control := APanel;
  LCanvas.Value.Draw(X, 0, LBitmap);
end;

procedure TfrmMain.UpdateActions;
begin
  UpdateStatusBar;
  inherited UpdateActions;
end;

procedure TfrmMain.OptimizeStatusBarPanelWidths;
begin
  OptimizeWidth(pnlSourceName);
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

procedure TfrmMain.UpdateStatusBar;
var
  N : Integer;
  S : string;
begin
  if Assigned(ctMain.ActiveTab) and Assigned(Manager)
    and Assigned(Manager.ActiveView)
    and Assigned(Manager.ActiveView.Subscriber)
    and (ctMain.ActiveTab.Caption <> 'Dashboard') then
  begin
    pnlSourceName.Caption := Format('%s (%d)', [
      Manager.ActiveView.Subscriber.SourceName,
      Manager.ActiveView.Subscriber.SourceId
    ]);
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
    S := Format(
      '<b>%d</b></x>',
      [Manager.ActiveView.Subscriber.MessageCount]
    );
    DrawPanelText(pnlMessageCount, S);
  end
  else
  begin
    pnlSourceName.Caption   := '';
    pnlMessageCount.Caption := '';
    pnlDelta.Caption        := '';
    OptimizeStatusBarPanelWidths;
  end;
end;
{$ENDREGION}

initialization
  EnsureZMQLibExists;

end.
