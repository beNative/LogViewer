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

unit LogViewer.MainForm;

{ Application main form. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.ImageList, System.Win.TaskbarCore,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Taskbar, Vcl.ActnList, Vcl.ImgList,

  ChromeTabs, ChromeTabsClasses, ChromeTabsTypes,

  Spring,

  DDuce.Settings.TextFormat,

  LogViewer.Interfaces,
  LogViewer.Factories, LogViewer.Manager, LogViewer.Settings,
  LogViewer.ComPort.Settings,

  LogViewer.Dashboard.View;

type
  TfrmMain = class(TForm)
    aclMain           : TActionList;
    actCenterToScreen : TAction;
    actShowVersion    : TAction;
    ctMain            : TChromeTabs;
    imlMain           : TImageList;
    pnlMainClient     : TPanel;
    tskbrMain         : TTaskbar;
    pnlStatusBar: TPanel;
    pnlStatusBarGrid: TGridPanel;
    pnlSourceName: TPanel;
    pnlMessageCount: TPanel;
    pnlDelta: TPanel;

    procedure actCenterToScreenExecute(Sender: TObject);
    procedure actShowVersionExecute(Sender: TObject);

    procedure ctMainButtonAddClick(
      Sender      : TObject;
      var Handled : Boolean
    );
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
    procedure ctMainTabDragDrop(
      Sender             : TObject;
      X, Y               : Integer;
      DragTabObject      : IDragTabObject;
      Cancelled          : Boolean;
      var TabDropOptions : TTabDropOptions
    );
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);

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

    procedure ViewsChanged(
      Sender     : TObject;
      const Item : ILogViewer;
      Action     : TCollectionChangedAction
    );

//    procedure ProcessDroppedTab(
//      Sender             : TObject;
//      X, Y               : Integer;
//      DragTabObject      : IDragTabObject;
//      Cancelled          : Boolean;
//      var TabDropOptions : TTabDropOptions
//    );

  protected
    {$REGION 'property access methods'}
    function GetEvents: ILogViewerEvents;
    function GetActions: ILogViewerActions;
    function GetMenus: ILogViewerMenus;
    function GetManager: ILogViewerManager;
    {$ENDREGION}

    procedure CreateDashboardView;

    procedure UpdateStatusBar;
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

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

uses
  Spring.Utils,

  DDuce.ObjectInspector.zObjectInspector, DDuce.Logger;

{$R *.dfm}

{$REGION 'non-interfaced routines'}
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
  FVI := TFileVersionInfo.GetVersionInfo(Application.ExeName);
  Caption := Format('%s %s', [FVI.ProductName, FVI.ProductVersion]);
  FSettings := TLogViewerFactories.CreateSettings;
  try
    FSettings.Load;
  except
    // ignore it
  end;
  FManager := TLogViewerFactories.CreateManager(Self, FSettings);
  //Manager.Views.OnChanged.Add(
  Events.OnAddLogViewer.Add(EventsAddLogViewer);
  Events.OnActiveViewChange.Add(EventsActiveViewChange);
  FSettings.FormSettings.AssignTo(Self);
  FSettings.OnChanged.Add(SettingsChanged);
  FMainToolbar := TLogViewerFactories.CreateMainToolbar(
    FManager.AsComponent,
    Self,
    Actions,
    Menus
  );
  CreateDashboardView;
end;

procedure TfrmMain.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FSettings.FormSettings.Assign(Self);
  FSettings.Save;
  FSettings.OnChanged.Remove(SettingsChanged);
  FSettings.Free;
  Events.OnAddLogViewer.Remove(EventsAddLogViewer);
  Events.OnActiveViewChange.Remove(EventsActiveViewChange);
  inherited BeforeDestruction;
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
  FVI := TFileVersionInfo.GetVersionInfo(Format('%s\%s.dll', [ExtractFileDir(ParamStr(0)), LIBZMQ]));
  ShowMessage(FVI.ToString);


end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.ctMainActiveTabChanged(Sender: TObject; ATab: TChromeTab);
var
  MV : ILogViewer;
begin
  if ATab.DisplayName = 'Dashboard' then
  begin
    FDashboard.Show;
    FDashboard.SetFocus;
  end
  else if Assigned(ATab.Data) then
  begin
    MV := ILogViewer(ATab.Data);
    MV.Form.Show;
    MV.Form.SetFocus;
    Manager.ActiveView := MV;
  end;
end;

procedure TfrmMain.ctMainButtonAddClick(Sender: TObject; var Handled: Boolean);
//var
//  LMessageViewer : ILogViewer;
//  LTab           : TChromeTab;
begin
//  LMessageViewer := TLogViewerFactories.CreateMessagesView(
//    FManager,
//    pnlMainClient,
//    FMessageViewer.Receiver
//  );
//  Manager.AddView(LMessageViewer);
//  LTab := ctMain.Tabs.Add;
//  LTab.Data := Pointer(LMessageViewer);
//  LTab.Caption := Format('%s-%s', [
//    LMessageViewer.Form.Caption,
//    LMessageViewer.Receiver.Name
//  ]);
  Handled := True;
end;

procedure TfrmMain.ctMainButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
  var Close: Boolean);
begin
  Close := ATab.DisplayName <> 'Dashboard';
  if Close then
  begin
    Manager.Views.Delete(Manager.Views.IndexOf(ILogViewer(ATab.Data)));
  end;
end;

procedure TfrmMain.ctMainNeedDragImageControl(Sender: TObject; ATab: TChromeTab;
  var DragControl: TWinControl);
begin
  DragControl := pnlMainClient;
end;

procedure TfrmMain.ctMainTabDragDrop(Sender: TObject; X, Y: Integer;
  DragTabObject: IDragTabObject; Cancelled: Boolean;
  var TabDropOptions: TTabDropOptions);
begin
  // TODO: not working yet
  //ProcessDroppedTab(Sender, X, Y, DragTabObject, Cancelled, TabDropOptions);
end;

procedure TfrmMain.EventsActiveViewChange(Sender: TObject;
  ALogViewer: ILogViewer);
var
  CT : TChromeTab;
  I  : Integer;
begin
  for I := 0 to ctMain.Tabs.Count - 1 do
  begin
    CT := ctMain.Tabs[I];
    if CT.Data = Pointer(ALogViewer) then
      ctMain.ActiveTab := CT;
  end;
  ALogViewer.Form.Show;
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
    Format('%s (%d %s)', [
      ALogViewer.Subscriber.Receiver.ToString,
      ALogViewer.Subscriber.SourceId,
      S
    ]);

  ALogViewer.Form.Show;
end;

procedure TfrmMain.SettingsChanged(Sender: TObject);
begin
  FormStyle   := FSettings.FormSettings.FormStyle;
  WindowState := FSettings.FormSettings.WindowState;
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
  FDashboard.BorderStyle := bsNone;
  FDashboard.Align       := alClient;

  ctMain.Tabs.Add;
  ctMain.ActiveTab.Data        := Pointer(FDashboard);
  ctMain.ActiveTab.Caption     := 'Dashboard';
  ctMain.ActiveTab.DisplayName := 'Dashboard';
  ctMain.ActiveTab.Pinned      := True;
  FDashboard.Show;
end;

{ Handles the drop operation of a dragged tab. }

//procedure TfrmMain.ProcessDroppedTab(Sender: TObject; X, Y: Integer;
//  DragTabObject: IDragTabObject; Cancelled: Boolean;
//  var TabDropOptions: TTabDropOptions);
//var
//  WinX, WinY: Integer;
//  NewForm   : TForm;
//begin
//  // Make sure that the drag drop hasn't been cancelled and that
//  // we are not dropping on a TChromeTab control
//  if (not Cancelled) and
//    (DragTabObject.SourceControl <> DragTabObject.DockControl) and
//    (DragTabObject.DockControl = nil) then
//  begin
//    // Find the drop position
//    WinX := Mouse.CursorPos.X - DragTabObject.DragCursorOffset.X -
//      ((Width - ClientWidth) div 2);
//    WinY := Mouse.CursorPos.Y - DragTabObject.DragCursorOffset.Y -
//      (Height - ClientHeight) + ((Width - ClientWidth) div 2);
//
//    // Create a new form
//    NewForm := TForm.Create(Application);
//
//    // Set the new form position
//    NewForm.Position := poDesigned;
//    NewForm.Left     := WinX;
//    NewForm.Top      := WinY;
//
//    // Show the form
//    NewForm.Show;
//
//    // Remove the original tab
//    TabDropOptions := [tdDeleteDraggedTab];
//  end;
//end;

procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  UpdateStatusBar;
  //UpdateTabs;
end;

procedure TfrmMain.UpdateStatusBar;
var
  N : Integer;
begin
  if Assigned(Manager) and Assigned(Manager.ActiveView) then
  begin
    pnlSourceName.Caption := Manager.ActiveView.Subscriber.SourceName;
    N := Manager.ActiveView.MilliSecondsBetweenSelection;
    if N <> -1 then
      pnlDelta.Caption := Format('Delta: %d', [N])
    else
      pnlDelta.Caption := '';
  end
  else
  begin
    pnlSourceName.Caption := '';
    pnlDelta.Caption      := '';
  end;
end;

procedure TfrmMain.ViewsChanged(Sender: TObject; const Item: ILogViewer;
  Action: TCollectionChangedAction);
begin
  if Action = caRemoved then
  begin

  end;
end;

{$ENDREGION}

initialization
  EnsureZMQLibExists;

end.
