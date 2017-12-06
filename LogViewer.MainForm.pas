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

unit LogViewer.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,

  ChromeTabs, ChromeTabsClasses,

  LogViewer.MessageList.View, LogViewer.Interfaces,
  LogViewer.Factories, LogViewer.Manager, LogViewer.Settings,
  LogViewer.ComPort.Settings;

type
  TfrmMain = class(TForm)
    sbrMain       : TStatusBar;
    ctMain        : TChromeTabs;
    pnlMainClient : TPanel;

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

  private
    FMessageViewer : ILogViewerMessagesView;
    FManager       : TdmManager;
    FSettings      : TLogViewerSettings;
    FMainToolbar   : TToolBar;
    FComPortSettings : TComPortSettings;

    procedure FFormSettingsChanged(Sender: TObject);

  protected
    function GetActions: ILogViewerActions;
    function GetMenus: ILogViewerMenus;
    function GetManager: ILogViewerManager;

    procedure UpdateTabs;
    procedure UpdateStatusBar;

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

    end;

var
  frmMain: TfrmMain;

implementation

uses
  Spring,

  LogViewer.Receivers.WinIPC, LogViewer.Receivers.ComPort,
  LogViewer.Receivers.WinODS,
  LogViewer.Resources;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
var
  MV : ILogViewerMessagesView;
begin
  inherited AfterConstruction;
  FSettings := TLogViewerFactories.CreateSettings;
//  FComPortSettings := TComPortSettings.Create;
//  FComPortSettings.Port := 'COM3';
  FManager := TLogViewerFactories.CreateManager(Self, FSettings);

  FSettings.FormSettings.OnChanged.Add(FFormSettingsChanged);

  FMainToolbar := TLogViewerFactories.CreateMainToolbar(
    FManager,
    Self,
    Actions,
    Menus
  );

  MV := TLogViewerFactories.CreateMessagesView(
    FManager,
    pnlMainClient,
    TLogViewerFactories.CreateWinIPCChannelReceiver
  );
  Manager.AddView(MV);
  MV.Receiver.Enabled := True;
  ctMain.Tabs.Add;
  ctMain.ActiveTab.Data := Pointer(MV);
  ctMain.ActiveTab.Caption := Format('%s-%s', [
    MV.Form.Caption,
    MV.Receiver.Name
  ]);

//  MV := TLogViewerFactories.CreateMessagesView(
//    FManager,
//    pnlMainClient,
//    TLogViewerFactories.CreateComPortChannelReceiver(FComPortSettings)
//  );
//  Manager.AddView(MV);
//  ctMain.Tabs.Add;
//  ctMain.ActiveTab.Data := Pointer(MV);
//  ctMain.ActiveTab.Caption := Format('%s-%s', [
//    MV.Form.Caption,
//    MV.Receiver.Name
//  ]);
//  MV.Receiver.Enabled := True;
//
//  MV := TLogViewerFactories.CreateMessagesView(
//    FManager,
//    pnlMainClient,
//    TLogViewerFactories.CreateWinODSChannelReceiver
//  );
//  Manager.AddView(MV);
//  ctMain.Tabs.Add;
//  ctMain.ActiveTab.Data := Pointer(MV);
//  ctMain.ActiveTab.Caption := Format('%s-%s', [
//    MV.Form.Caption,
//    MV.Receiver.Name
//  ]);
//  MV.Receiver.Enabled := True;

  //(FManager as ILogViewerManager).ActiveView := FMessageViewer;
end;

procedure TfrmMain.BeforeDestruction;
begin
  FSettings.Free;
  FComPortSettings.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.ctMainActiveTabChanged(Sender: TObject; ATab: TChromeTab);
var
  MV : ILogViewerMessagesView;
begin
  if Assigned(ATab.Data) then
  begin
    MV := ILogViewerMessagesView(ATab.Data);
    MV.Form.Show;
    MV.Form.SetFocus;
    Manager.ActiveView := MV;
  end;
end;

procedure TfrmMain.ctMainButtonAddClick(Sender: TObject; var Handled: Boolean);
var
  LMessageViewer: ILogViewerMessagesView;
  LTab            : TChromeTab;
begin
  LMessageViewer := TLogViewerFactories.CreateMessagesView(
    FManager,
    pnlMainClient,
    FMessageViewer.Receiver
  );
  Manager.AddView(LMessageViewer);
  LTab := ctMain.Tabs.Add;
  LTab.Data := Pointer(LMessageViewer);
  LTab.Caption := Format('%s-%s', [
    LMessageViewer.Form.Caption,
    LMessageViewer.Receiver.Name
  ]);
  Handled := True;
end;

procedure TfrmMain.ctMainButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
  var Close: Boolean);
begin
  Close := False;
end;

procedure TfrmMain.ctMainNeedDragImageControl(Sender: TObject; ATab: TChromeTab;
  var DragControl: TWinControl);
begin
  DragControl := pnlMainClient;
end;

procedure TfrmMain.FFormSettingsChanged(Sender: TObject);
begin
  FSettings.FormSettings.AssignTo(Self);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMain.GetActions: ILogViewerActions;
begin
  Result := Manager.Actions;
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
procedure TfrmMain.UpdateStatusBar;
begin
//
end;

procedure TfrmMain.UpdateTabs;
var
  MV : ILogViewerMessagesView;
  CT : TChromeTab;
begin
  if Manager.Views.Count = 1 then
  begin
    ctMain.Visible := False;
//    if Assigned(Editor) then
//      Editor.Visible := True;
  end
  else
  begin
    ctMain.BeginUpdate;
    ctMain.Tabs.Clear;
    for MV in Manager.Views do
    begin
      CT := ctMain.Tabs.Add;
      //CT.Caption := ExtractFileName(EV.FileName);
      CT.Data := Pointer(MV);
    end;
    ctMain.Visible := True;
    ctMain.EndUpdate;
  end;
end;
{$ENDREGION}

end.
