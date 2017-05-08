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
  LogViewer.Factories, LogViewer.Manager, LogViewer.Settings;

type
  TfrmMain = class(TForm)
    sbrMain       : TStatusBar;
    ctMain        : TChromeTabs;
    pnlMainClient : TPanel;

    procedure ctMainActiveTabChanged(Sender: TObject; ATab: TChromeTab);
    procedure ctMainButtonAddClick(Sender: TObject; var Handled: Boolean);
    procedure ctMainButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
      var Close: Boolean);
    procedure ctMainNeedDragImageControl(Sender: TObject; ATab: TChromeTab;
      var DragControl: TWinControl);

  private
    FMessageViewerIPC : TfrmMessageList;
    FReceiver      : IChannelReceiver;
//    FMessageViewerODS : TfrmMessageList;
    //FReceiverODS      : IChannelReceiver;
    FManager          : TdmManager;
    FSettings         : TLogViewerSettings;
    FMainToolbar      : TToolBar;

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
  LogViewer.Receivers.WinIPC, LogViewer.Receivers.Serial,
  LogViewer.Receivers.WinODS,
  LogViewer.Resources;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings := TLogViewerSettings.Create;
  //FReceiver := TWinIPChannelReceiver.Create;
  //FReceiver := TWinODSReceiver.Create;
  FReceiver := TSerialPortReceiver.Create;
  FManager := TLogViewerFactories.CreateManager(Self);
  FMessageViewerIPC := TLogViewerFactories.CreateMessagesView(
    FManager,
    pnlMainClient,
    FReceiver
  );
//  FMessageViewerODS := TLogViewerFactories.CreateMessagesView(
//    FManager,
//    tsODS,
//    FReceiverODS
//  );
  FMainToolbar := TLogViewerFactories.CreateMainToolbar(
    FManager,
    Self,
    Actions,
    Menus
  );

  (FManager as ILogViewerManager).ActiveView := FMessageViewerIPC;

  FReceiver.Enabled := True;
  //FReceiverODS.Enabled := True;
end;

procedure TfrmMain.BeforeDestruction;
begin
  FReceiver := nil;
  FSettings.Free;
  inherited BeforeDestruction;
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

procedure TfrmMain.UpdateStatusBar;
begin
//
end;

procedure TfrmMain.UpdateTabs;
//var
  //EV : IEditorView;
  //CT : TChromeTab;
begin
//  if Views.Count = 1 then
//  begin
//    ctMain.Visible := False;
//    if Assigned(Editor) then
//      Editor.Visible := True;
//  end
//  else
//  begin
//    ctMain.BeginUpdate;
//    ctMain.Tabs.Clear;
//    //for EV in Views do
//    begin
//      CT := ctMain.Tabs.Add;
//      CT.Caption := ExtractFileName(EV.FileName);
//      CT.Data := Pointer(EV);
//    end;
//    ctMain.Visible := True;
//    ctMain.EndUpdate;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.ctMainActiveTabChanged(Sender: TObject; ATab: TChromeTab);
begin
//
end;

procedure TfrmMain.ctMainButtonAddClick(Sender: TObject; var Handled: Boolean);
begin
//
end;

procedure TfrmMain.ctMainButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
  var Close: Boolean);
begin
//
end;

procedure TfrmMain.ctMainNeedDragImageControl(Sender: TObject; ATab: TChromeTab;
  var DragControl: TWinControl);
begin
  DragControl := pnlMainClient;
end;
{$ENDREGION}

end.
