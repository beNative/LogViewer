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

unit LogViewer.Dashboard.View;

interface

{ Provides an overview of all active channels. }

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ActnList,

  VirtualTrees,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,


  LogViewer.Interfaces;

type
  TfrmDashboard = class(TForm)
    aclMain                : TActionList;
    actAddComPortLogViewer : TAction;
    actAddWinIPCLogViewer  : TAction;
    actAddWinODSLogViewer  : TAction;
    actAddZeroMQLogViewer  : TAction;
    btnAddComPortLogViewer : TButton;
    btnAddWinIPCLogViewer  : TButton;
    btnAddWinODSLogViewer  : TButton;
    btnAddZeroMQLogViewer  : TButton;
    chkComPortEnabled      : TCheckBox;
    chkWinIPCEnabled       : TCheckBox;
    chkWinODSEnabled       : TCheckBox;
    chkZeroMQEnabled       : TCheckBox;
    pnlLogChannels         : TPanel;

    procedure actAddWinIPCLogViewerExecute(Sender: TObject);
    procedure actAddWinODSLogViewerExecute(Sender: TObject);
    procedure actAddZeroMQLogViewerExecute(Sender: TObject);
    procedure actAddComPortLogViewerExecute(Sender: TObject);

  private
    FManager             : ILogViewerManager;
    FVSTChannelReceivers : TVirtualStringTree;
    FTVPChannelReceivers : TTreeViewPresenter;

  protected
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner   : TComponent;
      AManager : ILogViewerManager
    ); reintroduce; virtual;
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Spring,

  DDuce.Factories.VirtualTrees,

  LogViewer.Factories;

{$REGION 'construction and destruction'}
procedure TfrmDashboard.AfterConstruction;
var
  R : IChannelReceiver;
begin
  inherited AfterConstruction;
  FVSTChannelReceivers := TVirtualStringTreeFactory.CreateList(Self, pnlLogChannels);
  FVSTChannelReceivers.AlignWithMargins := False;
//  FTVPChannelReceivers := TFactories.CreateTreeViewPresenter(
//    Self,
//    FVSTChannelReceivers,
//    (FManager.Receivers as IInterfaceList).AsReadOnlyList as IObjectList
//  );

  //TLogViewerFactories.CreateComPortChannelReceiver(nil);
  R := TLogViewerFactories.CreateWinIPCChannelReceiver;
  FManager.AddReceiver(R);
  R.Enabled := True;
end;

constructor TfrmDashboard.Create(AOwner: TComponent;
  AManager: ILogViewerManager);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AManager, 'AManager');
  FManager := AManager;
end;

procedure TfrmDashboard.UpdateActions;
begin
  actAddWinIPCLogViewer.Enabled  := chkWinIPCEnabled.Checked;
  actAddWinODSLogViewer.Enabled  := chkWinODSEnabled.Checked;
  actAddZeroMQLogViewer.Enabled  := chkZeroMQEnabled.Checked;
  actAddComPortLogViewer.Enabled := chkComPortEnabled.Checked;

  FManager.Actions.UpdateActions;

  inherited UpdateActions;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDashboard.actAddComPortLogViewerExecute(Sender: TObject);
//var
//  MV : ILogViewer;
begin
//  MV := TLogViewerFactories.CreateLogViewer(
//    FManager,
//    TLogViewerFactories.CreateComPortChannelReceiver(nil)
//  );
//  FManager.AddView(MV);
end;

procedure TfrmDashboard.actAddWinIPCLogViewerExecute(Sender: TObject);
//var
//  MV : ILogViewer;
begin
//  MV := TLogViewerFactories.CreateLogViewer(
//    FManager,
//    TLogViewerFactories.CreateWinIPCChannelReceiver
//  );
//  FManager.AddView(MV);
end;

procedure TfrmDashboard.actAddWinODSLogViewerExecute(Sender: TObject);
//var
//  MV : ILogViewer;
begin
//  MV := TLogViewerFactories.CreateLogViewer(
//    FManager,
//    TLogViewerFactories.CreateWinODSChannelReceiver
//  );
//  FManager.AddView(MV);
end;

procedure TfrmDashboard.actAddZeroMQLogViewerExecute(Sender: TObject);
//var
//  MV : ILogViewer;
begin
//  MV := TLogViewerFactories.CreateLogViewer(
//    FManager,
//    TLogViewerFactories.CreateZeroMQChannelReceiver
//  );
//  FManager.AddView(MV);
end;
{$ENDREGION}

end.
