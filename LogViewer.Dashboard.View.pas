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
    {$REGION 'designer controls'}
    aclMain           : TActionList;
    chkComPortEnabled : TCheckBox;
    chkWinIPCEnabled  : TCheckBox;
    chkWinODSEnabled  : TCheckBox;
    chkZeroMQEnabled  : TCheckBox;
    pnlLogChannels    : TPanel;
    {$ENDREGION}

    procedure chkWinIPCEnabledClick(Sender: TObject);
    procedure chkWinODSEnabledClick(Sender: TObject);
    procedure chkZeroMQEnabledClick(Sender: TObject);
    procedure chkComPortEnabledClick(Sender: TObject);

  private
    FManager             : ILogViewerManager;
    FVSTChannelReceivers : TVirtualStringTree;
//    FTVPChannelReceivers : TTreeViewPresenter;

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
  Spring, Spring.Collections,

  DDuce.Factories, DDuce.Factories.VirtualTrees,

  LogViewer.Factories;

{$REGION 'construction and destruction'}
constructor TfrmDashboard.Create(AOwner: TComponent;
  AManager: ILogViewerManager);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AManager, 'AManager');
  FManager := AManager;
end;

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
//    IObjectList(FManager.Receivers.AsList.AsReadOnlyList)
//  );

  R := TLogViewerFactories.CreateWinIPCChannelReceiver(FManager);
  FManager.AddReceiver(R);
  R.Enabled := FManager.Settings.WinIPCSettings.Enabled;

//  R := TLogViewerFactories.CreateWinODSChannelReceiver(FManager);
//  FManager.AddReceiver(R);
//  R.Enabled := FManager.Settings.WinODSSettings.Enabled;

  R := TLogViewerFactories.CreateZeroMQChannelReceiver(FManager);
  FManager.AddReceiver(R);
  R.Enabled := FManager.Settings.ZeroMQSettings.Enabled;
end;

procedure TfrmDashboard.chkComPortEnabledClick(Sender: TObject);
begin
  //FManager.Settings.ComPortSettings.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TfrmDashboard.chkWinIPCEnabledClick(Sender: TObject);
begin
  FManager.Settings.WinIPCSettings.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TfrmDashboard.chkWinODSEnabledClick(Sender: TObject);
begin
  FManager.Settings.WinODSSettings.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TfrmDashboard.chkZeroMQEnabledClick(Sender: TObject);
begin
  FManager.Settings.ZeroMQSettings.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TfrmDashboard.UpdateActions;
begin
  chkWinIPCEnabled.Checked := FManager.Settings.WinIPCSettings.Enabled;
  chkWinODSEnabled.Checked := FManager.Settings.WinODSSettings.Enabled;
  chkZeroMQEnabled.Checked := FManager.Settings.ZeroMQSettings.Enabled;
  //chkComPortEnabled.Checked := FManager.Settings.ComPortSettings.
  FManager.Actions.UpdateActions;

  inherited UpdateActions;
end;
{$ENDREGION}

end.
