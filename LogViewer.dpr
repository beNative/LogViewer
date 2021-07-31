{$SetPEFlags 1} // strip relocation info

program LogViewer;

{$R *.dres}

uses
  System.SysUtils,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  VirtualTrees,
  DDuce.CustomImageDrawHook,
  DDuce.Logger,
  DDuce.Logger.Interfaces,
  DDuce.Logger.Channels.Zmq,
  LogViewer.CallStack.Data in 'LogViewer.CallStack.Data.pas',
  LogViewer.CallStack.Settings in 'LogViewer.CallStack.Settings.pas',
  LogViewer.CallStack.View in 'LogViewer.CallStack.View.pas' {frmCallStackView},
  LogViewer.Commands in 'LogViewer.Commands.pas',
  LogViewer.ComPort.Settings in 'LogViewer.ComPort.Settings.pas',
  LogViewer.Dashboard.Data in 'LogViewer.Dashboard.Data.pas',
  LogViewer.Dashboard.View in 'LogViewer.Dashboard.View.pas' {frmDashboard},
  LogViewer.DataSet.View in 'LogViewer.DataSet.View.pas' {frmDataSetView},
  LogViewer.DisplayValues.Settings in 'LogViewer.DisplayValues.Settings.pas',
  LogViewer.DisplayValues.Settings.View in 'LogViewer.DisplayValues.Settings.View.pas' {frmDisplayValuesSettings},
  LogViewer.Events in 'LogViewer.Events.pas',
  LogViewer.Factories in 'LogViewer.Factories.pas',
  LogViewer.Factories.Toolbars in 'LogViewer.Factories.Toolbars.pas',
  LogViewer.Image.View in 'LogViewer.Image.View.pas' {frmImageView},
  LogViewer.Interfaces in 'LogViewer.Interfaces.pas',
  LogViewer.MainForm in 'LogViewer.MainForm.pas' {frmMain},
  LogViewer.Manager in 'LogViewer.Manager.pas' {dmManager: TDataModule},
  LogViewer.MessageFilter.Data in 'LogViewer.MessageFilter.Data.pas',
  LogViewer.MessageFilter.View in 'LogViewer.MessageFilter.View.pas' {frmMessageFilter},
  LogViewer.MessageList.LogNode in 'LogViewer.MessageList.LogNode.pas',
  LogViewer.MessageList.Settings in 'LogViewer.MessageList.Settings.pas',
  LogViewer.MessageList.Settings.View in 'LogViewer.MessageList.Settings.View.pas' {frmViewSettings},
  LogViewer.MessageList.View in 'LogViewer.MessageList.View.pas' {frmMessageList},
  LogViewer.RawData.View in 'LogViewer.RawData.View.pas' {frmRawDataView},
  LogViewer.Receivers.Base in 'LogViewer.Receivers.Base.pas',
  LogViewer.Receivers.ComPort in 'LogViewer.Receivers.ComPort.pas',
  LogViewer.Receivers.ComPort.Settings in 'LogViewer.Receivers.ComPort.Settings.pas',
  LogViewer.Receivers.ComPort.Settings.View in 'LogViewer.Receivers.ComPort.Settings.View.pas' {frmComPortSettings},
  LogViewer.Receivers.FileSystem in 'LogViewer.Receivers.FileSystem.pas',
  LogViewer.Receivers.FileSystem.Settings in 'LogViewer.Receivers.FileSystem.Settings.pas',
  LogViewer.Receivers.Winipc in 'LogViewer.Receivers.Winipc.pas',
  LogViewer.Receivers.Winipc.Settings in 'LogViewer.Receivers.Winipc.Settings.pas',
  LogViewer.Receivers.Winipc.Settings.View in 'LogViewer.Receivers.Winipc.Settings.View.pas' {frmWinipcSettings},
  LogViewer.Receivers.Winods in 'LogViewer.Receivers.Winods.pas',
  LogViewer.Receivers.Winods.Settings in 'LogViewer.Receivers.Winods.Settings.pas',
  LogViewer.Receivers.Winods.Settings.View in 'LogViewer.Receivers.Winods.Settings.View.pas' {frmWinodsSettings},
  LogViewer.Receivers.Zmq in 'LogViewer.Receivers.Zmq.pas',
  LogViewer.Receivers.Zmq.Settings in 'LogViewer.Receivers.Zmq.Settings.pas',
  LogViewer.Receivers.Zmq.Settings.View in 'LogViewer.Receivers.Zmq.Settings.View.pas' {frmZmqSettings},
  LogViewer.Resources in 'LogViewer.Resources.pas',
  LogViewer.Settings.Dialog.Data in 'LogViewer.Settings.Dialog.Data.pas',
  LogViewer.Settings.Dialog in 'LogViewer.Settings.Dialog.pas' {frmLogViewerSettings},
  LogViewer.Settings in 'LogViewer.Settings.pas',
  LogViewer.Subscribers.Base in 'LogViewer.Subscribers.Base.pas',
  LogViewer.Subscribers.ComPort in 'LogViewer.Subscribers.ComPort.pas',
  LogViewer.Subscribers.FileSystem in 'LogViewer.Subscribers.FileSystem.pas',
  LogViewer.Subscribers.Winipc in 'LogViewer.Subscribers.Winipc.pas',
  LogViewer.Subscribers.Winods in 'LogViewer.Subscribers.Winods.pas',
  LogViewer.Subscribers.Zmq in 'LogViewer.Subscribers.Zmq.pas',
  LogViewer.ValueList.View in 'LogViewer.ValueList.View.pas' {frmValueListView},
  LogViewer.Watches.Data in 'LogViewer.Watches.Data.pas',
  LogViewer.Watches.Settings in 'LogViewer.Watches.Settings.pas',
  LogViewer.Watches.Settings.View in 'LogViewer.Watches.Settings.View.pas' {frmWatchSettings},
  LogViewer.Watches.View in 'LogViewer.Watches.View.pas' {frmWatchesView},
  LogViewer.MessageData.View in 'LogViewer.MessageData.View.pas' {TfrmMessageData},
  LogViewer.CallStack.Settings.View in 'LogViewer.CallStack.Settings.View.pas' {frmCallStackSettings},
  LogViewer.DisplayValues.Settings.ValueManager in 'LogViewer.DisplayValues.Settings.ValueManager.pas',
  LogViewer.LogLevels.Settings in 'LogViewer.LogLevels.Settings.pas',
  LogViewer.LogLevels.Settings.View in 'LogViewer.LogLevels.Settings.View.pas' {frmLogLevelSettings},
  LogViewer.WinipcBroker in 'LogViewer.WinipcBroker.pas';

{$R *.res}

begin
  {$WARNINGS OFF}
  ReportMemoryLeaksOnShutdown := DebugHook > 0;
  {$WARNINGS ON}
  Application.Initialize;
  Application.Title := 'Log viewer';
  Application.CreateForm(TfrmMain, frmMain);
  Logger.Info('LogViewer Started.');
  Application.Run;
end.


