program LogViewer;

{$R *.dres}

uses
  //LeakCheck,
  System.SysUtils,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  VirtualTrees,
  DDuce.CustomImageDrawHook,
  DDuce.Logger,
  DDuce.Logger.Interfaces,
  DDuce.Logger.Channels.ZeroMQ,
  LogViewer.Watches.Data in 'LogViewer.Watches.Data.pas',
  LogViewer.Settings in 'LogViewer.Settings.pas',
  LogViewer.MessageList.View in 'LogViewer.MessageList.View.pas' {frmMessageList},
  LogViewer.CallStack.Data in 'LogViewer.CallStack.Data.pas',
  LogViewer.Receivers.WinIPC in 'LogViewer.Receivers.WinIPC.pas',
  LogViewer.Receivers.ZeroMQ in 'LogViewer.Receivers.ZeroMQ.pas',
  LogViewer.Interfaces in 'LogViewer.Interfaces.pas',
  LogViewer.Resources in 'LogViewer.Resources.pas',
  LogViewer.MainForm in 'LogViewer.MainForm.pas' {frmMain},
  LogViewer.Watches.View in 'LogViewer.Watches.View.pas' {frmWatchesView},
  LogViewer.CallStack.View in 'LogViewer.CallStack.View.pas' {frmCallStackView},
  LogViewer.Factories in 'LogViewer.Factories.pas',
  LogViewer.Manager in 'LogViewer.Manager.pas' {dmManager: TDataModule},
  LogViewer.Settings.Dialog in 'LogViewer.Settings.Dialog.pas' {frmLogViewerSettings},
  LogViewer.Receivers.ComPort in 'LogViewer.Receivers.ComPort.pas',
  LogViewer.Factories.Toolbars in 'LogViewer.Factories.Toolbars.pas',
  LogViewer.MessageList.Settings in 'LogViewer.MessageList.Settings.pas',
  LogViewer.Commands in 'LogViewer.Commands.pas',
  LogViewer.Events in 'LogViewer.Events.pas',
  LogViewer.ComPort.Settings in 'LogViewer.ComPort.Settings.pas',
  LogViewer.ComPort.Settings.View in 'LogViewer.ComPort.Settings.View.pas' {frmComPortSettings},
  LogViewer.WinODS.Settings.View in 'LogViewer.WinODS.Settings.View.pas' {frmWinODSSettings},
  LogViewer.WinIPC.Settings.View in 'LogViewer.WinIPC.Settings.View.pas' {frmWinIPCSettings},
  LogViewer.ZeroMQ.Settings.View in 'LogViewer.ZeroMQ.Settings.View.pas' {frmZeroMQSettings},
  LogViewer.WinODS.Settings in 'LogViewer.WinODS.Settings.pas',
  LogViewer.ZeroMQ.Settings in 'LogViewer.ZeroMQ.Settings.pas',
  LogViewer.WinIPC.Settings in 'LogViewer.WinIPC.Settings.pas',
  LogViewer.Watches.Settings in 'LogViewer.Watches.Settings.pas',
  LogViewer.CallStack.Settings in 'LogViewer.CallStack.Settings.pas',
  LogViewer.Watches.Settings.View in 'LogViewer.Watches.Settings.View.pas' {frmWatchSettings},
  LogViewer.MessageList.LogNode in 'LogViewer.MessageList.LogNode.pas',
  LogViewer.Dashboard.View in 'LogViewer.Dashboard.View.pas' {frmDashboard},
  LogViewer.Receivers.Base in 'LogViewer.Receivers.Base.pas',
  LogViewer.DisplayValues.Settings in 'LogViewer.DisplayValues.Settings.pas',
  LogViewer.DisplayValues.Settings.View in 'LogViewer.DisplayValues.Settings.View.pas' {frmDisplayValuesSettings},
  LogViewer.Subscribers.ZeroMQ in 'LogViewer.Subscribers.ZeroMQ.pas',
  LogViewer.Subscribers.ComPort in 'LogViewer.Subscribers.ComPort.pas',
  LogViewer.Subscribers.WinIPC in 'LogViewer.Subscribers.WinIPC.pas',
  LogViewer.Subscribers.Base in 'LogViewer.Subscribers.Base.pas',
  LogViewer.Subscribers.WinODS in 'LogViewer.Subscribers.WinODS.pas',
  LogViewer.ValueList.View in 'LogViewer.ValueList.View.pas' {frmValueList},
  LogViewer.Receivers.WinODS in 'LogViewer.Receivers.WinODS.pas',
  LogViewer.MessageFilter.View in 'LogViewer.MessageFilter.View.pas' {frmMessageFilter},
  LogViewer.MessageFilter.Data in 'LogViewer.MessageFilter.Data.pas',
  LogViewer.Receivers.FileSystem in 'LogViewer.Receivers.FileSystem.pas',
  LogViewer.Subscribers.FileSystem in 'LogViewer.Subscribers.FileSystem.pas',
  LogViewer.Dashboard.Data in 'LogViewer.Dashboard.Data.pas',
  LogViewer.MessageList.Data in 'LogViewer.MessageList.Data.pas',
  LogViewer.Settings.Dialog.Data in 'LogViewer.Settings.Dialog.Data.pas',
  LogViewer.Receivers.MQTT in 'LogViewer.Receivers.MQTT.pas',
  LogViewer.Subscribers.MQTT in 'LogViewer.Subscribers.MQTT.pas',
  LogViewer.FileSystem.Settings in 'LogViewer.FileSystem.Settings.pas',
  LogViewer.MQTT.Settings in 'LogViewer.MQTT.Settings.pas';

{$R *.res}

begin
  {$WARNINGS OFF}
  ReportMemoryLeaksOnShutdown := DebugHook > 0;
  {$WARNINGS ON}
  Application.Initialize;
  Application.Title := 'Log viewer';
  // setup logchannel for using a log LogViewer instance to debug itself.
  Logger.Channels.Add(
    TZeroMQChannel.Create(Format('tcp://*:%d', [LOGVIEWER_ZMQ_PORT]))
  );
  Logger.Clear;
  Logger.Info('LogViewer Started.');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.


