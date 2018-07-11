program LogViewer;

{$R *.dres}

uses
  Forms,
  VirtualTrees,
  LogViewer.Watches.Data in 'LogViewer.Watches.Data.pas',
  LogViewer.Settings in 'LogViewer.Settings.pas',
  LogViewer.MessageList.View in 'LogViewer.MessageList.View.pas' {frmMessageList},
  LogViewer.CallStack.Data in 'LogViewer.CallStack.Data.pas',
  LogViewer.Receivers.WinIPC in 'LogViewer.Receivers.WinIPC.pas',
  LogViewer.Receivers.ZeroMQ in 'LogViewer.Receivers.ZeroMQ.pas',
  LogViewer.Interfaces in 'LogViewer.Interfaces.pas',
  LogViewer.Receivers.WinODS in 'LogViewer.Receivers.WinODS.pas',
  uCustomImageDrawHook in 'uCustomImageDrawHook.pas',
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
  LogViewer.Settings.Dialog.ConfigNode in 'LogViewer.Settings.Dialog.ConfigNode.pas',
  LogViewer.Watches.Settings in 'LogViewer.Watches.Settings.pas',
  LogViewer.CallStack.Settings in 'LogViewer.CallStack.Settings.pas',
  LogViewer.Watches.Settings.View in 'LogViewer.Watches.Settings.View.pas' {frmWatchSettings},
  LogViewer.MessageList.LogNode in 'LogViewer.MessageList.LogNode.pas',
  LogViewer.Dashboard.View in 'LogViewer.Dashboard.View.pas' {frmDashboard},
  LogViewer.Receivers.Base in 'LogViewer.Receivers.Base.pas',
  LogViewer.LogQueue in 'LogViewer.LogQueue.pas',
  LogViewer.DisplayValues.Settings in 'LogViewer.DisplayValues.Settings.pas',
  LogViewer.DisplayValues.Settings.View in 'LogViewer.DisplayValues.Settings.View.pas' {frmDisplayValuesSettings},
  LogViewer.Dashboard.View.Node in 'LogViewer.Dashboard.View.Node.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Log viewer';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

