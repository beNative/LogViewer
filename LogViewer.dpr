program LogViewer;



uses
  Forms,
  VirtualTrees,
  DDuce.Factories in '..\..\libraries\dduce\Source\Modules\DDuce.Factories.pas',
  DDuce.FormSettings in '..\..\libraries\dduce\Source\Modules\DDuce.FormSettings.pas',
  DDuce.Editor.ActionList.Templates in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.ActionList.Templates.pas',
  DDuce.Editor.ActionList.ToolView in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.ActionList.ToolView.pas' {frmActionListView},
  DDuce.Editor.AlignLines.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.AlignLines.Settings.pas',
  DDuce.Editor.AlignLines.ToolView in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.AlignLines.ToolView.pas',
  DDuce.Editor.CharacterMap.ToolView in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.CharacterMap.ToolView.pas',
  DDuce.Editor.Codeformatters in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Codeformatters.pas',
  DDuce.Editor.Codeformatters.Sql in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Codeformatters.Sql.pas',
  DDuce.Editor.Codetags in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Codetags.pas',
  DDuce.Editor.Colors.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Colors.Settings.pas',
  DDuce.Editor.Commands in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Commands.pas',
  DDuce.Editor.Commentstripper in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Commentstripper.pas',
  DDuce.Editor.Events in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Events.pas',
  DDuce.Editor.Factories.Manager in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Factories.Manager.pas',
  DDuce.Editor.Factories.Menus in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Factories.Menus.pas',
  DDuce.Editor.Factories in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Factories.pas',
  DDuce.Editor.Factories.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Factories.Settings.pas',
  DDuce.Editor.Factories.Toolbars in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Factories.Toolbars.pas',
  DDuce.Editor.Factories.Views in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Factories.Views.pas',
  DDuce.Editor.Filter.Toolview in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Filter.Toolview.pas',
  DDuce.Editor.Highlighters in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Highlighters.pas',
  DDuce.Editor.Interfaces in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Interfaces.pas',
  DDuce.Editor.Manager in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Manager.pas' {dmEditorManager: TDataModule},
  DDuce.Editor.Options.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Options.Settings.pas',
  DDuce.Editor.Resources in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Resources.pas' {ResourcesDataModule: TDataModule},
  DDuce.Editor.Search.Data in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Search.Data.pas',
  DDuce.Editor.Search.Engine in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Search.Engine.pas',
  DDuce.Editor.Search.Engine.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Search.Engine.Settings.pas',
  DDuce.Editor.Search.Templates in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Search.Templates.pas',
  DDuce.Editor.Search.Toolview in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Search.Toolview.pas',
  DDuce.Editor.Selectioninfo.ToolView in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Selectioninfo.ToolView.pas' {frmSelectionInfo},
  DDuce.Editor.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Settings.pas',
  DDuce.Editor.Sortstrings.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Sortstrings.Settings.pas',
  DDuce.Editor.Sortstrings.Toolview in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Sortstrings.Toolview.pas',
  DDuce.Editor.Test.ToolView in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Test.ToolView.pas',
  DDuce.Editor.Tools.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Tools.Settings.pas',
  DDuce.Editor.ToolView.Base in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.ToolView.Base.pas' {CustomEditorToolView},
  DDuce.Editor.ToolView.Manager in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.ToolView.Manager.pas',
  DDuce.Editor.Types in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Types.pas',
  DDuce.Editor.Utils in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Utils.pas',
  DDuce.Editor.View in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.View.pas' {EditorView},
  DDuce.Editor.Viewlist.Data in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Viewlist.Data.pas',
  DDuce.Editor.Viewlist.ToolView in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Viewlist.ToolView.pas' {frmViewList},
  DDuce.DynamicRecord in '..\..\libraries\dduce\Source\DDuce.DynamicRecord.pas',
  DDuce.RandomData in '..\..\libraries\dduce\Source\DDuce.RandomData.pas',
  DDuce.Reflect in '..\..\libraries\dduce\Source\DDuce.Reflect.pas',
  DDuce.ScopedReference in '..\..\libraries\dduce\Source\DDuce.ScopedReference.pas',
  DSharp.Bindings.Collections in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Bindings.Collections.pas',
  DSharp.Bindings.CollectionView in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Bindings.CollectionView.pas',
  DSharp.Bindings.Notifications in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Bindings.Notifications.pas',
  DSharp.Collections.ObservableCollection in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Collections.ObservableCollection.pas',
  DSharp.Core.Collections in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.Collections.pas',
  DSharp.Core.DataTemplates.Default in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.DataTemplates.Default.pas',
  DSharp.Core.DataTemplates in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.DataTemplates.pas',
  DSharp.Core.DependencyProperty in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.DependencyProperty.pas',
  DSharp.Core.Expressions in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.Expressions.pas',
  DSharp.Core.Framework in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.Framework.pas',
  DSharp.Core.PropertyChangedBase in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.PropertyChangedBase.pas',
  DSharp.Core.Reflection in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.Reflection.pas',
  DSharp.Core.Utils in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Core.Utils.pas',
  DSharp.Windows.ColumnDefinitions.ControlTemplate in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Windows.ColumnDefinitions.ControlTemplate.pas',
  DSharp.Windows.ColumnDefinitions in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Windows.ColumnDefinitions.pas',
  DSharp.Windows.ControlTemplates in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Windows.ControlTemplates.pas',
  DSharp.Windows.CustomPresenter in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Windows.CustomPresenter.pas',
  DSharp.Windows.CustomPresenter.Types in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Windows.CustomPresenter.Types.pas',
  DSharp.Windows.TreeViewPresenter in '..\..\libraries\dduce\Source\Dependencies\DSharp\DSharp.Windows.TreeViewPresenter.pas',
  DDuce.Components.DBGridView in '..\..\libraries\dduce\Source\Components\DDuce.Components.DBGridView.pas',
  DDuce.Components.Factories in '..\..\libraries\dduce\Source\Components\DDuce.Components.Factories.pas',
  DDuce.Components.GridView in '..\..\libraries\dduce\Source\Components\DDuce.Components.GridView.pas',
  DDuce.Components.Inspector in '..\..\libraries\dduce\Source\Components\DDuce.Components.Inspector.pas',
  DDuce.Components.LogTree in '..\..\libraries\dduce\Source\Components\DDuce.Components.LogTree.pas',
  DDuce.Components.PropertyInspector.CollectionEditor in '..\..\libraries\dduce\Source\Components\DDuce.Components.PropertyInspector.CollectionEditor.pas' {frmCollectionEditor},
  DDuce.Components.PropertyInspector in '..\..\libraries\dduce\Source\Components\DDuce.Components.PropertyInspector.pas',
  DDuce.Components.PropertyInspector.StringsEditor in '..\..\libraries\dduce\Source\Components\DDuce.Components.PropertyInspector.StringsEditor.pas' {StringsEditorDialog},
  DDuce.Components.XMLTree.Editors in '..\..\libraries\dduce\Source\Components\DDuce.Components.XMLTree.Editors.pas',
  DDuce.Components.XMLTree.NodeAttributes in '..\..\libraries\dduce\Source\Components\DDuce.Components.XMLTree.NodeAttributes.pas',
  DDuce.Components.XMLTree in '..\..\libraries\dduce\Source\Components\DDuce.Components.XMLTree.pas',
  DDuce.WinIPC.Client in '..\..\libraries\dduce\Source\DDuce.WinIPC.Client.pas',
  DDuce.Logger.Channels.Base in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Channels.Base.pas',
  DDuce.Logger.Channels.LogFile in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Channels.LogFile.pas',
  DDuce.Logger.Channels.WinIPC in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Channels.WinIPC.pas',
  DDuce.Logger.Channels.ZeroMQ in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Channels.ZeroMQ.pas',
  DDuce.Logger in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.pas',
  DDuce.Logger.Interfaces in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Interfaces.pas',
  DDuce.Logger.Factories in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Factories.pas',
  LogViewer.Watches.Data in 'LogViewer.Watches.Data.pas',
  DDuce.WinIPC.Server in '..\..\libraries\dduce\Source\DDuce.WinIPC.Server.pas',
  DDuce.Logger.Base in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Base.pas',
  JsonDataObjects in '..\..\libraries\JsonDataObjects\Source\JsonDataObjects.pas',
  LogViewer.Settings in 'LogViewer.Settings.pas',
  LogViewer.MessageList.View in 'LogViewer.MessageList.View.pas' {frmMessageList},
  LogViewer.CallStack.Data in 'LogViewer.CallStack.Data.pas',
  LogViewer.Receivers.WinIPC in 'LogViewer.Receivers.WinIPC.pas',
  LogViewer.Receivers.ZeroMQ in 'LogViewer.Receivers.ZeroMQ.pas',
  LogViewer.Interfaces in 'LogViewer.Interfaces.pas',
  LogViewer.Receivers.WinODS in 'LogViewer.Receivers.WinODS.pas',
  DDuce.Editor.Filter.Data in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Filter.Data.pas',
  DDuce.Editor.Filter.Settings in '..\..\libraries\dduce\Source\Modules\Editor\DDuce.Editor.Filter.Settings.pas',
  uCustomImageDrawHook in 'uCustomImageDrawHook.pas',
  LogViewer.Resources in 'LogViewer.Resources.pas',
  DDuce.ObjectInspector.zObjectInspector in '..\..\libraries\dduce\Source\Modules\ObjectInspector\DDuce.ObjectInspector.zObjectInspector.pas' {frmComponentInspectorzObjectInspector},
  LogViewer.MainForm in 'LogViewer.MainForm.pas' {frmMain},
  LogViewer.Watches.View in 'LogViewer.Watches.View.pas' {frmWatchesView},
  LogViewer.CallStack.View in 'LogViewer.CallStack.View.pas' {frmCallStackView},
  LogViewer.Factories in 'LogViewer.Factories.pas',
  LogViewer.Manager in 'LogViewer.Manager.pas' {dmManager: TDataModule},
  Spring.DesignPatterns in '..\..\libraries\spring4d\Source\Base\Spring.DesignPatterns.pas',
  Spring.Events.Base in '..\..\libraries\spring4d\Source\Base\Spring.Events.Base.pas',
  Spring.Events in '..\..\libraries\spring4d\Source\Base\Spring.Events.pas',
  Spring.Helpers in '..\..\libraries\spring4d\Source\Base\Spring.Helpers.pas',
  Spring.MethodIntercept in '..\..\libraries\spring4d\Source\Base\Spring.MethodIntercept.pas',
  Spring in '..\..\libraries\spring4d\Source\Base\Spring.pas',
  Spring.Reflection in '..\..\libraries\spring4d\Source\Base\Spring.Reflection.pas',
  Spring.ResourceStrings in '..\..\libraries\spring4d\Source\Base\Spring.ResourceStrings.pas',
  Spring.SystemUtils in '..\..\libraries\spring4d\Source\Base\Spring.SystemUtils.pas',
  Spring.Times in '..\..\libraries\spring4d\Source\Base\Spring.Times.pas',
  Spring.ValueConverters in '..\..\libraries\spring4d\Source\Base\Spring.ValueConverters.pas',
  Spring.VirtualClass in '..\..\libraries\spring4d\Source\Base\Spring.VirtualClass.pas',
  Spring.VirtualInterface in '..\..\libraries\spring4d\Source\Base\Spring.VirtualInterface.pas',
  Spring.Collections.Adapters in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Adapters.pas',
  Spring.Collections.Base in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Base.pas',
  Spring.Collections.Dictionaries in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Dictionaries.pas',
  Spring.Collections.Enumerable in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Enumerable.pas',
  Spring.Collections.Events in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Events.pas',
  Spring.Collections.Extensions in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Extensions.pas',
  Spring.Collections.LinkedLists in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.LinkedLists.pas',
  Spring.Collections.Lists in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Lists.pas',
  Spring.Collections.MultiMaps in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.MultiMaps.pas',
  Spring.Collections in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.pas',
  Spring.Collections.Queues in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Queues.pas',
  Spring.Collections.Sets in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Sets.pas',
  Spring.Collections.Stacks in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Stacks.pas',
  Spring.Logging.Appenders.Base in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.Appenders.Base.pas',
  Spring.Logging.Appenders in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.Appenders.pas',
  Spring.Logging.Controller in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.Controller.pas',
  Spring.Logging.Extensions in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.Extensions.pas',
  Spring.Logging.Loggers in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.Loggers.pas',
  Spring.Logging.NullLogger in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.NullLogger.pas',
  Spring.Logging in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.pas',
  Spring.Logging.ResourceStrings in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.ResourceStrings.pas',
  Spring.Logging.Serializers in '..\..\libraries\spring4d\Source\Base\Logging\Spring.Logging.Serializers.pas',
  Spring.Patches.GetInvokeInfo in '..\..\libraries\spring4d\Source\Base\Patches\Spring.Patches.GetInvokeInfo.pas',
  Spring.Patches.QC93646 in '..\..\libraries\spring4d\Source\Base\Patches\Spring.Patches.QC93646.pas',
  Spring.Patches.QC98671 in '..\..\libraries\spring4d\Source\Base\Patches\Spring.Patches.QC98671.pas',
  Spring.Patches.QC107219 in '..\..\libraries\spring4d\Source\Base\Patches\Spring.Patches.QC107219.pas',
  LogViewer.Settings.Dialog in 'LogViewer.Settings.Dialog.pas' {frmLogViewerSettings},
  LogViewer.Receivers.ComPort in 'LogViewer.Receivers.ComPort.pas',
  ZeroMQ.API in '..\..\libraries\Delphi-ZeroMQ\ZeroMQ.API.pas',
  ZeroMQ in '..\..\libraries\Delphi-ZeroMQ\ZeroMQ.pas',
  synaser in '..\..\libraries\synapse\synaser.pas',
  synafpc in '..\..\libraries\synapse\synafpc.pas',
  synautil in '..\..\libraries\synapse\synautil.pas',
  LogViewer.Factories.Toolbars in 'LogViewer.Factories.Toolbars.pas',
  LogViewer.MessageList.Settings in 'LogViewer.MessageList.Settings.pas',
  LogViewer.Commands in 'LogViewer.Commands.pas',
  LogViewer.Events in 'LogViewer.Events.pas',
  LogViewer.ComPort.Settings in 'LogViewer.ComPort.Settings.pas',
  LogViewer.AddMessageView.Dialog in 'LogViewer.AddMessageView.Dialog.pas' {frmAddMessageView},
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
  NativeXml in '..\..\libraries\dduce\Source\Dependencies\NativeXml\NativeXml.pas',
  NativeXmlCodepages in '..\..\libraries\dduce\Source\Dependencies\NativeXml\NativeXmlCodepages.pas',
  NativeXmlNodes in '..\..\libraries\dduce\Source\Dependencies\NativeXml\NativeXmlNodes.pas',
  NativeXmlObjectStorage in '..\..\libraries\dduce\Source\Dependencies\NativeXml\NativeXmlObjectStorage.pas',
  sdDebug in '..\..\libraries\dduce\Source\Dependencies\NativeXml\sdDebug.pas',
  sdStreams in '..\..\libraries\dduce\Source\Dependencies\NativeXml\sdStreams.pas',
  sdStringTable in '..\..\libraries\dduce\Source\Dependencies\NativeXml\sdStringTable.pas',
  DDuce.Factories.VirtualTrees in '..\..\libraries\dduce\Source\Factories\DDuce.Factories.VirtualTrees.pas',
  DDuce.Factories.zObjInspector in '..\..\libraries\dduce\Source\Factories\DDuce.Factories.zObjInspector.pas',
  DDuce.Factories.GridView in '..\..\libraries\dduce\Source\Factories\DDuce.Factories.GridView.pas',
  Spring.Collections.Trees in '..\..\libraries\spring4d\Source\Base\Collections\Spring.Collections.Trees.pas',
  LogViewer.Dashboard.View in 'LogViewer.Dashboard.View.pas' {frmDashboard};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Log viewer';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

