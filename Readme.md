# LogViewer

LogViewer is a general purpose message viewer for log messages with support for multiple sources to receive messages from. The application is a modular design which can easily be extended to support other message types and transfer protocols.

Currently supported are:

* Windows IPC (using ``WM_COPY`` messages)
* [ZeroMQ](https://github.com/zeromq/libzmq) (`PUB`/`SUB` socket) for logging over the network. It demonstrates the brilliant performance of ZMQ sockets.

Currently in progress:
* Serial port (both native RS232 or Bluetooth)
* Windows ``OutputDebugString`` API
* MQTT (commonly used by IOT devices)

Each of these receivers support multiple subscribers for which messages can be displayed in a dedicated logviewer.

I compiled a pre-release version in the releases section.

## Screenshots

### Message viewer

![LogViewer](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer.png)

![LogViewer](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer1.png)

![LogViewer](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer2.png)

![LogViewer](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer3.png)

### Call Stack

![CallStack](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer.CallStack.png)

### Watch List

Watch list with value history support. The history list can be automatically synchronize with the message list to monitor the value at any given time.

![WatchList](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer.WatchList.png)

### Settings Dialog

Fully customizable message display settings.

![Settings](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer.SettingsDialog.png)

## UML

In the [Documents](https://github.com/beNative/LogViewer/tree/master/Documents) folder you will find some UML diagrams which reveal the main application architecture. 

![ILogViewerManager](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer.ILogViewerManager.png)
![TLogViewerSettings](https://github.com/beNative/LogViewer/blob/master/Images/LogViewer.TLogViewerSettings.png)

There is also a mindmap with a technical overview of the application: [LogViewer.xmind](https://github.com/beNative/LogViewer/blob/master/Documents/LogViewer.xmind)
You need XMind to open this document. You can download a free version from the main website: [XMind](https://www.xmind.net/).

I made an image snapshot of the mindmap that you can open here: [LogViewer.Mindmap.png](https://raw.githubusercontent.com/beNative/LogViewer/master/Documents/LogViewer.Mindmap.png)

## ILogger API

The [DDuce](http://github.com/beNative/dduce) library provides the Logger module to add logging to your application (Delphi/FreePascal-Lazarus).
The DDuce demo application demonstrates the currently supported message types.

![Logger demo](https://github.com/beNative/LogViewer/blob/master/Wiki/DDuce%20Logger24-10-2018%2022-12-44.png)

More examples can be found in my other Delphi projects listed on Github. LogViewer is also able to emit log messages by itself which can be monitored in another instance. Therefore the source instance needs to have the setting 'Emit log messages' enabled. The other instance needs to be started in 'debug mode' which also can be configured in the application settings.

A subset of this API has been ported to FPC/Lazarus. Take a look at my Lazarus projects to see examples how they are used.

## Features currently supported using the ILogger interface methods
- Info/Warning and Error messages
- Method tracking with stack display showing execution times
- Screenshot capture
- Conditional logging
- Checkpoints
- Counters
- Watches + value history support
- Datasets
- Bitmaps, and various other image formats
- Objects
- Components
- Native value types with type information
- Text with suppport for multiple highlighters
- Actions
- Up to 32 logging levels

## Libraries used
The Object Pascal (Delphi) sources depend on the following open source libraries and components:
  * [Spring4D](http://bitbucket.org/sglienke/spring4d)
  * [DSharp](http://bitbucket.org/sglienke/dsharp)
  * [Virtual treeview](http://github.com/Virtual-TreeView/Virtual-TreeView)
  * [DDuce](http://github.com/beNative/dduce)
  * [ZeroMQ](http://github.com/beNative/Delphi-ZeroMQ)
  * [zcontrols](http://github.com/beNative/zcontrols)
  * [TBCEditor](http://github.com/beNative/TBCEditor)
  * [TChromeTabs](http://github.com/norgepaul/TChromeTabs)
  * [Ararat Synapse](http://github.com/beNative/synapse)
  * [OMultiPanel](http://github.com/beNative/omultipanel)
  * [KControls](http://github.com/beNative/kcontrols)
  * [JsonDataObjects](http://github.com/ahausladen/JsonDataObjects)
