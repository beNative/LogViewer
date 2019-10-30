# LogViewer

LogViewer is a general purpose message viewer for log messages with support for multiple sources to receive messages from.

* Windows ``OutputDebugString`` API
* Windows IPC (using ``WM_COPY`` messages)
* [ZeroMQ](https://github.com/zeromq/libzmq) (`PUB`/`SUB` socket) for logging over the network. It demonstrates the brilliant performance of ZMQ sockets.
* Serial port
* MQTT
* File change monitor

Each of these receivers support multiple subscribers for which messages can be displayed in
a dedicated logviewer.

The [DDuce](http://github.com/beNative/dduce) library provides the Logger module to add logging to your application (currently only Delphi support, but FPC-Lazarus support will be added soon).
The DDuce demo application demonstrates the currently supported message types.

![Logger demo](https://github.com/beNative/LogViewer/blob/master/Wiki/DDuce%20Logger24-10-2018%2022-12-44.png)

## General layout
- the main log treeview
- watches with history 
- method callstack level display
- message details

## Features currently supported using the ILogger interface methods
- Info/Warning and Error messages
- Method tracking with stack display showing execution times
- Screenshot capture
- Conditional logging
- Checkpoints
- Counters
- Watches + value history support
- Datasets
- Bitmaps
- Objects
- Components
- Native value types with type information
- Text with suppport for multiple highlighters
- Actions
- Up to 32 logging levels

## Overview
A mindmap with a technical overview of the application: [LogViewer.xmind](https://github.com/beNative/LogViewer/blob/master/Documents/LogViewer.xmind)

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
  * [midiio](http://github.com/beNative/midiio)

## Screenshots
Here is a preliminary look of the application (2018/09/22):

![LogViewer](https://github.com/beNative/LogViewer/blob/master/Wiki/LogViewer_22-09-2018%2013-21-07.png)

