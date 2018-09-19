# LogViewer

A general purpose logviewer that supports multiple source types to receive
messages from.

* Windows IPC (using WM_COPY messages)
* Windows OutputDebugString API
* [ZeroMQ](https://github.com/zeromq/libzmq) (PUB/SUB socket)
* Serial port
* Spring4D logging API

Each of these receivers supports multiple channels which can be displayed in
a dedicated logviewer.

A logviewer consists of
- the main log treeview
- watches with history
- method callstack level display
- message details

Features currently supported:
- Info/Warning and Error messages
- Method tracking
- Screenshot capture
- Conditional logging
- Checkpoints
- Counters
- Watches
- Datasets
- Actions

The sources depend on the following libraries and components:
  * [Spring4D](http://bitbucket.org/sglienke/spring4d)
  * [DSharp](http://bitbucket.org/sglienke/dsharp)
  * [Virtual treeview](http://github.com/Virtual-TreeView/Virtual-TreeView)
  * [DDuce](http://github.com/beNative/dduce)
  * [ZeroMQ](http://github.com/beNative/Delphi-ZeroMQ)
  * [zcontrols](http://github.com/beNative/zcontrols)
  * [TBCEditor](https://github.com/beNative/TBCEditor)
  * [TChromeTabs](https://github.com/norgepaul/TChromeTabs)
  * [Ararat Synapse](https://sourceforge.net/projects/synalist/)

Here is a preliminary look of the application:

![LogViewer](https://github.com/beNative/LogViewer/blob/master/Wiki/LogViewer_30-11-2017%2021-35-21.png)

I'm working towards the first stable release now.
