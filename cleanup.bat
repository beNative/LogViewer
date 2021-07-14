@echo off
for /d /r %%f in (__history;__recovery;*.log) do rmdir /s /q %%f
for /r %%f in (*.dcu;*.dsk;*.cbk;*.vlb;*.dsm;*.rsm;*.stat;*.identcache;*.dproj.local;*.drc;*.map;*.o;*.log;*.skincfg;*.tvsconfig) do del /f /q %%f
