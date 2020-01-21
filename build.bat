@echo off
set %project%=LogViewer.dproj
set rsvars="c:\Program Files (x86)\Embarcadero\Studio\20.0\bin\rsvars.bat"
call %rsvars%
msbuild %project% /t:make /p:config=Debug /p:platform=Win32
msbuild %project% /t:make /p:config=Release /p:platform=Win32
msbuild %project% /t:make /p:config=Debug /p:platform=Win64
msbuild %project% /t:make /p:config=Release /p:platform=Win64
pause
