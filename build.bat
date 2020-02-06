@echo off
set project=LogViewer
where rsvars.bat /q
if %errorlevel% neq 0 (
  echo Add the Embarcadero Studio bin folder to your system path.
  echo e.g. "c:\Program Files (x86)\Embarcadero\Studio\19.0\bin"   
) else (
  call rsvars
  msbuild %project%.dproj /t:make /p:config=Debug /p:platform=Win32
  msbuild %project%.dproj /t:make /p:config=Release /p:platform=Win32
  msbuild %project%.dproj /t:make /p:config=Debug /p:platform=Win64
  msbuild %project%.dproj /t:make /p:config=Release /p:platform=Win64
  where upx.exe /q
  if %errorlevel% neq 0 (  
    echo upx not found
  ) else (
    upx .\Bin\Win32\%project%.exe
  )               
)
pause
