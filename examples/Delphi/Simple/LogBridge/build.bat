@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
cd /d %~dp0
msbuild LogBridgeDemo.dproj /p:Config=Debug /p:Platform=Win32 /v:minimal
