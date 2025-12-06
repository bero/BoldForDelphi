@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild "C:\Attracs\BoldForDelphi\packages\Delphi29.3\dclBold.dproj" /p:Config=Debug /p:Platform=Win32 /v:minimal
