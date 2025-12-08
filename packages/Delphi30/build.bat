@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild "C:\Attracs\BoldForDelphi\packages\Delphi30\dclBold.dproj" /p:Config=Debug /p:Platform=Win32 /v:minimal
