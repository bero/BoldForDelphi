@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
cd /d C:\Attracs\BoldForDelphi\examples\Delphi\Simple\ObjectSpace\Query
msbuild QueryDemoProject.dproj /p:Config=Debug /p:Platform=Win32 /t:Build /v:minimal
