@echo off

REM check admin privileges
REM if not admin, restart elevated
set admin=0
whoami /groups | find "12288" >NUL && set admin=1
if %admin% EQU 0 powershell -Command "Start-Process -Verb RunAs %0
if %admin% EQU 0 exit

set "HANDLER_PATH=%APPDATA%\wadloader3\handler"
mkdir %HANDLER_PATH%

REM TODO replace %HANDLER_DOWNLOAD_URL% before offering this file to download
set "HANDLER_DOWNLOAD_URL=http://localhost:8080"

REM download the handler executable
REM replace handler.bat with haskell exe
curl -o "%HANDLER_PATH%\local-client.exe" "%HANDLER_DOWNLOAD_URL%/download/handler"

REM registers the handler as programm to handle the "wadloader" protocol
reg add HKEY_CLASSES_ROOT\wadloader /t REG_SZ /d "Wadloader 3 local-handler" /f
reg add HKEY_CLASSES_ROOT\wadloader /v "URL Protocol" /t REG_SZ /d "" /f
reg add HKEY_CLASSES_ROOT\wadloader\shell /f
reg add HKEY_CLASSES_ROOT\wadloader\shell\open /f
reg add HKEY_CLASSES_ROOT\wadloader\shell\open\command /t REG_SZ /d "%HANDLER_PATH%\local-client.exe %%1" /f
pause
