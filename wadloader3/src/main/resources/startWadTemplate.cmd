@echo off

if "%GZDOOM_PATH%"=="" set /p "GZDOOM_PATH=Enter the path to gzdoom.exe: "
if "%IWAD_PATH%"=="" set /p "IWAD_PATH=Enter the path to the iwad you want to use: "

start /b %GZDOOM_PATH% -iwad %IWAD_PATH% -file%FILES%

echo "%GZDOOM_PATH%"
