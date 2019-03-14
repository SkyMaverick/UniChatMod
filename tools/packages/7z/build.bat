@echo off

SET pack_path=%1
SET app_path=%~dp0%2
SET arc="7z.exe"

REM remove all trash files
REM for /d %%i in (%app_path%, %app_path%\*) do start /b cmd /c (del "%%i\*.lib")

REM pack with 7-zip archivator
7z a -y -t7z -stl -ssw -mx5 -r0 -sfx %pack_path%\%2.exe %app_path%
7z a -y -tzip -stl -ssw -mx5 -r0 %pack_path%\%2.zip %app_path%
