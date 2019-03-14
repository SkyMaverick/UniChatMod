@echo off

SET pack_path=%1
SET app_path=%~dp0%2

SET arc="7z.exe"

if defined ARCHID (
    SET cpu_arch=%ARCHID%
) else (
    SET cpu_arch=%PROCESSOR_ARCHITECTURE%
)


REM pack with 7-zip archivator
7z a -y -t7z -stl -ssw -mx5 -r0 -sfx %pack_path%\%2_win_%cpu_arch%.exe %app_path%
7z a -y -tzip -stl -ssw -mx5 -r0 %pack_path%\%2_win_%cpu_arch%.zip %app_path%
