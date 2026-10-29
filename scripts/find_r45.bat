@echo off
setlocal EnableExtensions EnableDelayedExpansion
rem Shared by setup.bat and burn_vp.bat: finds the newest installed R 4.5.x.
rem Use it as: call "%ROOT%\scripts\find_r45.bat"
rem Afterwards RSCRIPT holds the full path of its bin\Rscript.exe, or is undefined if none was
rem found. Nothing is printed or written, and no other variable is changed in the caller.

rem Registry first, then the usual install folders. The highest patch number wins.
rem Other R versions are ignored.
set "RSCRIPT="
set "BESTPATCH=-1"
for %%H in (HKLM HKCU) do (
  set "KEY="
  for /f "usebackq tokens=1,2,*" %%a in (`reg query "%%H\SOFTWARE\R-core\R" /s /v InstallPath 2^>nul`) do (
    set "LINE=%%a"
    if /i "!LINE:~0,4!"=="HKEY" (
      set "KEY=%%a"
    ) else if /i "%%a"=="InstallPath" (
      if defined KEY for %%k in ("!KEY!") do call :consider "%%~nxk" "%%c"
    )
  )
)
for /d %%d in ("%ProgramFiles%\R\R-4.5.*") do call :consider "%%~nxd" "%%~fd"
for /d %%d in ("%LOCALAPPDATA%\Programs\R\R-4.5.*") do call :consider "%%~nxd" "%%~fd"

rem Only RSCRIPT leaves the local scope: %RSCRIPT% is expanded before endlocal runs.
rem If it is empty, this clears RSCRIPT in the caller too.
endlocal & set "RSCRIPT=%RSCRIPT%"
exit /b 0

:consider
rem %1 = version text such as 4.5.2 or R-4.5.2, %2 = R install folder.
set "V=%~1"
if /i "!V:~0,2!"=="R-" set "V=!V:~2!"
if not "!V:~0,4!"=="4.5." goto :eof
set "P=!V:~4!"
set /a "N=!P!" 2>nul
if not "!N!"=="!P!" goto :eof
if not exist "%~2\bin\Rscript.exe" goto :eof
if !N! GTR !BESTPATCH! (
  set "BESTPATCH=!N!"
  set "RSCRIPT=%~2\bin\Rscript.exe"
)
goto :eof
