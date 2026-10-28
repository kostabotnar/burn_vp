@echo off
setlocal EnableExtensions EnableDelayedExpansion
rem Double-click launcher for scripts\setup_environment.R (Windows only).
rem Keep this file in the unzipped project folder.
rem Testing options (not for users): BURN_VP_NO_PAUSE=1 skips the final pause,
rem and "setup.bat --find-r" only prints the Rscript.exe it would use.

set "ROOT=%~dp0"
if "%ROOT:~-1%"=="\" set "ROOT=%ROOT:~0,-1%"
set "FINDONLY="
if /i "%~1"=="--find-r" set "FINDONLY=1"

rem Confirm this is the project folder before anything is written.
set "MARKED="
if exist "%ROOT%\PROJECT_VERSION" (
  findstr /r /c:"^Project: burn_vp *$" "%ROOT%\PROJECT_VERSION" >nul 2>&1 && set "MARKED=1"
)
if not defined MARKED (
  echo This file must stay inside the unzipped project folder, next to the file
  echo PROJECT_VERSION. Please do not move or copy setup.bat on its own.
  echo Nothing was changed.
  set "CODE=1"
  goto :finish
)

rem Look for R 4.5.x: registry first, then the usual install folders.
rem The highest patch number wins. Other R versions are ignored.
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

if not defined RSCRIPT (
  echo R 4.5 was not found on this computer.
  echo Please install R 4.5.2 from
  echo https://cran.r-project.org/bin/windows/base/old/4.5.2/
  echo Another R version will not work with this tool.
  set "CODE=1"
  goto :finish
)

if defined FINDONLY (
  echo !RSCRIPT!
  set "CODE=0"
  goto :finish
)

echo Using R: !RSCRIPT!

rem Keep R's temporary files inside the project (R on Windows reads TMPDIR, then TMP, then TEMP).
if not exist "%ROOT%\output\tmp" mkdir "%ROOT%\output\tmp"
if not exist "%ROOT%\output\tmp" (
  echo Could not create the folder output\tmp in the project folder.
  set "CODE=1"
  goto :finish
)
set "TMPDIR=%ROOT%\output\tmp"
set "TMP=%ROOT%\output\tmp"
set "TEMP=%ROOT%\output\tmp"

echo Starting setup. The first run can take several minutes. Please wait.
echo.
rem --no-init-file keeps .Rprofile from starting renv before setup is ready for it.
"!RSCRIPT!" --no-init-file "%ROOT%\scripts\setup_environment.R"
set "CODE=!ERRORLEVEL!"
echo.
if "!CODE!"=="0" (
  echo Setup has finished. You can close this window.
) else (
  echo Setup did not finish. Please send the log file in the folder output\setup.
)
goto :finish

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

:finish
if not defined BURN_VP_NO_PAUSE pause
exit /b !CODE!
