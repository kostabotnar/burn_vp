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
  for /f "usebackq eol=# delims=" %%L in ("%ROOT%\PROJECT_VERSION") do if "%%L"=="Project: burn_vp" set "MARKED=1"
)
if not defined MARKED (
  echo This file must stay inside the unzipped project folder, next to the file
  echo PROJECT_VERSION. Please do not move or copy setup.bat on its own.
  echo Nothing was changed.
  set "CODE=1"
  goto :finish
)

rem Look for R 4.5.x with the shared finder (also used by burn_vp.bat). It sets RSCRIPT
rem to the highest patch version found, or leaves it undefined.
if not exist "%ROOT%\scripts\find_r45.bat" (
  echo The project folder is incomplete: the file scripts\find_r45.bat is missing.
  echo Please contact the project team.
  echo Nothing was changed.
  set "CODE=1"
  goto :finish
)
call "%ROOT%\scripts\find_r45.bat"

if not defined RSCRIPT (
  echo R 4.5 was not found on this computer.
  echo Setup cannot continue. Please contact the project team.
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
rem Only what this run creates is removed again at the end (see :cleanup).
set "OUTCREATED="
set "TMPCREATED="
if not exist "%ROOT%\output" set "OUTCREATED=1"
if not exist "%ROOT%\output\tmp" set "TMPCREATED=1"
if not exist "%ROOT%\output\tmp" mkdir "%ROOT%\output\tmp"
if not exist "%ROOT%\output\tmp" (
  echo Could not create the folder output\tmp in the project folder.
  set "CODE=1"
  goto :finish
)
rem R stops with a fatal error when its temporary folder path has a space, so the short
rem (8.3) form of the path is used. If Windows has no short form, the path cannot be used.
for %%I in ("%ROOT%\output\tmp") do set "TMPSHORT=%%~sI"
if not "!TMPSHORT: =!"=="!TMPSHORT!" (
  echo The project folder path contains a space, and R cannot keep its temporary
  echo files there. Please move the whole project folder to a short path without
  echo spaces, for example C:\burn_vp, and double-click setup.bat again.
  set "CODE=1"
  call :cleanup
  goto :finish
)
set "TMPDIR=!TMPSHORT!"
set "TMP=!TMPSHORT!"
set "TEMP=!TMPSHORT!"

echo Starting setup. The first run can take several minutes. Please wait.
echo.
rem --no-init-file keeps .Rprofile from starting renv before setup is ready for it.
"!RSCRIPT!" --no-init-file "%ROOT%\scripts\setup_environment.R"
set "CODE=!ERRORLEVEL!"
call :cleanup
echo.
if "!CODE!"=="0" (
  echo Setup has finished. You can close this window.
) else (
  echo Setup did not finish. Please send the log file in the folder output\setup.
)
goto :finish

:cleanup
rem Runs only after the PROJECT_VERSION check passed. R has exited and removed its own temporary
rem folder inside output\tmp. Only this exact folder is removed, and output itself only when
rem this run created it and nothing else is in it.
if defined TMPCREATED if exist "%ROOT%\output\tmp" rd /s /q "%ROOT%\output\tmp"
if exist "%ROOT%\output\tmp" echo Note: the folder output\tmp could not be removed. You can delete it.
if defined OUTCREATED rd "%ROOT%\output" 2>nul
goto :eof

:finish
if not defined BURN_VP_NO_PAUSE pause
exit /b !CODE!
