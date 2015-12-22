@echo off
rem #  ------------------------------------------------------------
rem #  Company: Software- und Organisations-Service GmbH
rem #  Purpose: "kill script" for JobScheduler Agent
rem #  ------------------------------------------------------------

SETLOCAL

set KILL_TASK_PID=
set KILL_TASK_LOG_FILE=nul
goto read_command_line_options_begin

:read_command_line_options
shift
:read_command_line_options_begin
set OPTION_VALUE=%~2
if "%~1" == "" goto read_command_line_options_end

if "%~1" == "-kill-agent-task-id" (
  if "%OPTION_VALUE%" == "" goto read_command_line_options
  if "%OPTION_VALUE:~0,1" == "-" goto read_command_line_options
  set KILL_TASK_ID=%~2
)

goto read_command_line_options
:read_command_line_options_end

if not defined KILL_TASK_ID (
  echo option -kill-agent-task-id is not set >> %KILL_TASK_LOG_FILE%
  exit /b 2
)

for /f "usebackq tokens=2 delims==" %%i in (`wmic process where "commandline like '%% -agent-task-id=%KILL_TASK_ID%%%' and not commandline like '%%-kill-agent-task-id%%'" get processid /format:value 2^>nul`) do set KILL_TASK_PID=%%i

if x%KILL_TASK_PID% == x (
  echo process with -agent-task-id=%KILL_TASK_ID% doesn't exist. >> %KILL_TASK_LOG_FILE%
  exit /b 0
)

echo killing task with pid %KILL_TASK_PID% >> %KILL_TASK_LOG_FILE%
taskkill /PID %KILL_TASK_PID% /F /T

if ERRORLEVEL 1 (
  echo ...kill task failed! >> %KILL_TASK_LOG_FILE%
)

exit /b %ERRORLEVEL%
ENDLOCAL
