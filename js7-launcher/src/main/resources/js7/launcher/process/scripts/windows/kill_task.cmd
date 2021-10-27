@echo off
rem #  ------------------------------------------------------------
rem #  Company: Software- und Organisations-Service GmbH
rem #  Purpose: "kill script" for JS7 Agent
rem #  ------------------------------------------------------------

SETLOCAL

set KILL_TASK_PID=
set KILL_TASK_ID=
set CONTROLLER_TASK_ID=
set JOB_PATH=
goto read_command_line_options_begin

:read_command_line_options
shift
:read_command_line_options_begin
set OPTION_VALUE=%~2
if "%~1" == "" goto read_command_line_options_end

if "%~1" == "--kill-agent-task-id" (
  if "%OPTION_VALUE%" == "" goto read_command_line_options
  if "%OPTION_VALUE:~0,1" == "-" goto read_command_line_options
  set KILL_TASK_ID=%~2
)

if "%~1" == "--controller-task-id" (
  if "%OPTION_VALUE%" == "" goto read_command_line_options
  if "%OPTION_VALUE:~0,1" == "-" goto read_command_line_options
  set CONTROLLER_TASK_ID=%~2
)

if "%~1" == "--job" (
  if "%OPTION_VALUE%" == "" goto read_command_line_options
  if "%OPTION_VALUE:~0,1" == "-" goto read_command_line_options
  set JOB_PATH=%~2
)

goto read_command_line_options
:read_command_line_options_end

if not defined KILL_TASK_ID (
  echo %DATE% %TIME% [error] option -kill-agent-task-id is not set. 1>&2
  exit /b 2
)

if defined JOB_PATH echo %DATE% %TIME% [info]  Task "%CONTROLLER_TASK_ID%" of Job "%JOB_PATH%" with Agent task id "%KILL_TASK_ID%" will be killed. 1>&2
if not defined JOB_PATH echo %DATE% %TIME% [info]  Task with Agent task id "%KILL_TASK_ID%" will be killed. 1>&2

for /f "usebackq tokens=2 delims==" %%i in (`wmic process where "commandline like '%% --agent-task-id=%KILL_TASK_ID%%%' and not commandline like '%%-kill-agent-task-id%%'" get processid /format:value 2^>nul`) do set KILL_TASK_PID=%%i

if x%KILL_TASK_PID% == x (
  echo %DATE% %TIME% [info]  process with --agent-task-id=%KILL_TASK_ID% doesn't exist. 1>&2
  exit /b 0
)

echo %DATE% %TIME% [info]  Killing task with pid %KILL_TASK_PID% and its children 1>&2
taskkill /PID %KILL_TASK_PID% /F /T 1>&2

if ERRORLEVEL 1 (
  echo %DATE% %TIME% [error] ...kill task failed! 1>&2
)

exit /b %ERRORLEVEL%
ENDLOCAL
