@echo off
rem #  ------------------------------------------------------------
rem #  Company: Software- und Organisations-Service GmbH
rem #  Purpose: "kill script" for JobScheduler Agent
rem #  ------------------------------------------------------------

SETLOCAL

set KILL_TASK_PID=
set KILL_TASK_ID=
set MASTER_TASK_ID=
set JOB_NAME=
if not defined KILL_TASK_LOG_FILE set KILL_TASK_LOG_FILE=nul
if not x%KILL_TASK_LOG_FILE%==xnul set KILL_TASK_LOG_FILE="%KILL_TASK_LOG_FILE%"
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

if "%~1" == "-kill-master-task-id" (
  if "%OPTION_VALUE%" == "" goto read_command_line_options
  if "%OPTION_VALUE:~0,1" == "-" goto read_command_line_options
  set MASTER_TASK_ID=%~2
)

if "%~1" == "-job-name" (
  if "%OPTION_VALUE%" == "" goto read_command_line_options
  if "%OPTION_VALUE:~0,1" == "-" goto read_command_line_options
  set JOB_NAME=%~2
)

goto read_command_line_options
:read_command_line_options_end 

if not defined KILL_TASK_ID (
  echo %DATE% %TIME% [error] option -kill-agent-task-id is not set. >> %KILL_TASK_LOG_FILE%
  exit /b 2
)

if defined JOB_NAME echo %DATE% %TIME% [info]  Task "%MASTER_TASK_ID%" of Job "%JOB_NAME%" with Agent task id "%KILL_TASK_ID%" will be killed. >> %KILL_TASK_LOG_FILE%
if not defined JOB_NAME echo %DATE% %TIME% [info]  Task with Agent task id "%KILL_TASK_ID%" will be killed. >> %KILL_TASK_LOG_FILE%

for /f "usebackq tokens=2 delims==" %%i in (`wmic process where "commandline like '%% -agent-task-id=%KILL_TASK_ID%%%' and not commandline like '%%-kill-agent-task-id%%'" get processid /format:value 2^>nul`) do set KILL_TASK_PID=%%i

if x%KILL_TASK_PID% == x (
  echo %DATE% %TIME% [info]  process with -agent-task-id=%KILL_TASK_ID% doesn't exist. >> %KILL_TASK_LOG_FILE%
  exit /b 0
)
  
echo %DATE% %TIME% [info]  Killing task with pid %KILL_TASK_PID% and its children >> %KILL_TASK_LOG_FILE%
taskkill /PID %KILL_TASK_PID% /F /T

if ERRORLEVEL 1 (
  echo %DATE% %TIME% [error] ...kill task failed! >> %KILL_TASK_LOG_FILE%
)

exit /b %ERRORLEVEL%
ENDLOCAL