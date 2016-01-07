#! /bin/sh
#  ------------------------------------------------------------
#  Company: Software- und Organisations-Service GmbH
#  Purpose: "kill script" for JobScheduler Agent
#  ------------------------------------------------------------

test -z "${KILL_TASK_LOG_FILE}" && KILL_TASK_LOG_FILE="/dev/null"
KILL_TASK_EXIT=0
PIDS_TREE=

killtree() {
    for _pid in $PIDS_TREE
    do
      date "+%Y-%m-%d %T,%3N %z [info]  killing pid ${_pid}..." >> "${KILL_TASK_LOG_FILE}"
      kill -9 ${_pid}
      kill_err=$?
      if [ $kill_err -ne 0 ]
      then
        date "+%Y-%m-%d %T,%3N %z [error] ...kill pid ${_pid} failed!" >> "${KILL_TASK_LOG_FILE}"
        KILL_TASK_EXIT=$kill_err
      fi
    done
}

stoptree() {
    _pid="$1"
    PIDS_TREE="${_pid} ${PIDS_TREE}"
    date "+%Y-%m-%d %T,%3N %z [info]  stopping pid ${_pid}..." >> "${KILL_TASK_LOG_FILE}"
    kill -stop ${_pid} # needed to stop quickly forking parent from producing children between child killing and parent killing
    for _child in `ps ax -o "pid= ppid=" | egrep " ${_pid}$" | awk '{print $1}'`; do
        stoptree "${_child}" 
    done
}

for param in "$@"
do
  case "$param" in
         -kill-agent-task-id=*)     KILL_TASK_ID=`echo "$param" | sed 's/-kill-agent-task-id=//'`
                            ;;
  case "$param" in
         -kill-master-task-id=*)    MASTER_TASK_ID=`echo "$param" | sed 's/-kill-master-task-id=//'`
                            ;;
  case "$param" in
         -job-name=*)               JOB_NAME=`echo "$param" | sed 's/-job-name=//'`
                            ;;
  esac
done

if [ -z "${KILL_TASK_ID}" ]
then
  date "+%Y-%m-%d %T,%3N %z [error] option -kill-agent-task-id is not set" >> "${KILL_TASK_LOG_FILE}"
  exit 2
fi

if [ ! -z "${JOB_NAME}" ]
then
  date "+%Y-%m-%d %T,%3N %z [info]  Task \"${MASTER_TASK_ID}\" of Job \"${JOB_NAME}\" with Agent task id \"${KILL_TASK_ID}\" will be killed." >> "${KILL_TASK_LOG_FILE}"
else
  date "+%Y-%m-%d %T,%3N %z [info]  Task with Agent task id \"${KILL_TASK_ID}\" will be killed." >> "${KILL_TASK_LOG_FILE}"
fi

KILL_TASK_PID=`ps -ef -w -w | grep " -agent-task-id=${KILL_TASK_ID}" | grep -v "grep" | awk '{print $2}'`

if [ -z "${KILL_TASK_PID}" ]
then
  date "+%Y-%m-%d %T,%3N %z [info]  process with -agent-task-id=${KILL_TASK_ID} doesn't exist." >> "${KILL_TASK_LOG_FILE}"
  exit 0
fi

date "+%Y-%m-%d %T,%3N %z [info]  Killing task with pid ${KILL_TASK_PID} and its children" >> "${KILL_TASK_LOG_FILE}"

stoptree ${KILL_TASK_PID}
killtree

exit $KILL_TASK_EXIT 
