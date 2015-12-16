#! /bin/sh
#  ------------------------------------------------------------
#  Company: Software- und Organisations-Service GmbH
#  Purpose: "kill script" for JobScheduler Agent
#  ------------------------------------------------------------

KILL_TASK_LOG_FILE="/dev/null"
KILL_TASK_EXIT=0
PIDS_TREE=

killtree() {
    for _pid in $PIDS_TREE
    do
      echo "killing pid ${_pid}..." >> ${KILL_TASK_LOG_FILE}
      kill -9 ${_pid}
      kill_err=$?
      if [ $kill_err -ne 0 ]
      then
        echo "...kill pid ${_pid} failed!" >> ${KILL_TASK_LOG_FILE}
        KILL_TASK_EXIT=$kill_err
      fi
    done
}

stoptree() {
    _pid="$1"
    PIDS_TREE="${_pid} ${PIDS_TREE}"
    echo "stopping pid ${_pid}..." >> ${KILL_TASK_LOG_FILE}
    kill -stop ${_pid} # needed to stop quickly forking parent from producing children between child killing and parent killing
    _regex="^\s*[0-9]+\s+${_pid}\s*$"
    for _child in `ps ax -o "pid= ppid=" | egrep "${_regex}" | awk '{print $1}'`; do
        stoptree "${_child}"
    done
}

for param in "$@"
do
  case "$param" in
         -kill-agent-task-id=*)     KILL_TASK_ID=`echo "$param" | sed -e 's/-kill-agent-task-id=//'`
                            ;;
  esac
done

if [ -z "${KILL_TASK_ID}" ]
then
  echo "option -kill-agent-task-id is not set" >> ${KILL_TASK_LOG_FILE}
  exit 2
fi

KILL_TASK_PID=`ps -ef -w -w | grep " -agent-task-id=${KILL_TASK_ID}" | grep -v "grep" | awk '{print $2}'`

if [ -z "${KILL_TASK_PID}" ]
then
  echo "process with -agent-task-id=${KILL_TASK_PID} doesn't exist." >> ${KILL_TASK_LOG_FILE}
  exit 0
fi

echo "killing task with pid ${KILL_TASK_PID}" >> ${KILL_TASK_LOG_FILE}

stoptree ${KILL_TASK_PID}
killtree

exit $KILL_TASK_EXIT
