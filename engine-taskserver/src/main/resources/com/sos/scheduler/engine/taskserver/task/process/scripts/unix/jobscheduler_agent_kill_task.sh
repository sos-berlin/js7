#! /bin/sh
#  ------------------------------------------------------------
#  Company: Software- und Organisations-Service GmbH
#  Purpose: "kill script" for JobScheduler Agent
#  ------------------------------------------------------------

set -e

log() {
    echo "[$1]  $2" 1>&2
    test -z "$KILL_TASK_LOG_FILE" || echo "`date '+%Y-%m-%d %T,%3N %z'` [$1]  $2" >> "$KILL_TASK_LOG_FILE"
}

psTreeSolaris() {
    ps -ef -o pid,ppid
}

psTree() {
    ps ax -o "pid= ppid="
}

if [ "$(uname)" = "SunOS" ]; then
    psTree=psTreeSolaris
else
    psTree=psTree
fi

collectAndStopAllPids() {
    # First stop all processes to inhibit quickly forking parent from producing children between child killing and parent killing
    # $1: Parent PID
    # $2: Identation
    ALL_PIDS="$1 $ALL_PIDS"
    log info "$2 kill -STOP $1 "
    kill -STOP $1 || true
    for _child in `$psTree | egrep " $1\$" | awk '{ print $1 }'`; do
        collectAndStopAllPids "$_child" "| $2"
    done
}

PID=""
for arg in "$@"; do
    case "$arg" in
        -kill-agent-task-id=*)
            AGENT_TASK_ID=`echo "$arg" | sed 's/-kill-agent-task-id=//'`
            ;;
        -master-task-id=*)
            MASTER_TASK_ID=`echo "$arg" | sed 's/-master-task-id=//'`
             ;;
        -job-name=*)
            JOB_NAME=`echo "$arg" | sed 's/-job-name=//'`
            ;;
        -pid=*)
            PID=$(echo "$arg" | sed 's/-pid=//')
            ;;
    esac
done

if [ -z "$AGENT_TASK_ID$PID" ]; then
    log error "Option -kill-agent-task-id is not set"
    exit 2
fi
if [ ! -z "$JOB_NAME" ]; then
    log info "Task '$MASTER_TASK_ID' of Job '$JOB_NAME' with Agent task id '$AGENT_TASK_ID' will be killed"
else
    log info "Task with Agent task id '$AGENT_TASK_ID' will be killed"
fi

TASK_PID=`ps ww | grep " -agent-task-id[=]$AGENT_TASK_ID" | awk '{ print $1 }'`
[ ! -z "$TASK_PID" ] || TASK_PID="$PID"
if [ -z "$TASK_PID" ]; then
    log info "Process with -agent-task-id=$AGENT_TASK_ID doesn't exist"
    exit 0
fi

log info "Killing task with PID $TASK_PID and its children"
ALL_PIDS=
collectAndStopAllPids "$TASK_PID"

exitCode=0
for _pid in $ALL_PIDS; do
    log info "kill -KILL $_pid"
    kill -KILL $_pid || {
        log error "...kill PID $_pid failed!"
        exitCode=1
    }
done
exit $exitCode
