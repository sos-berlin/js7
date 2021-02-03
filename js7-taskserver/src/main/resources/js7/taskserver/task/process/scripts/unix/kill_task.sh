#! /bin/sh
#  ------------------------------------------------------------
#  Company: Software- und Organisations-Service GmbH
#  Purpose: "kill script" for JS7 Agent
#  ------------------------------------------------------------

set -e

log() {
    echo "[$1]  $2" 1>&2
}

AGENT_TASK_ID=""
CONTROLLER_TASK_ID=""
JOB_PATH=""
PID=""

for arg in "$@"; do
    case "$arg" in
        --kill-agent-task-id=*)
            AGENT_TASK_ID=`echo "$arg" | sed 's/--kill-agent-task-id=//'`
            ;;
        --controller-task-id=*)
            CONTROLLER_TASK_ID=`echo "$arg" | sed 's/--controller-task-id=//'`
             ;;
        --job=*)
            JOB_PATH=`echo "$arg" | sed 's/--job=//'`
            ;;
        --pid=*)
            PID=`echo "$arg" | sed 's/--pid=//'`
            ;;
    esac
done

if [ -z "$AGENT_TASK_ID$PID" ]; then
    log error "Option --kill-agent-task-id is not set"
    exit 2
fi
if [ ! -z "$JOB_PATH" ]; then
    log info "Task '$CONTROLLER_TASK_ID' of Job '$JOB_PATH' with Agent task id '$AGENT_TASK_ID' is being killed"
else
    log info "Task with Agent task id '$AGENT_TASK_ID' is being killed"
fi

TASK_PID=`ps ww | grep " --agent-task-id[=]$AGENT_TASK_ID\\b" | awk '{ print $1 }'`
[ ! -z "$TASK_PID" ] || TASK_PID="$PID"
if [ -z "$TASK_PID" ]; then
    log info "Process with --agent-task-id=$AGENT_TASK_ID doesn't exist"
    exit 0
fi

log info "Killing task with PID $TASK_PID and its children"
descendants=

if [ "`uname`" = "SunOS" ]; then
    psTree="ps -ef -o pid,ppid"
elif [ "`uname`" = "Darwin" ]; then
    psTree="ps -e -o pid,ppid"
else
    psTree="ps ax -o pid,ppid"
fi

collectAndStopAllPids() {
    # First stop all processes to inhibit quickly forking parent from producing children between child killing and parent killing
    # $1: Parent PID
    # $2: Indentation
    log info "$2 kill -STOP $1"
    kill -STOP $1 || true
    for _child in `$psTree | egrep " $1\$" | awk '{ print $1 }'`; do
        descendants="$_child $descendants"
        collectAndStopAllPids "$_child" "| $2"
    done
}

collectAndStopAllPids "$TASK_PID"

exitCode=0
log info "kill -KILL $TASK_PID  (the task process)"
kill -KILL $TASK_PID || exitCode=1

for pid in $descendants; do
    log info "kill -KILL $pid"
    kill -KILL $pid 2>/dev/null || true
done

exit $exitCode
