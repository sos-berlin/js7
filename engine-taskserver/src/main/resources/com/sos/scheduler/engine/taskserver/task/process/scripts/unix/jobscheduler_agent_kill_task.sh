#! /bin/sh
#  ------------------------------------------------------------
#  Company: Software- und Organisations-Service GmbH
#  Purpose: "kill script" for JobScheduler Agent
#  ------------------------------------------------------------

KILL_TASK_EXIT=0
PIDS_TREE=

log() {
    echo "[$1]  $2" 1>&2
    test -z "$KILL_TASK_LOG_FILE" || echo "`date '+%Y-%m-%d %T,%3N %z'` [$1]  $2" >> "$KILL_TASK_LOG_FILE"
}

killtree() {
    for _pid in $PIDS_TREE; do
        log info "kill -KILL $_pid"
        kill -KILL $_pid
        kill_err=$?
        if [ $kill_err -ne 0 ]; then
            log error "...kill pid $_pid failed!"
            KILL_TASK_EXIT=$kill_err
        fi
    done
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

stoptree() {
    # First stop all processes to inhibit quickly forking parent from producing children between child killing and parent killing
    _pid="$1"
    PIDS_TREE="$_pid $PIDS_TREE"
    log info "kill -STOP $_pid "
    kill -STOP $_pid
    for _child in `$psTree | egrep " $_pid$" | awk '{print $1}'`; do
        stoptree "$_child"
    done
}

PID=""
for param in "$@"; do
    case "$param" in
        -kill-agent-task-id=*)
            KILL_TASK_ID=`echo "$param" | sed 's/-kill-agent-task-id=//'`
            ;;
         -kill-master-task-id=*)
             MASTER_TASK_ID=`echo "$param" | sed 's/-kill-master-task-id=//'`
             ;;
         -job-name=*)
            JOB_NAME=`echo "$param" | sed 's/-job-name=//'`
            ;;
         -pid=*)
            PID=$(echo "$param" | sed 's/-pid=//')
            ;;
    esac
done

if [ -z "$KILL_TASK_ID$PID" ]; then
    log error "Option -kill-agent-task-id is not set"
    exit 2
fi

if [ ! -z "$JOB_NAME" ]; then
    log info "Task '$MASTER_TASK_ID' of Job '$JOB_NAME' with Agent task id '$KILL_TASK_ID' will be killed"
else
    log info "Task with Agent task id '$KILL_TASK_ID' will be killed"
fi

KILL_TASK_PID=`ps ww | grep " -agent-task-id=$KILL_TASK_ID" | grep -v "grep" | awk '{ print $1 }'`

[ ! -z "$KILL_TASK_PID" ] || KILL_TASK_PID="$PID"

if [ -z "$KILL_TASK_PID" ]; then
    log info "Process with -agent-task-id=$KILL_TASK_ID doesn't exist"
    exit 0
fi

log info "Killing task with pid $KILL_TASK_PID and its children"
stoptree "$KILL_TASK_PID"
killtree

exit $KILL_TASK_EXIT
