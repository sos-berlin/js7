#! /bin/bash
set -e

declare -a javaOptions
declare -a agentOptions
declare SCHEDULER_AGENT_DATA

for arg in "$@"; do
    case $arg in
        -rmx-port=*)
            port="${arg#*=}"
            javaOptions=(
                "-Dcom.sun.management.jmxremote"
                "-Dcom.sun.management.jmxremote.ssl=false"
                "-Dcom.sun.management.jmxremote.authenticate=false"
                "-Dcom.sun.management.jmxremote.port=$port")
            shift
            ;;
        -intellij-port=*)
            port="${arg#*=}"
            javaOptions=("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=$port")
            shift
            ;;
        -data-directory=*)
            SCHEDULER_AGENT_DATA="${arg#*=}"
            agentOptions+=("$arg")
            shift
            ;;
        *)
            agentOptions+=("$arg")
            shift
            ;;
    esac
done
. "$(cd "$(dirname "$0")" && pwd || kill $$)/set-context.sh"

export SCHEDULER_AGENT_DATA
crashKillScript=$([ -n "$SCHEDULER_AGENT_DATA" ] && echo "$SCHEDULER_AGENT_DATA/kill_tasks_after_crash.sh")
echo "crashKillScript=$crashKillScript"

logbackConfig=$(
    if [ -f "$SCHEDULER_AGENT_DATA/config/logback.xml" ]; then :
        echo "file:$SCHEDULER_AGENT_DATA/config/logback.xml"
    else
        echo "com/sos/scheduler/engine/agent/main/logback.xml"
    fi)
logbackArg="-Dlogback.configurationFile=$logbackConfig"
agentOptions=("-job-java-options=$logbackArg" "${agentOptions[@]}")
executeAgent=("$java" "${javaOptions[@]}" -classpath "$jarDir/*" "$logbackArg" com.sos.scheduler.engine.agent.main.AgentMain "${agentOptions[@]}")
echo "${executeAgent[@]}"
if [ -n "$crashKillScript" ]; then :
    rm -f "$crashKillScript"
    set +E
    "${executeAgent[@]}"
    returnCode=$?
    if [ -s "$crashKillScript" ]; then :
        ps fux || true
        echo Executing crash kill script $crashKillScript:
        cat $crashKillScript
        (. "$crashKillScript" || true)
        ps fux || true
    fi
    set -E
    exit $returnCode
else
    exec "${executeAgent[@]}"
fi
