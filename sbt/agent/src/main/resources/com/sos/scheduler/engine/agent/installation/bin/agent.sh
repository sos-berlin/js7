#!/usr/bin/env bash
set -e

. "$(cd "$(dirname -- "$0")" && pwd || kill $$)/set-context.sh"
declare jobschedulerHome classpath pathSeparator JAVA_HOME java

declare -a javaOptions=()
declare -a agentOptions=()
data=/var/lib/jobscheduler/agent

#if [ -z "$data" ]; then :
#    data="$agentHome"
#fi

httpPort=
for arg in "$@"; do
  case $arg in
    -rmx-port=*)
      a="${arg#*=}"
      javaOptions=(
        "-Dcom.sun.management.jmxremote"
        "-Dcom.sun.management.jmxremote.ssl=false"
        "-Dcom.sun.management.jmxremote.authenticate=false"
        "-Dcom.sun.management.jmxremote.port=$a")
      shift
      ;;
    -debug-port=*)
      a="${arg#*=}"
      javaOptions=("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=$a")
      shift
      ;;
    -data-directory=*)
      data="${arg#*=}"
      shift
      ;;
    -http-port=*)
      httpPort="${arg#*=}"
      shift
      ;;
    *)
      agentOptions+=("$arg")
      shift
      ;;
  esac
done

[ -z "$data" ] || agentOptions+=("-data-directory=$(toSystemPath "$data" || kill $$)")
[ -z "$httpPort" ] || agentOptions+=("-http-port=$httpPort")
logs="$data/logs"

crashKillScript=$([ -n "$data" ] && echo "$data/kill_tasks_after_crash.sh")
echo "crashKillScript=$crashKillScript"

export SCHEDULER_LOGS="$logs"  # Used in logback.xml
logbackConfig=$(
  if [ -f "$data/config/logback.xml" ]; then :
    echo "file:$data/config/logback.xml"
  else
    echo "com/sos/scheduler/engine/agent/main/logback.xml"
  fi)
logbackArg="-Dlogback.configurationFile=$logbackConfig"
agentOptions=("-job-java-options=$logbackArg" "${agentOptions[@]}")
executeAgent=(
  "$java" \
  "${javaOptions[@]}" \
  -classpath "$(export IFS="$pathSeparator"; echo "${classpath[*]}")" \
  "$logbackArg" com.sos.scheduler.engine.agent.main.AgentMain "${agentOptions[@]}"
)
echo "${executeAgent[@]}"

if [ -n "$crashKillScript" ]; then :
  rm -f "$crashKillScript"

  "${executeAgent[@]}" &
  trap "kill -SIGTERM $! && wait $!" SIGTERM
  wait $!
  trap - SIGTERM

  returnCode=$?
  if [ -s "$crashKillScript" ]; then :
    ps fux || true
    echo Executing crash kill script $crashKillScript:
    cat $crashKillScript
    (. "$crashKillScript" || true)
    ps fux || true
  fi
  exit $returnCode
else
  exec "${executeAgent[@]}"
fi
