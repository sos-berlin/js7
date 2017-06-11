#!/usr/bin/env bash
set -e

export JOBSCHEDULER_HOME="$(cd "$(dirname -- "$0")/../bin/.." && pwd || kill $$)"
. "$JOBSCHEDULER_HOME/bin/set-context.sh"
declare classpath pathSeparator JAVA_HOME java

javaOptions=()
agentOptions=()
data=/var/opt/jobscheduler/agent/data
config=""

#if [ -z "$data" ]; then :
#    data="$agentHome"
#fi

httpPort=
for arg in "$@"; do
  case "$arg" in
    -rmx-port=*)
      a="${arg#*=}"
      javaOptions+=(
        "-Dcom.sun.management.jmxremote"
        "-Dcom.sun.management.jmxremote.ssl=false"
        "-Dcom.sun.management.jmxremote.authenticate=false"
        "-Dcom.sun.management.jmxremote.port=$a")
      shift
      ;;
    -debug-port=*)
      a="${arg#*=}"
      javaOptions+=("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=$a")
      shift
      ;;
    -java-option=*)
      a="${arg#*=}"
      javaOptions+=("$a")
      shift
      ;;
    -directory=*)
      config="${arg#*=}"/config
      data="${arg#*=}"/data
      shift
      ;;
    -config-directory=*)
      config="${arg#*=}"
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

[ -z "$config" ] || agentOptions+=("-config-directory=$(toSystemPath "$config" || kill $$)")
[ -z "$data" ] || agentOptions+=("-data-directory=$(toSystemPath "$data" || kill $$)")
[ -z "$httpPort" ] || agentOptions+=("-http-port=$httpPort")
logs="$data/logs"

crashKillScript=$([ -n "$data" ] && echo "$data/kill_tasks_after_crash.sh")
echo "crashKillScript=$crashKillScript"

[ -n "$config" ] || config="$data/config"
#if [ ! -d "$config" ]; then :
#  echo "Missing directory $config"
#  exit 1
#fi

[ -d "$logs" ] || mkdir "$logs"
export SCHEDULER_LOGS="$logs"  # Used in log4j2.xml
if [ -f "$config/log4j2.xml" ]; then :
  javaOptions+=("-Dlog4j.configurationFile=$config/log4j2.xml")
  javaOptions+=("-DLog4jContextSelector=org.apache.logging.log4j.core.async.AsyncLoggerContextSelector")
fi

execute=(
  "$java" \
  "${javaOptions[@]}" \
  -classpath "$(export IFS="$pathSeparator"; echo "${classpath[*]}")" \
  com.sos.jobscheduler.agent.main.AgentMain "${agentOptions[@]}"
)
echo "${execute[@]}"

if [ -n "$crashKillScript" ]; then :
  rm -f "$crashKillScript"

  "${execute[@]}" &
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
  exec "${execute[@]}"
fi
