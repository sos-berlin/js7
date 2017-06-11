#!/usr/bin/env bash
set -e

# Simple JobScheduler Master starter

# Example usage:
#   cd /tmp
#   rm -rf engine
#   tar xfz .../master/target/jobscheduler-master-bin.tar.gz
#   export SCHEDULER_DATA=data (containing config/, config/live/ and log/)
#   master-x.y.z/bin/master.sh

export JOBSCHEDULER_HOME="$(cd "$(dirname -- "$0")/../bin/.." && pwd || kill $$)"
. "$JOBSCHEDULER_HOME/bin/set-context.sh"
declare classpath pathSeparator JAVA_HOME java

data=/var/opt/jobscheduler/master/data
config=""
httpPort=4444
masterOptions=()
javaOptions=()

for arg in "$@"; do :
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
      masterOptions+=("$arg")
      shift
      ;;
  esac
done

[ -z "$config" ] || masterOptions+=("-config-directory=$(toSystemPath "$config" || kill $$)")
[ -z "$data" ] || masterOptions+=("-data-directory=$(toSystemPath "$data" || kill $$)")
[ -z "$httpPort" ] || masterOptions+=("-http-port=$httpPort")
logs="$data/logs"

[ -n "$config" ] || config="$data/config"
if [ ! -d "$config" ]; then :
  echo "Missing directory $config"
  exit 1
fi

live="$config/live"
if [ ! -d "$live" ]; then :
  echo "Missing directory $live"
  exit 1
fi

[ -d "$logs" ] || mkdir "$logs"
export SCHEDULER_LOGS="$logs"  # Used in log4j2.xml
if [ -f "$config/log4j2.xml" ]; then :
  javaOptions+=("-Dlog4j.configurationFile=$config/log4j2.xml")
  javaOptions+=("-DLog4jContextSelector=org.apache.logging.log4j.core.async.AsyncLoggerContextSelector")
fi

execute=(
  "$java"
  "${javaOptions[@]}"
  -classpath "$(export IFS="$pathSeparator"; echo "${classpath[*]}")"
  com.sos.jobscheduler.master.MasterMain "${masterOptions[@]}"
)
echo "${execute[@]}"
exec "${execute[@]}"
