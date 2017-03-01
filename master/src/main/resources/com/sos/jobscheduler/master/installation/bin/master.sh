#!/usr/bin/env bash
set -e

# Simple JobScheduler Master starter

# Example usage:
#   cd /tmp
#   rm -rf engine
#   tar xfz .../master/target/jobscheduler-master-bin.tar.gz
#   export SCHEDULER_DATA=data (containing config/, config/live/ and log/)
#   master-x.y.z/bin/master.sh

. "$(cd "$(dirname -- "$0")" && pwd || kill $$)/set-context.sh"
declare jobschedulerHome classpath pathSeparator JAVA_HOME java

data=/var/lib/jobscheduler/master
httpPort=4444
declare -a masterOptions=()

for arg in "$@"; do :
  case $arg in
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

[ -z "$data" ] || masterOptions+=("-data-directory=$(toSystemPath "$data" || kill $$)")
[ -z "$httpPort" ] || masterOptions+=("-http-port=$httpPort")
logs="$data/logs"

configDirectory="$data"/config
if [ ! -d "$configDirectory" ]; then :
  echo "Missing directory $configDirectory"
  exit 1
fi

liveDirectory="$data"/config/live
if [ ! -d "$liveDirectory" ]; then :
  echo "Missing directory $liveDirectory"
  exit 1
fi

export SCHEDULER_LOGS="$logs"  # Used in logback.xml
if [ -f "$configDirectory/logback.xml" ]; then :
  logbackConfig="file:$configDirectory/logback.xml"
else
  logbackConfig="com/sos/jobscheduler/master/logback.xml"
fi

logbackArg="-Dlogback.configurationFile=$logbackConfig"
javaOptions=("$logbackArg" "${javaOptions[@]}")

execute=(
  "$java"
  "${javaOptions[@]}"
  -classpath "$(export IFS="$pathSeparator"; echo "${classpath[*]}")"
  com.sos.jobscheduler.master.MasterMain "${masterOptions[@]}"
)
echo "${execute[@]}"
exec "${execute[@]}"
