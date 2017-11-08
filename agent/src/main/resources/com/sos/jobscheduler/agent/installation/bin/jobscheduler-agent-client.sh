#!/usr/bin/env bash
set -e

. "$(cd "$(dirname -- "$0")" && pwd || kill $$)/internal/set-context.sh"
declare java lib

"$java"\
  -classpath "$lib/*"\
  -Dlog4j.configurationFile="com/sos/jobscheduler/agent/client/main/log4j2.xml"\
  com.sos.jobscheduler.agent.client.main.AgentClientMain\
  "$@"
