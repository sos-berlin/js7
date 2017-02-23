#!/usr/bin/env bash
set -e

. "$(cd "$(dirname -- "$0")" && pwd || kill $$)/set-context.sh"

"$java"\
  -classpath "$lib/*"\
  -Dlogback.configurationFile="com/sos/jobscheduler/agent/client/main/logback.xml"\
  com.sos.jobscheduler.agent.client.main.AgentClientMain\
  "$@"
