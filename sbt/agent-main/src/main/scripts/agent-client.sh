#! /bin/bash
set -e

. "$(cd "$(dirname "$0")" && pwd || kill $$)/set-context.sh"

"$java" -classpath "$jarDir/*" -Dlogback.configurationFile="com/sos/scheduler/engine/agent/client/main/logback.xml" com.sos.scheduler.engine.agent.client.main.AgentClientMain "$@"
