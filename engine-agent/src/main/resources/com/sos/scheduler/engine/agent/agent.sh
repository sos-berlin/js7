#! /bin/bash
set -e

[ -z "$SCHEDULER_AGENT_HOME" ] && SCHEDULER_AGENT_HOME=$(cd "$(dirname "$0")/.."; pwd)

if [ "$OSTYPE" = "cygwin" ]; then
    jarDir=$(cygpath -w "$SCHEDULER_AGENT_HOME/jar")
    javaHome=""
    [ -n "$JAVA_HOME" ] && javaHome=$(cygpath "$JAVA_HOME")
else
    jarDir="$SCHEDULER_AGENT_HOME/jar"
    javaHome="$JAVA_HOME"
fi

java=java
[ -n "$javaHome" ] && java="$javaHome/bin/java"

"$java" -classpath "$jarDir/*" com.sos.scheduler.engine.agent.Agent "$@"
