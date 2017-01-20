# Set variables
# - SCHEDULER_AGENT_HOME
# - SCHEDULER_AGENT_DATA
# - javaDir
# - java

declare SCHEDULER_AGENT_HOME
if [ -z "$SCHEDULER_AGENT_HOME" ]; then :
    SCHEDULER_AGENT_HOME=$(cd "$(dirname "$0")/.." && pwd || kill $$)
fi

declare JAVA_HOME
declare OSTYPE
if [ "$OSTYPE" = "cygwin" ]; then
    jarDir=$(cygpath -w "$SCHEDULER_AGENT_HOME/jar" || kill $$)
    javaHome=""
    [ -n "$JAVA_HOME" ] && javaHome=$(cygpath "$JAVA_HOME" || kill $$)
else
    jarDir="$SCHEDULER_AGENT_HOME/jar"
    javaHome="$JAVA_HOME"
fi

declare SCHEDULER_AGENT_DATA
if [ -z "$SCHEDULER_AGENT_DATA" ]; then :
    SCHEDULER_AGENT_DATA="$SCHEDULER_AGENT_HOME"
fi

java=java
if [ -n "$javaHome" ]; then :
    java="$javaHome/bin/java"
fi
