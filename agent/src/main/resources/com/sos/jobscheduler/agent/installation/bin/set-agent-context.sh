# Set variables
# - JAVA_HOME
# - java
# - agentHome
# - data

. set-context.sh

declare agentHome
if [ -z "$agentHome" ]; then :
    agentHome=$(cd "$(dirname -- "$0")/.." && pwd || kill $$)
fi
