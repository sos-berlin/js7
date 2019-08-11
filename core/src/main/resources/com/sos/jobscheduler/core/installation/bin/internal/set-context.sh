# Set variables
# - JOBSCHEDULER_HOME
# - JAVA_HOME
# - java
# - pathSeparator
# - classpathString

isWindows() {
  [ "$(uname -o)" = "Cygwin" ]
}

toUnixPath() {
  if (isWindows); then
    cygpath "$@"
  else
    echo "$@"
  fi
}

toSystemPath() {
  if isWindows; then
    cygpath -w "$@"
  else
    echo "$@"
  fi
}

if [ -z "$JOBSCHEDULER_HOME" ]; then :
  JOBSCHEDULER_HOME="$(cd "${0%/*}/../bin/.." && pwd)"
  export JOBSCHEDULER_HOME
fi

declare JAVA_HOME
declare -a classpath=()
if isWindows; then
  pathSeparator=";"
  classpath+=("$(cygpath -w "$JOBSCHEDULER_HOME/lib")/*")
  javaHome=""
  [ -n "$JAVA_HOME" ] && javaHome="$(cygpath "$JAVA_HOME")"
  #unused javaHome=$(dirname $(dirname $(readlink --canonicalize $(which java))))
else
  pathSeparator=":"
  classpath+=("$JOBSCHEDULER_HOME/lib/*")
  javaHome="$JAVA_HOME"
fi

classpathString="$(export IFS="$pathSeparator"; echo "${classpath[*]}")"

java=java
if [ -n "$javaHome" ]; then :
    java="$javaHome/bin/java"
fi
