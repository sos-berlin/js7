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
  export JOBSCHEDULER_HOME="$(cd "$(dirname -- "$0")/../bin/.." && pwd || kill $$)"
fi

declare JAVA_HOME
declare -a classpath=()
if isWindows; then
  pathSeparator=";"
  classpath+=("$(cygpath -w "$JOBSCHEDULER_HOME/lib" || kill $$)/*")
  javaHome=""
  [ -n "$JAVA_HOME" ] && javaHome="$(cygpath "$JAVA_HOME" || kill $$)"
  #unused javaHome=$(dirname $(dirname $(readlink --canonicalize $(which java || kill $$))))
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
