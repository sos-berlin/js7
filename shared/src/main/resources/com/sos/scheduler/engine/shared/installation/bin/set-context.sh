# Set variables
# - JAVA_HOME
# - java

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

jobschedulerHome=$(cd "$(dirname -- "$0")/.." && pwd || kill $$)

declare JAVA_HOME
declare -a classpath=()
if isWindows; then
  pathSeparator=";"
  classpath+=("$(cygpath -w "$jobschedulerHome/lib/*" || kill $$)")
  javaHome=""
  [ -n "$JAVA_HOME" ] && javaHome=$(cygpath "$JAVA_HOME" || kill $$)
  #unused javaHome=$(dirname $(dirname $(readlink --canonicalize $(which java || kill $$))))
else
  pathSeparator=":"
  classpath+=("$jobschedulerHome/lib/*")
  javaHome="$JAVA_HOME"
fi

java=java
if [ -n "$javaHome" ]; then :
    java="$javaHome/bin/java"
fi
