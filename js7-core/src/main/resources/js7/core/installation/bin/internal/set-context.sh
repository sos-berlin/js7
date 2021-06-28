# Set variables
# - JS7_HOME
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

if [ -z "${JS7_HOME:-}" ]; then :
  JS7_HOME="$(cd "${0%/*}/../bin/.." && pwd)"
  export JS7_HOME
fi

declare JAVA_HOME
declare -a classpath=()
if isWindows; then
  pathSeparator=";"
  classpath+=("$(cygpath -w "$JS7_HOME/lib")/*")
  javaHome=""
  [ -n "$JAVA_HOME" ] && javaHome="$(cygpath "$JAVA_HOME")"
  #unused javaHome=$(dirname $(dirname $(readlink --canonicalize $(which java))))
else
  pathSeparator=":"
  classpath+=("$JS7_HOME/lib/*")
  javaHome="$JAVA_HOME"
fi

classpathString="$(export IFS="$pathSeparator"; echo "${classpath[*]}")"

java=java
if [ -n "$javaHome" ]; then :
    java="$javaHome/bin/java"
fi

timestamp() {
  local t
  t=$(date +"%Y-%m-%d %H:%M:%S.%N")
  echo "${t:0:23}"
}

log() {
  if [ $# -eq 0 ]; then
    echo
  else
    echo "$(timestamp)" "$@"
  fi
}
