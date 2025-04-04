# Set variables
# - JS7_HOME
# - JAVA_HOME
# - java
# - pathSeparator
# - classpathString

if [ -z "${JS7_HOME:-}" ]; then :
  JS7_HOME="$(cd "${0%/*}/../bin/.." && pwd)"
  export JS7_HOME
fi

declare JAVA_HOME
declare -a classpath=()
. <(egrep "^JAVA_VERSION=" "$JAVA_HOME/release")

if [ "$(uname -o)" == "Cygwin" ]; then
  function toUnixPath() {
    cygpath "$@"
  }
  function toSystemPath() {
    cygpath -w "$@"
  }
  pathSeparator=";"
  classpath+=("$(cygpath -w "$JS7_HOME/lib")/*")
  javaHome=""
  [ -n "$JAVA_HOME" ] && javaHome="$(cygpath "$JAVA_HOME")"
  #unused javaHome=$(dirname $(dirname $(readlink --canonicalize $(which java))))
else
  function toUnixPath() {
    echo "$@"
  }
  function toSystemPath() {
    echo "$@"
  }
  pathSeparator=":"
  classpath+=("$JS7_HOME/lib/*")
  javaHome="$JAVA_HOME"
fi

classpathString="$(export IFS="$pathSeparator"; echo "${classpath[*]}")"

java=java
if [ -n "$javaHome" ]; then :
  java="$javaHome/bin/java"
fi

standardJavaOptions=()
standardJavaOptions+=("-Dfile.encoding=UTF-8")
standardJavaOptions+=("-XX:MaxJavaStackTraceDepth=999999")  # To analyze StackOverflowError
if [[ "$JAVA_VERSION" =~ ([0-9]+).* && ${BASH_REMATCH[1]} -ge 24 ]]; then
  standardJavaOptions+=("-XX:+UnlockExperimentalVMOptions" "-XX:+UseCompactObjectHeaders")
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
