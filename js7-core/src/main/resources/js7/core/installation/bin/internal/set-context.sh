# Set variables
# - JS7_HOME
# - JAVA_HOME
# - java
# - pathSeparator
# - classpathString

if [ -z "${JS7_HOME:-}" ]; then :
  JS7_HOME="/opt/js7"
  export JS7_HOME
fi

declare JAVA_HOME
declare -a classpath=()
. <(grep -E "^JAVA_VERSION=" "$JAVA_HOME/release")
JAVA_VERSION_MAJOR="$(echo "$JAVA_VERSION" | awk -F . '{print $1}')"

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
#standardJavaOptions+=("-Djs7.virtualThreads=on")
standardJavaOptions+=("-Dcats.effect.warnOnNonMainThreadDetected=false") # TODO Before Cats Effect 3.6
standardJavaOptions+=("-XX:MaxJavaStackTraceDepth=999999")  # To analyze StackOverflowError
if [[ "$JAVA_VERSION_MAJOR" -ge 24 ]]; then
  standardJavaOptions+=("--sun-misc-unsafe-memory-access=allow") # TODO Before Scala 3.8
  standardJavaOptions+=("-XX:+UnlockExperimentalVMOptions" "-XX:+UseCompactObjectHeaders")
fi
standardJavaOptions+=("-XX:+UseStringDeduplication")


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
