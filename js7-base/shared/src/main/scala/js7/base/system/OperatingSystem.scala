package js7.base.system

object OperatingSystem:

  val name: String = sys.props("os.name")
  val isJVM: Boolean = sys.props.contains("java.version")
  val isWindows: Boolean = name.startsWith("Windows")
  val isMac: Boolean = name.startsWith("Mac OS")
  val isUnix: Boolean = !isWindows
  val isSolaris: Boolean = name.startsWith("SunOS")
  val LineEnd: String = if isWindows then "\r\n" else "\n"
  val PathEnvName: String = if isWindows then "Path" else "PATH"
