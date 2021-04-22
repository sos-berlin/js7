package js7.base.system

object OperatingSystem {
  val name: String = sys.props("os.name")
  val isJVM = sys.props.contains("java.version")
  val isWindows = name startsWith "Windows"
  val isMac = name startsWith "Mac OS"
  val isUnix = !isWindows
  val isSolaris = name startsWith "SunOS"
  val LineEnd = if (isWindows) "\r\n" else "\n"
}
