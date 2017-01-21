import sbt.File

object BuildUtils {
  lazy val isWindows = sys.props("os.name") startsWith "Windows"

  def listFiles(directory: File): Vector[File] =
    (Option(directory.listFiles) getOrElse sys.error(s"Missing directory $directory")).toVector
}
