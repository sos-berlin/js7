import java.nio.file.{FileVisitOption, Files}
import sbt.{File, ModuleID}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

object BuildUtils {
  lazy val isWindows = sys.props("os.name") startsWith "Windows"
  lazy val isMac = sys.props("os.name") startsWith "Mac OS"

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] = o :: Nil

  implicit class PercentModuleIDSeq(val delegate: Seq[sbt.ModuleID]) extends AnyVal {
    def %(configurations: String) = delegate map { _ % configurations }
  }

  def recursiveFileMapping(directory: File, to: String = ""): Seq[(File, String)] = {
    val to_ = (to stripSuffix "/") + "/"
    for (file ← listFilesRecursively(directory)) yield
      file → (to_ + (file.toString stripPrefix directory.toString stripPrefix "/"))
  }

  def listFiles(directory: File): Vector[File] =
    (Option(directory.listFiles) getOrElse sys.error(s"Missing directory $directory")).toVector

  def listFilesRecursively(directory: File, options: FileVisitOption*): Vector[File] = {
    val javaStream = Files.walk(directory.toPath, options: _*)
    try {
      val builder = Vector.newBuilder[File]
      for (file ← javaStream.iterator.asScala) builder += file.toFile
      builder.result.sortBy { _.toString }
    }
    finally javaStream.close()
  }
}
