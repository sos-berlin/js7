import java.nio.file.{FileVisitOption, Files, Path}
import sbt.File
import scala.collection.JavaConversions._
import scala.collection.immutable.Seq

object BuildUtils {
  lazy val isWindows = sys.props("os.name") startsWith "Windows"

  def recursiveFileMapping(directory: File, to: String = ""): Seq[(File, String)] = {
    val to_ = (to stripSuffix "/") + "/"
    for (file ← listFilesRecursively(directory)) yield
      file → (to_ + (file.toString stripPrefix directory.toString))
  }

  def listFiles(directory: File): Vector[File] =
    (Option(directory.listFiles) getOrElse sys.error(s"Missing directory $directory")).toVector

  def listFilesRecursively(directory: File, options: FileVisitOption*): Vector[File] = {
    val javaStream = Files.walk(directory.toPath, options: _*)
    try {
      val builder = Vector.newBuilder[File]
      for (file ← javaStream.iterator) builder += file.toFile
      builder.result.sortBy { _.toString }
    }
    finally javaStream.close()
  }
}
