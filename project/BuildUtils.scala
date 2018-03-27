import java.nio.ByteBuffer
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{FileVisitOption, Files}
import java.util.jar.JarFile
import java.util.{Base64, UUID}
import sbt.{File, ModuleID}
import scala.collection.JavaConverters._

object BuildUtils {
  val isWindows = sys.props("os.name") startsWith "Windows"
  val isMac = sys.props("os.name") startsWith "Mac OS"

  def initializeLogger(): Unit = {
    sys.props ++= List(
      "Log4jContextSelector" → "org.apache.logging.log4j.core.async.AsyncLoggerContextSelector",
      "AsyncLogger.WaitStrategy" → "Blocking")
  }

  def newBuildId: String = {
    val uuid = UUID.randomUUID
    val buffer = ByteBuffer.wrap(new Array[Byte](16))
    buffer.putLong(uuid.getMostSignificantBits)
    buffer.putLong(uuid.getLeastSignificantBits)
    Base64.getUrlEncoder.encodeToString(buffer.array) stripSuffix "=="
  }

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] = o :: Nil

  implicit final class PercentModuleIDSeq(private val delegate: Seq[sbt.ModuleID]) extends AnyVal {
    def %(configurations: String) = delegate map { _ % configurations }
  }

  def copyJarEntries(jar: File, entryToFile: Seq[(String, File)]): Seq[File] = {
    val jarFile = new JarFile(jar)
    try for ((entryPath, file) ← entryToFile) yield {
      val entry = jarFile.getEntry(entryPath)
      if (entry == null) sys.error(s"Missing entry in $jar: $entryPath")
      Files.createDirectories(file.toPath.getParent)
      Files.copy(jarFile.getInputStream(entry), file.toPath, REPLACE_EXISTING)
      file
    } finally jarFile.close()
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
