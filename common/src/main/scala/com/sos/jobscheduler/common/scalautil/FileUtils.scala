package com.sos.jobscheduler.common.scalautil

import akka.util.ByteString
import com.google.common.base.Charsets.UTF_8
import com.google.common.io.FileWriteMode.APPEND
import com.google.common.io.{Files => GuavaFiles}
import com.sos.jobscheduler.base.circeutils.CirceUtils.CompactPrinter
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.JavaCollections.implicits._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.Closer.withCloser
import com.sos.jobscheduler.common.system.OperatingSystem.isUnix
import io.circe.Encoder
import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.nio.charset.Charset
import java.nio.file.Files.{delete, isSymbolicLink, setPosixFilePermissions}
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import java.nio.file.{FileAlreadyExistsException, FileVisitOption, Files, Path, Paths}
import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.language.implicitConversions

object FileUtils {

  val EmptyPath = Paths.get("")
  val WorkingDirectory = Paths.get(sys.props("user.dir")).toAbsolutePath

  /**
   * Touchs the file and deletes it when closer is closed.
   * Used mainly by tests.
   */
  def touchAndDeleteWithCloser[A <: Path](path: A)(implicit closer: Closer): path.type = {
    GuavaFiles.touch(path.toFile)
    path withCloser delete
    path
  }

  object implicits {
    implicit def pathToFile(path: Path): File = path.toFile

    implicit def fileToPath(file: File): Path = file.toPath

    implicit final class RichPath(private val delegate: Path) extends AnyVal
    {
      /** Writes `string` encoded with UTF-8 to file. */
      def :=[A <: CharSequence](string: A): Unit =
        contentString = string

      def :=(byteString: Array[Byte]): Unit =
        contentBytes = byteString

      def :=(byteString: Seq[Byte]): Unit =
        contentBytes = byteString

      def :=[A](a: A)(implicit jsonEncoder: Encoder[A]): Unit =
        contentString = jsonEncoder(a).pretty(CompactPrinter)

      /** Appends `string` encoded with UTF-8 to file. */
      def ++=(string: CharSequence): Unit =
        append(string)

      /** Must be a relative path without backslashes or single or double dot directories. */
      def /(relative: String): Path =
        delegate resolve checkRelativePath(relative).orThrow

      def byteString: ByteString = file.byteString

      def contentBytes: Array[Byte] = file.contentBytes

      def contentBytes_=(o: Array[Byte]): Unit = file.contentBytes = o

      def contentBytes_=(o: Seq[Byte]): Unit = file.contentBytes = o

      def contentString: String = file.contentString

      def contentString_=(o: CharSequence): Unit = file.contentString = o

      def contentString(encoding: Charset) = file.contentString(encoding)

      def writeExecutable(string: String): Unit = {
        this := string
        makeExecutable()
      }

      def makeExecutable(): Unit =
        if (isUnix) {
          setPosixFilePermissions(delegate, PosixFilePermissions.fromString("r-x------"))
        }

      def write(string: String, encoding: Charset = UTF_8): Unit =  file.write(string, encoding)

      def append(o: CharSequence, encoding: Charset = UTF_8): Unit = file.append(o, encoding)

      private def file = delegate.toFile

      /**
        * Returns the content of the directory denoted by `this`.
        */
      def pathSet: Set[Path] = autoClosing(Files.list(delegate)) { _.toSet }
    }

    implicit final class RichFile(private val delegate: File) extends AnyVal
    {
      def byteString: ByteString = ByteString(delegate.contentBytes)

      def contentBytes: Array[Byte] = GuavaFiles.toByteArray(delegate)

      def contentBytes_=(o: Array[Byte]): Unit = GuavaFiles.write(o, delegate)

      def contentBytes_=(o: Seq[Byte]): Unit = GuavaFiles.write(o.toArray, delegate)

      def contentString: String = contentString(UTF_8)

      def contentString_=(o: CharSequence): Unit = write(o, UTF_8)

      def contentString(encoding: Charset) = GuavaFiles.asCharSource(delegate, encoding).read()

      def write(string: CharSequence, encoding: Charset = UTF_8): Unit =  GuavaFiles.asCharSink(delegate, encoding).write(string)

      def append(string: CharSequence, encoding: Charset = UTF_8): Unit = GuavaFiles.asCharSink(delegate, encoding, APPEND).write(string)
    }
  }

  import implicits._

  def writeToFile[A](file: Path, append: Boolean = false)(body: OutputStream => A): A =
    autoClosing(new BufferedOutputStream(new FileOutputStream(file, append)))(body)

  @tailrec
  def createShortNamedDirectory(directory: Path, prefix: String): Path = {
    try Files.createDirectory(directory / (prefix + newRandomFilenameString()))
    catch {
      case _: FileAlreadyExistsException => createShortNamedDirectory(directory, prefix)
    }
  }

  private val FilenameCharacterSet = "abcdefghijklmnopqrstuvwxyz0123456789"
  private[scalautil] val ShortNameSize = 6  // Results in 36**6 == 2176782336 permutations
  private[scalautil] val ShortNamePermutationCount = List.fill(ShortNameSize)(FilenameCharacterSet.toSet.size.toLong).product

  private def newRandomFilenameString() =
    (Iterator.fill(ShortNameSize) { ThreadLocalRandom.current.nextInt(FilenameCharacterSet.length) } map FilenameCharacterSet).mkString

  def withTemporaryFile[A](body: Path => A): A = withTemporaryFile(prefix = "", suffix = ".tmp")(body)

  def withTemporaryFile[A](prefix: String, suffix: String, attributes: FileAttribute[_]*)(body: Path => A): A =
    autoDeleting(Files.createTempFile(prefix, suffix, attributes: _*))(body)


  def autoDeleting[A](file: Path)(body: Path => A): A =
    withCloser { closer =>
      closer.onClose {
        delete(file)
      }
      body(file)
  }

  def withTemporaryDirectory[A](prefix: String = "")(body: Path => A): A = {
    val dir = Files.createTempDirectory(prefix)
    withCloser { closer =>
      closer.onClose {
        deleteDirectoryRecursively(dir)
      }
      body(dir)
    }
  }

  def deleteDirectoryRecursively(dir: Path): Unit = {
    require(dir.isDirectory, s"Not a directory: $dir")
    if (!isSymbolicLink(dir)) {
      deleteDirectoryContentRecursively(dir)
    }
    delete(dir)
  }

  def deleteDirectoryContentRecursively(dir: Path): Unit = {
    for (f <- dir.pathSet) {
      if (f.isDirectory && !isSymbolicLink(f)) deleteDirectoryContentRecursively(f)
      delete(f)
    }
  }

  def nestedPathsIterator(directory: Path, options: FileVisitOption*): AutoCloseable with Iterator[Path] =
    new AbstractIterator[Path] with AutoCloseable {
      private val javaStream = Files.walk(directory, options: _*)
      private val iterator = javaStreamToIterator(javaStream)

      def close() = javaStream.close()
      def hasNext = iterator.hasNext
      def next() = iterator.next()
    }

  private val RelativePathRegex = """^(/|\\|..?/|..?\\)|([/\\]..?([/\\]|$))""".r.unanchored

  def checkRelativePath(path: String): Checked[String] =
    if (path.isEmpty)
      Left(Problem("Relative file path must not be empty"))
    else if (RelativePathRegex.pattern.matcher(path).find())
      Left(Problem(s"Not a valid relative file path: $path"))
    else
      Right(path)
}
