package js7.common.scalautil

import akka.util.ByteString
import com.google.common.io.FileWriteMode.APPEND
import com.google.common.io.{Files => GuavaFiles}
import js7.base.circeutils.CirceUtils.CompactPrinter
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax._
import js7.base.utils.Closer.withCloser
import js7.base.utils.JavaCollections.syntax._
import js7.common.system.OperatingSystem.isUnix
import io.circe.Encoder
import java.io.{File, FileOutputStream}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{delete, deleteIfExists, isDirectory, isSymbolicLink, readAllBytes, setPosixFilePermissions}
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import java.nio.file.{FileAlreadyExistsException, FileVisitOption, Files, Path, Paths}
import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.language.implicitConversions
import scodec.bits.ByteVector

object FileUtils
{
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
    implicit def pathToFile(path: Path): File =
      path.toFile

    implicit def fileToPath(file: File): Path =
      file.toPath
  }

  object syntax
  {
    implicit final class RichPath(private val delegate: Path) extends AnyVal
    {
      /** Must be a relative path without backslashes or single or double dot directories. */
      def /(relative: String): Path =
        delegate resolve checkRelativePath(relative).orThrow

      /** Writes `string` encoded with UTF-8 to file. */
      def :=[A <: CharSequence](string: A): Unit =
        contentString = string

      def :=(bytes: Array[Byte]): Unit =
        contentBytes = bytes

      def :=(bytes: collection.Seq[Byte]): Unit =
        contentBytes = bytes

      def :=(byteVector: ByteVector): Unit =
        autoClosing(new FileOutputStream(delegate.toFile))(byteVector.copyToStream)

      def :=[A](a: A)(implicit jsonEncoder: Encoder[A]): Unit =
        contentString = jsonEncoder(a).printWith(CompactPrinter)

      /** Appends `string` encoded with UTF-8 to file. */
      def ++=(string: CharSequence): Unit =
        append(string)

      def ++=(byteVector: ByteVector): Unit=
        autoClosing(new FileOutputStream(delegate.toFile, true))(byteVector.copyToStream)

      def byteVector: ByteVector =
        ByteVector(delegate.contentBytes)

      def byteString: ByteString =
        ByteString(delegate.contentBytes)

      def contentBytes: Array[Byte] =
        readAllBytes(delegate)

      def contentBytes_=(bytes: Array[Byte]): Unit =
        Files.write(delegate, bytes)

      def contentBytes_=(bytes: collection.Seq[Byte]): Unit =
        Files.write(delegate, bytes.toArray)

      def contentString: String = contentString(UTF_8)

      def contentString_=(string: CharSequence): Unit =
        write(string, UTF_8)

      def contentString(encoding: Charset) =
        GuavaFiles.asCharSource(delegate.toFile, encoding).read()

      def writeExecutable(string: String): Unit = {
        this := string
        makeExecutable()
      }

      def makeExecutable(): Unit =
        if (isUnix) {
          setPosixFilePermissions(delegate, PosixFilePermissions.fromString("r-x------"))
        }

      def write(string: CharSequence, encoding: Charset = UTF_8): Unit =
        GuavaFiles.asCharSink(delegate.toFile, encoding).write(string)

      def append(string: CharSequence, encoding: Charset = UTF_8): Unit =
        GuavaFiles.asCharSink(delegate.toFile, encoding, APPEND).write(string)

      /**
        * Returns the content of the directory denoted by `this`.
        */
      def directoryContents: Seq[Path] =
        directoryContentsAs(Vector)

      /**
        * Returns the content of the directory denoted by `this`.
        */
      def directoryContentsAs[C](factory: collection.Factory[Path, C]): C =
        autoClosing(Files.list(delegate)) { _.asScala.to(factory) }
    }
  }

  import syntax._

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
        deleteIfExists(file)
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
    require(isDirectory(dir), s"Not a directory: $dir")
    if (!isSymbolicLink(dir)) {
      deleteDirectoryContentRecursively(dir)
    }
    delete(dir)
  }

  def deleteDirectoryContentRecursively(dir: Path): Unit = {
    for (f <- dir.directoryContents) {
      if (isDirectory(f) && !isSymbolicLink(f)) deleteDirectoryContentRecursively(f)
      delete(f)
    }
  }

  def nestedPathsIterator(directory: Path, options: FileVisitOption*): AutoCloseable with Iterator[Path] =
    new AbstractIterator[Path] with AutoCloseable {
      private val javaStream = Files.walk(directory, options: _*)
      private val underlyingIterator = javaStream.asScala

      def close() = javaStream.close()
      def hasNext = underlyingIterator.hasNext
      def next() = underlyingIterator.next()
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
