package js7.base.io.file

import com.google.common.io.FileWriteMode.APPEND
import com.google.common.io.{Files => GuavaFiles}
import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Files.{delete, deleteIfExists, isDirectory, isSymbolicLink, setPosixFilePermissions}
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import java.nio.file.{FileAlreadyExistsException, FileVisitOption, Files, Path, Paths}
import java.util.concurrent.ThreadLocalRandom
import js7.base.data.Writable.ops._
import js7.base.data.{ByteArray, ByteSequence, Writable}
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.{isUnix, isWindows}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax._
import js7.base.utils.Closer.withCloser
import js7.base.utils.JavaCollections.syntax._
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.language.implicitConversions

object FileUtils
{
  val EmptyPath = Paths.get("")
  val WorkingDirectory = Paths.get(sys.props("user.dir")).toAbsolutePath
  def temporaryDirectory = Paths.get(sys.props("java.io.tmpdir"))

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
    implicit val PathOrdering: Ordering[Path] = (a, b) => a compareTo b

    implicit final class RichPath(private val delegate: Path) extends AnyVal
    {
      /** Must be a relative path without backslashes or single or double dot directories. */
      def /(relative: String): Path =
        delegate resolve checkRelativePath(relative).orThrow

      /** Writes `string` encoded with UTF-8 to file. */
      def :=[A <: CharSequence](string: A): Unit =
        contentString = string

      def :=(bytes: collection.Seq[Byte]) = write(bytes)

      def write(bytes: collection.Seq[Byte]): Unit =
        write(bytes.toArray)

      def :=(bytes: Array[Byte]) = write(bytes)

      def write(bytes: Array[Byte]): Unit =
        Files.write(delegate, bytes)

      def write(string: CharSequence, encoding: Charset = UTF_8): Unit =
        // Java 11: Files.writeString(delegate, string, encoding, CREATE, TRUNCATE_EXISTING)
        GuavaFiles.asCharSink(delegate.toFile, encoding).write(string)

      def :=(byteSeq: ByteArray) = write(byteSeq)

      def write(byteSeq: ByteArray): Unit =
        autoClosing(new BufferedOutputStream(new FileOutputStream(delegate.toFile)))(
          byteSeq.writeToStream(_))

      def :=[W: Writable](w: W) = write(w)

      def write[W: Writable](w: W): Unit =
        autoClosing(new BufferedOutputStream(new FileOutputStream(delegate.toFile)))(
          w.writeToStream(_))

      /** Appends `string` encoded with UTF-8 to file. */
      def ++=(string: CharSequence): Unit =
        append(string)

      def append(string: CharSequence, encoding: Charset = UTF_8): Unit =
        // Java 11: Files.writeString(delegate, string, encoding, CREATE, APPEND)
        GuavaFiles.asCharSink(delegate.toFile, encoding, APPEND).write(string)

      def ++=[B: ByteSequence](byteSeq: B): Unit=
        autoClosing(new BufferedOutputStream(new FileOutputStream(delegate.toFile, true)))(
          byteSeq.writeToStream(_))

      def byteArray: ByteArray =
        readAs[ByteArray]

      def readAs[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq]): ByteSeq =
        ByteSeq.unsafeWrap(contentBytes)

      def contentBytes: Array[Byte] =
        Files.readAllBytes(delegate)

      def contentString: String = contentString(UTF_8)

      def contentString_=(string: CharSequence): Unit =
        write(string, UTF_8)

      def contentString(encoding: Charset) =
        // Java 11: Files.readString(encoding)
        GuavaFiles.asCharSource(delegate.toFile, encoding).read()

      def writeExecutable(string: String): Unit = {
        write(string, if (isWindows) ISO_8859_1 else UTF_8)
        makeExecutable()
      }

      def makeExecutable(): Unit =
        if (isUnix) {
          setPosixFilePermissions(delegate, PosixFilePermissions.fromString("r-x------"))
        }

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
  private[io] val ShortNameSize = 6  // Results in 36**6 == 2176782336 permutations
  private[io] val ShortNamePermutationCount = List.fill(ShortNameSize)(FilenameCharacterSet.toSet.size.toLong).product

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
