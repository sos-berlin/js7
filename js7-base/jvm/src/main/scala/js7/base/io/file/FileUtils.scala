package js7.base.io.file

import cats.effect.Resource
import java.io.{BufferedOutputStream, File, FileOutputStream, IOException, OutputStreamWriter}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Files.{copy, createDirectory, delete, deleteIfExists, isDirectory, isSymbolicLink, newOutputStream, setPosixFilePermissions}
import java.nio.file.StandardOpenOption.{APPEND, CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import java.nio.file.{FileAlreadyExistsException, FileVisitOption, Files, OpenOption, Path, Paths}
import java.util.Objects.requireNonNull
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
import monix.eval.Task
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
    touchFile(path)
    path withCloser delete
    path
  }

  def touchFile(path: Path): Unit = {
    val file = path.toFile
    if (!file.createNewFile() && !file.setLastModified(System.currentTimeMillis))
      throw new IOException("touchFile file: unable to update modification time of " + file)
  }

  def copyDirectory(from: Path, to: Path): Unit = {
    createDirectory(to)
    autoClosing(Files.list(from)) { stream =>
      for (file <- stream.asScala) {
        val destination = to.resolve(file.getFileName)
        if (isDirectory(file))
          copyDirectory(file, destination)
        else
          copy(file, destination)
      }
    }
  }

  // COMPATIBLE with Java 11 Files.writeString
  def writeString(path: Path, chars: CharSequence, options: OpenOption*): Path =
    writeString(path, chars, UTF_8, options: _*)

  // COMPATIBLE with Java 11 Files.writeString
  def writeString(path: Path, chars: CharSequence, charset: Charset, options: OpenOption*)
  : Path = {
    requireNonNull(chars)
    autoClosing(new OutputStreamWriter(newOutputStream(path, options: _*), charset)) {
      _.append(chars)
    }
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
      def :=(string: String): Unit =
        contentString = string

      def :=(bytes: collection.Seq[Byte]) = write(bytes)

      def :=(bytes: Array[Byte]) = write(bytes)

      def write(bytes: Array[Byte]): Unit =
        Files.write(delegate, bytes)


      def :=[W: Writable](w: W) = write(w)

      def write[W: Writable](byteSeq: W): Unit =
        autoClosing(new BufferedOutputStream(new FileOutputStream(delegate.toFile)))(
          byteSeq.writeToStream(_))

      /** Appends `string` encoded with UTF-8 to file. */
      def ++=(string: String): Unit =
        append(string)

      def append(string: String, encoding: Charset = UTF_8): Unit =
        write(string, encoding, append = true)

      def write(bytes: collection.Seq[Byte]): Unit =
        write(bytes.toArray)

      def write(string: String, encoding: Charset = UTF_8, append: Boolean = false): Unit = {
        val options = Vector(CREATE, WRITE, if (append) APPEND else TRUNCATE_EXISTING)
        writeString(delegate, string, encoding, options: _*)
      }

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

      def contentString_=(string: String): Unit =
        write(string, UTF_8)

      def contentString(encoding: Charset) =
        // Java 11: Files.readString(encoding)
        new String(ByteArray.fromFileUnlimited(delegate).unsafeArray, encoding)

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

  def temporaryDirectoryResource(prefix: String): Resource[Task, Path] =
    Resource.make(
      acquire = Task(Files.createTempDirectory(prefix)))(
      release = dir => Task(deleteDirectoryRecursively(dir)))

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
