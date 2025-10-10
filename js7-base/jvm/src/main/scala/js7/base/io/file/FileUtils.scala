package js7.base.io.file

import cats.effect.{IO, Resource, Sync}
import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream, IOException, InputStream}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{copy, createDirectory, delete, deleteIfExists, exists, isDirectory, isSymbolicLink, setPosixFilePermissions}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.StandardOpenOption.{APPEND, CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import java.nio.file.{CopyOption, FileAlreadyExistsException, FileVisitOption, Files, Path, Paths}
import java.util.concurrent.ThreadLocalRandom
import js7.base.data.Writable.ops.*
import js7.base.data.{ByteArray, ByteSequence, Writable}
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isUnix
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.*
import js7.base.utils.Closer.withCloser
import js7.base.utils.JavaCollections.syntax.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.{Closer, Labeled}
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.language.implicitConversions
import scala.util.control.NonFatal

object FileUtils:

  private val logger = Logger[this.type]
  val EmptyPath: Path = Paths.get("")
  val WorkingDirectory: Path = Paths.get(sys.props("user.dir")).toAbsolutePath

  def temporaryDirectory: Path = Paths.get(sys.props("java.io.tmpdir"))

  /**
   * Touchs the file and deletes it when closer is closed.
   * Used mainly by tests.
   */
  def touchAndDeleteWithCloser[A <: Path](path: A)(implicit closer: Closer): path.type =
    touchFile(path)
    path.withCloser(delete)
    path

  def touchFile(path: Path): Unit =
    val file = path.toFile
    if !file.createNewFile() && !file.setLastModified(System.currentTimeMillis) then
      throw new IOException("touchFile file: unable to update modification time of " + file)

  def copyDirectoryContent(from: Path, to: Path, options: CopyOption*): Unit =
    if !exists(to) then createDirectory(to)
    autoClosing(Files.list(from)): stream =>
      for file <- stream.asScala do
        val destination = to.resolve(file.getFileName)
        if isDirectory(file) then
          copyDirectoryContent(file, destination)
        else
          copy(file, destination, options*)

  object implicits:
    implicit def pathToFile(path: Path): File =
      path.toFile

    implicit def fileToPath(file: File): Path =
      file.toPath

  object syntax:
    implicit val PathOrdering: Ordering[Path] = (a, b) => a.compareTo(b)

    implicit final class RichPath(private val delegate: Path) extends AnyVal:
      /** Must be a relative path without backslashes or single or double dot directories. */
      def /(relative: String): Path =
        delegate.resolve(checkRelativePath(relative).orThrow)

      /** Writes `string` encoded with UTF-8 to file. */
      def :=(string: String): Unit =
        contentString = string

      def :=(bytes: collection.Seq[Byte]): Unit =
        write(bytes)

      def :=(bytes: Array[Byte]): Unit =
        write(bytes)

      def write(bytes: Array[Byte]): Unit =
        Files.write(delegate, bytes)

      def :=(path: Path): Unit =
        copy(path, delegate, REPLACE_EXISTING)

      def :=[W: Writable](w: W): Unit =
        write(w)

      def write[W: Writable](writable: W): Unit =
        autoClosing(new BufferedOutputStream(new FileOutputStream(delegate.toFile)))(
          writable.writeToStream(_))

      /** Appends `string` encoded with UTF-8 to file. */
      def ++=(string: String): Unit =
        append(string)

      def append(string: String, encoding: Charset = UTF_8): Unit =
        write(string, encoding, append = true)

      def write(bytes: collection.Seq[Byte]): Unit =
        write(bytes.toArray)

      def write(string: String, encoding: Charset = UTF_8, append: Boolean = false): Unit =
        val options = Vector(CREATE, WRITE, if append then APPEND else TRUNCATE_EXISTING)
        Files.writeString(delegate, string, encoding, options*)

      def ++=[W: Writable](writable: W): Unit =
        autoClosing(new BufferedOutputStream(new FileOutputStream(delegate.toFile, true)))(
          writable.writeToStream(_))

      def labeledByteArray: Labeled[ByteArray] =
        // Do not publish the complete path. We use only the filename.
        Labeled(readAs[ByteArray], delegate.getFileName.toString)

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
        Files.readString(delegate, encoding)

      def writeUtf8Executable(string: String): Unit =
        write(string, UTF_8)
        makeExecutable()

      def useInputStream[A](body: InputStream => A): A =
        autoClosing(new BufferedInputStream(new FileInputStream(delegate.toFile)))(body)

      def inputStreamResource[F[_]](implicit F: Sync[F]): Resource[F, InputStream] =
        Resource.fromAutoCloseable:
          F.delay:
            new BufferedInputStream(new FileInputStream(delegate.toFile))

      def makeExecutable(): Unit =
        if isUnix then
          setPosixFilePermissions(delegate, PosixFilePermissions.fromString("r-x------"))

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

      //def toFs2Path: fs2.io.file.Path =
      //  fs2.io.file.Path.fromNioPath(delegate)

  import syntax.*

  @tailrec
  def createShortNamedDirectory(directory: Path, prefix: String): Path =
    try Files.createDirectory(directory / (prefix + newRandomFilenameString()))
    catch
      case _: FileAlreadyExistsException => createShortNamedDirectory(directory, prefix)

  private val FilenameCharacterSet = "abcdefghijklmnopqrstuvwxyz0123456789"
  private[io] val ShortNameSize = 6  // Results in 36**6 == 2176782336 permutations
  private[io] val ShortNamePermutationCount = List.fill(ShortNameSize)(FilenameCharacterSet.toSet.size.toLong).product

  private def newRandomFilenameString() =
    (Iterator.fill(ShortNameSize) { ThreadLocalRandom.current.nextInt(FilenameCharacterSet.length) } map FilenameCharacterSet).mkString

  def withTemporaryFile[A](body: Path => A): A = withTemporaryFile(prefix = "", suffix = ".tmp")(body)

  def withTemporaryFile[A](prefix: String, suffix: String, attributes: FileAttribute[?]*)(body: Path => A): A =
    autoDeleting(Files.createTempFile(prefix, suffix, attributes*))(body)

  def temporaryFileResource[F[_]](
    prefix: String = "",
    suffix: String = ".tmp",
    attributes: FileAttribute[?]*)
    (using F: Sync[F])
  : Resource[F, Path] =
    Resource.make(
      acquire = F.delay:
        Files.createTempFile(prefix, suffix, attributes*))(
      release = file => F.delay:
        Files.deleteIfExists(file))

  def temporaryFileResource(
    directory: Path,
    prefix: String,
    suffix: String,
    attributes: FileAttribute[?]*)
  : Resource[IO, Path] =
    Resource.make(
      acquire = IO:
        Files.createTempFile(directory, prefix, suffix, attributes*))(
      release = file =>
        IO.interruptible:
          FileDeleter.tryDeleteFile(file))

  def autoDeleting[A](file: Path)(body: Path => A): A =
    withCloser: closer =>
      closer.onClose:
        deleteIfExists(file)
      body(file)

  def withTemporaryDirectory[A](prefix: String = "")(body: Path => A): A =
    val dir = Files.createTempDirectory(prefix)
    withCloser: closer =>
      closer.onClose:
        deleteDirectoryRecursively(dir)
      body(dir)

  def temporaryDirectoryResource[F[_]](prefix: String)(using F: Sync[F]): Resource[F, Path] =
    Resource.make(
      acquire = F.delay(Files.createTempDirectory(prefix)))(
      release = dir => F.delay(deleteDirectoryRecursively(dir)))

  def provideFile[F[_]](file: Path)(implicit F: Sync[F]): Resource[F, Path] =
    Resource.make(
      acquire = F.delay:
        deleteIfExists(file)
        file)(
      release = _ => F.delay:
        try deleteIfExists(file)
        catch case t: IOException => logger.error(s"Delete $file => $t"))

  def deleteDirectoryRecursively(dir: Path): Unit =
    if !isDirectory(dir) then throw new IOException(s"Not a directory: $dir")
    if !isSymbolicLink(dir) then
      deleteDirectoryContentRecursively(dir)
    loggingDelete(dir)

  def deleteDirectoryContentRecursively(dir: Path): Unit =
    for f <- dir.directoryContents do
      if isDirectory(f) && !isSymbolicLink(f) then deleteDirectoryContentRecursively(f)
      loggingDelete(f)

  def tryDeleteDirectoryContentRecursively(dir: Path): Unit =
    for f <- dir.directoryContents do
      if isDirectory(f) && !isSymbolicLink(f) then deleteDirectoryContentRecursively(f)
      try loggingDelete(f)
      catch case NonFatal(t) =>
        logger.warn(s"Delete $f => ${t.toStringWithCauses}")

  private def loggingDelete(file: Path): Unit =
    delete(file)
    logger.trace(s"Deleted file $file")

  def nestedPathsIterator(directory: Path, options: FileVisitOption*): AutoCloseable & Iterator[Path] =
    new AbstractIterator[Path] with AutoCloseable:
      private val javaStream = Files.walk(directory, options*)
      private val underlyingIterator = javaStream.asScala

      def close(): Unit = javaStream.close()
      def hasNext = underlyingIterator.hasNext
      def next() = underlyingIterator.next()

  private val RelativePathRegex = """^(/|\\|..?/|..?\\)|([/\\]..?([/\\]|$))""".r.unanchored

  def checkRelativePath(path: String): Checked[String] =
    if path.isEmpty then
      Left(Problem("Relative file path must not be empty"))
    else if RelativePathRegex.pattern.matcher(path).find() then
      Left(Problem(s"Not a valid relative file path: $path"))
    else
      Right(path)
