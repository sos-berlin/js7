package com.sos.jobscheduler.common.scalautil

import akka.util.ByteString
import com.google.common.base.Charsets.UTF_8
import com.google.common.io.FileWriteMode.APPEND
import com.google.common.io.{Closer, Files ⇒ GuavaFiles}
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import java.io.File
import java.nio.charset.Charset
import java.nio.file.Files.{delete, isSymbolicLink}
import java.nio.file.attribute.FileAttribute
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

    implicit final class RichPath(private val delegate: Path) extends AnyVal {

      def /(filename: String): Path = delegate resolve filename

      def byteString: ByteString = file.byteString

      def contentBytes: Array[Byte] = file.contentBytes

      def contentBytes_=(o: Array[Byte]): Unit = file.contentBytes = o

      def contentString: String = file.contentString

      def contentString_=(o: String): Unit = file.contentString = o

      def contentString(encoding: Charset) = file.contentString(encoding)

      def write(string: String, encoding: Charset = UTF_8): Unit =  file.write(string, encoding)

      def append(o: String, encoding: Charset = UTF_8): Unit = file.append(o, encoding)

      private def file = delegate.toFile

      /**
        * Returns the content of the directory denoted by `this`.
        */
      def pathSet: Set[Path] = autoClosing(Files.list(delegate)) { _.toSet }
    }

    implicit final class RichFile(private val delegate: File) extends AnyVal {

      def /(filename: String) = new File(delegate, filename)

      def byteString: ByteString = ByteString(delegate.contentBytes)

      def contentBytes: Array[Byte] = GuavaFiles.toByteArray(delegate)

      def contentBytes_=(o: Array[Byte]): Unit = GuavaFiles.write(o, delegate)

      def contentString: String = contentString(UTF_8)

      def contentString_=(o: String): Unit = write(o, UTF_8)

      def contentString(encoding: Charset) = GuavaFiles.asCharSource(delegate, encoding).read()

      def write(string: String, encoding: Charset = UTF_8): Unit =  GuavaFiles.asCharSink(delegate, encoding).write(string)

      def append(string: String, encoding: Charset = UTF_8): Unit = GuavaFiles.asCharSink(delegate, encoding, APPEND).write(string)
    }
  }

  import implicits._

  @tailrec
  def createShortNamedDirectory(directory: Path, prefix: String): Path = {
    try Files.createDirectory(directory / (prefix + newRandomFilenameString()))
    catch {
      case _: FileAlreadyExistsException ⇒ createShortNamedDirectory(directory, prefix)
    }
  }

  private val FilenameCharacterSet = "abcdefghijklmnopqrstuvwxyz0123456789"
  private[scalautil] val ShortNameSize = 6  // Results in 36**6 == 2176782336 permutations
  private[scalautil] val ShortNamePermutationCount = List.fill(ShortNameSize)(FilenameCharacterSet.toSet.size.toLong).product

  private def newRandomFilenameString() =
    (Iterator.fill(ShortNameSize) { ThreadLocalRandom.current.nextInt(FilenameCharacterSet.length) } map FilenameCharacterSet).mkString

  def withTemporaryFile[A](body: Path ⇒ A): A = withTemporaryFile(prefix = "", suffix = ".tmp")(body)

  def withTemporaryFile[A](prefix: String, suffix: String, attributes: FileAttribute[_]*)(body: Path ⇒ A): A =
    autoDeleting(Files.createTempFile(prefix, suffix, attributes: _*))(body)


  def autoDeleting[A](file: Path)(body: Path ⇒ A): A =
    withCloser { closer ⇒
      closer.onClose {
        delete(file)
      }
      body(file)
  }

  def withTemporaryDirectory[A](prefix: String = "")(body: Path ⇒ A): A = {
    val dir = Files.createTempDirectory(prefix)
    withCloser { closer ⇒
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
    for (f ← dir.pathSet) {
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
}
