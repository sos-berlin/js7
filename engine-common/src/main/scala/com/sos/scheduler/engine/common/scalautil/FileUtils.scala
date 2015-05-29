package com.sos.scheduler.engine.common.scalautil

import com.google.common.base.Charsets.UTF_8
import com.google.common.io.{Closer, Files ⇒ GuavaFiles}
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import java.io.File
import java.nio.charset.Charset
import java.nio.file.{Files, Path}
import scala.language.implicitConversions

object FileUtils {

  /**
   * Touchs the file and deletes it when closer is closed.
   * Used mainly by tests.
   */
  def touchAndDeleteWithCloser[A <: Path](path: A)(implicit closer: Closer): path.type = {
    GuavaFiles.touch(path.toFile)
    path withCloser Files.delete
    path
  }

  object implicits {
    implicit def pathToFile(path: Path): File = path.toFile

    implicit def fileToPath(file: File): Path = file.toPath

    implicit class RichPath(val delegate: Path) extends AnyVal {

      def /(filename: String): Path = delegate resolve filename

      def contentBytes: Array[Byte] = file.contentBytes

      def contentBytes_=(o: Array[Byte]): Unit = file.contentBytes = o

      def contentString: String = file.contentString

      def contentString_=(o: String): Unit = file.contentString = o

      def contentString(encoding: Charset) = file.contentString(encoding)

      def write(string: String, encoding: Charset = UTF_8): Unit =  file.write(string, encoding)

      def append(o: String, encoding: Charset = UTF_8): Unit = file.append(o, encoding)

      private def file = delegate.toFile
    }

    implicit class RichFile(val delegate: File) extends AnyVal {

      def /(filename: String) = new File(delegate, filename)

      def contentBytes: Array[Byte] = GuavaFiles.toByteArray(delegate)

      def contentBytes_=(o: Array[Byte]): Unit = GuavaFiles.write(o, delegate)

      def contentString: String = contentString(UTF_8)

      def contentString_=(o: String): Unit = write(o, UTF_8)

      def contentString(encoding: Charset) = GuavaFiles.toString(delegate, encoding)

      def write(string: String, encoding: Charset = UTF_8): Unit =  GuavaFiles.write(string, delegate, encoding)

      def append(o: String, encoding: Charset = UTF_8): Unit = GuavaFiles.append(o, delegate, encoding)
    }
  }
}
