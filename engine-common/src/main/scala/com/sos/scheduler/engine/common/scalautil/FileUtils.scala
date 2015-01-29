package com.sos.scheduler.engine.common.scalautil

import com.google.common.base.Charsets.UTF_8
import com.google.common.io.{Files ⇒ GuavaFiles}
import java.io.File
import java.nio.charset.Charset
import java.nio.file.Path
import scala.language.implicitConversions

object FileUtils {
  object implicits {
    implicit def pathToFile(path: Path): File = path.toFile

    implicit def fileToPath(file: File): Path = file.toPath

    implicit class RichPath(val delegate: Path) extends AnyVal {

      def /(filename: String): Path = new File(delegate, filename).toFile

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
