package com.sos.scheduler.engine.common.scalautil

import com.google.common.base.Charsets.UTF_8
import com.google.common.io.Files
import java.io.File
import java.nio.charset.Charset
import java.nio.file.Path
import scala.language.implicitConversions

object FileUtils {
  object implicits {
    implicit def pathToFile(o: Path): File = o.toFile

    implicit def fileToPath(o: File): Path = o.toPath

    implicit class RichFile(val delegate: File) extends AnyVal {

      def /(filename: String) = new File(delegate, filename)

      def contentBytes: Array[Byte] = Files.toByteArray(delegate)

      def contentBytes_=(o: Array[Byte]): Unit = Files.write(o, delegate)

      def contentString: String = contentString(UTF_8)

      def contentString_=(o: String): Unit = write(o, UTF_8)

      def contentString(encoding: Charset) = Files.toString(delegate, encoding)

      def write(string: String, encoding: Charset = UTF_8): Unit =  Files.write(string, delegate, encoding)

      def append(o: String, encoding: Charset = UTF_8): Unit = Files.append(o, delegate, encoding)
    }
  }
}
