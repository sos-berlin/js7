package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.shared.event.journal.GzipCompression._
import java.io.{FileInputStream, InputStream, OutputStream}
import java.nio.file.Path
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/**
  * @author Joacim Zschimmer
  */
trait GzipCompression extends StreamConversion {

  def compressWithGzip = true

  override def convertOutputStream(out: OutputStream) =
    if (compressWithGzip)
      new GZIPOutputStream(out, /*syncFlush=*/true)
    else
      super.convertOutputStream(out)

  override def convertInputStream(in: InputStream, path: Path) = {
    if (isGzipCompressed(path))
      new GZIPInputStream(in)
    else
      super.convertInputStream(in, path)
  }

  //override def isCorruptException(exception: Exception): Boolean =
  //  exception.isInstanceOf[java.util.zip.ZipException] || super.isCorruptException(exception)
}

object GzipCompression {
  private val GzipMagicNumber = Vector[Byte](0x1F.toByte, 0x8B.toByte, 0x08.toByte)

  private[journal] def isGzipCompressed(file: Path): Boolean =
    autoClosing(new FileInputStream(file)) { in ⇒
      val header = new Array[Byte](3)
      in.read(header) == GzipMagicNumber.size && header.sameElements(GzipMagicNumber)
    }
}
