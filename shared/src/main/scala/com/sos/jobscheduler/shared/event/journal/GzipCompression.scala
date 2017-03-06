package com.sos.jobscheduler.shared.event.journal

import java.io.{InputStream, OutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/**
  * @author Joacim Zschimmer
  */
trait GzipCompression extends StreamConversion {

  override def convertOutputStream(out: OutputStream) =
    new GZIPOutputStream(out, /*syncFlush=*/true)

  override def convertInputStream(in: InputStream) =
    new GZIPInputStream(in)

  //override def isCorruptException(exception: Exception): Boolean =
  //  exception.isInstanceOf[java.util.zip.ZipException] || super.isCorruptException(exception)
}
