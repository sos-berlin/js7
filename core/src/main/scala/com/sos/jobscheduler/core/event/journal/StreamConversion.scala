package com.sos.jobscheduler.core.event.journal

import java.io.{EOFException, InputStream, OutputStream}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
trait StreamConversion {

  def convertOutputStream(out: OutputStream): OutputStream =
    out

  /** Usable for GzipCompression.
    * @param path for file type detection only
    */
  def convertInputStream(in: InputStream, path: Path): InputStream =
    in

  def isIncompleteException(exception: Exception): Boolean =
    exception.isInstanceOf[EOFException]

  def isCorruptException(exception: Exception): Boolean =
    false
}
