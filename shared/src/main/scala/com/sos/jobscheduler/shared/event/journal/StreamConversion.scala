package com.sos.jobscheduler.shared.event.journal

import java.io.{EOFException, InputStream, OutputStream}

/**
  * @author Joacim Zschimmer
  */
trait StreamConversion {

  def convertOutputStream(out: OutputStream): OutputStream =
    out

  def convertInputStream(in: InputStream): InputStream =
    in

  def isIncompleteException(exception: Exception): Boolean =
    exception.isInstanceOf[EOFException]

  def isCorruptException(exception: Exception): Boolean =
    false
}
