package com.sos.scheduler.engine.common.io

import java.io.InputStream
import scala.sys._

// NOT IN USE, NO TEST
private final class InputStreamIterator(in: InputStream) extends Iterator[Byte] {
  private var nextByte: Int = -2

  def hasNext =
    provideNextByte() != -1

  def next() = {
    val result = provideNextByte()
    if (result < 0)  error("End of InputStream")
    nextByte = -2
    result.toByte
  }

  @inline private def provideNextByte() = {
    if (nextByte == -2)
      nextByte = in.read()
    nextByte
  }
}
