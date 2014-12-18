package com.sos.scheduler.engine.common.io

import java.io.Reader

final class ReaderIterator(reader: Reader) extends Iterator[Char] {
  private var nextChar: Int = -2

  def hasNext = provideNextByte() != -1

  def next() = {
    val result = provideNextByte()
    if (result < 0) throw new NoSuchElementException("End of Reader")
    nextChar = -2
    result.toChar
  }

  @inline private def provideNextByte() = {
    if (nextChar == -2)
      nextChar = reader.read()
    nextChar
  }
}
