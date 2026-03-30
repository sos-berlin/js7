package js7.base.io

import java.io.{FilterOutputStream, OutputStream}

final class CountingOutputStream(out: OutputStream, suppressClose: Boolean = false)
extends FilterOutputStream(out):

  private var _count = 0L

  inline def byteCount: Long =
    _count

  override def write(b: Int): Unit =
    _count += 1
    out.write(b)

  override def write(array: Array[Byte]): Unit =
    _count += array.length
    out.write(array)

  override def write(array: Array[Byte], offset: Int, length: Int): Unit =
    _count += length
    out.write(array, offset, length)

  override def close() =
    if !suppressClose then
      out.close()
