package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.Strings.RichString
import java.nio.charset.CodingErrorAction.REPLACE
import java.nio.charset.StandardCharsets.UTF_8
import scodec.bits.ByteVector

object ScodecUtils
{
  implicit final class RichByteVector(private val underlying: ByteVector) extends AnyVal
  {
    def utf8StringTruncateAt(truncateAt: Int): String =  // TODO Truncate big ByteVector before decoding
      utf8String.truncateWithEllipsis(truncateAt)

    def utf8String: String =
      UTF_8.newDecoder()
        .onMalformedInput(REPLACE)
        .onUnmappableCharacter(REPLACE)
        .decode(underlying.toByteBuffer)
        .toString

    def indexOf(byte: Byte): Int =
      indexOf(byte, 0)

    def indexOf(byte: Byte, start: Int): Int =
      indexOf(byte, start.toLong).toInt

    def indexOf(byte: Byte, start: Long): Long = {
      var i = start
      val length = underlying.length
      while (i < length) {
        if (underlying.get(i) == byte) return i
        i += 1
      }
      -1
    }
  }
}
