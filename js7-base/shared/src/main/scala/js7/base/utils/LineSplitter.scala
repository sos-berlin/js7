package js7.base.utils

import cats.syntax.monoid.*
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteArray, ByteSequence}
import scala.collection.mutable

/**
  * Splits a stream of UTF-8-encoded ByteSeqs into lines separated by LF.
  */
final class LineSplitter[ByteSeq](using ByteSeq: ByteSequence[ByteSeq])
extends (ByteSeq => Vector[ByteArray]):

  private val lines = Vector.newBuilder[ByteArray]
  private lazy val startedLine = mutable.ArrayBuffer.empty[ByteSeq]

  def apply(byteSeq: ByteSeq): Vector[ByteArray] =
    if byteSeq.isEmpty then
      Vector.empty
    else
      var p = 0
      val length = byteSeq.length

      while p < length do
        byteSeq.indexOf('\n'.toByte, p) match
          case -1 =>
            startedLine += byteSeq.slice(p, length)
            p = length

          case i =>
            // For p == 0 && i + 1 == bytes.length, no copy is needed
            val slice = byteSeq.slice(p, i + 1)
            startedLine += slice
            lines += ByteArray.combineByteSequences(startedLine)
            startedLine.clear()
            p = i + 1

      val result = lines.result()
      lines.clear()
      result
