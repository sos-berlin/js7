package js7.base.utils

import cats.effect.IO
import cats.syntax.monoid.*
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteArray, ByteSequence}
import fs2.Stream
import scala.collection.mutable

/**
  * Splits a stream of UTF-8-encoded ByteSeqs into lines, separated by LF.
  */
final class ByteSequenceToLinesStream[ByteSeq](
  using ByteSeq: ByteSequence[ByteSeq])
extends (ByteSeq => Stream[IO, ByteArray]):

  private val lines = Vector.newBuilder[ByteArray]
  private lazy val startedLine = mutable.ArrayBuffer.empty[ByteSeq]

  def apply(byteSeq: ByteSeq): Stream[IO, ByteArray] =
    if byteSeq.isEmpty then
      Stream.empty
    else
      var p = 0
      val length = byteSeq.length

      while p < length do
        byteSeq.indexOf('\n'.toByte, p) match
          case -1 =>
            startedLine += byteSeq.slice(p, length)
            p = length

          case i =>
            val slice = byteSeq.slice(p, i + 1)  /*For p == 0 && i + 1 == bytes.length, no copy is needed*/
            startedLine += slice
            lines += ByteArray.combineByteSequences(startedLine)
            startedLine.clear()
            p = i + 1

      val result = Stream.emits(lines.result())
      lines.clear()
      result


//object ByteSequenceToLinesStream:
//
//  extension [F[_], I: ByteSequence](stream: Stream[F, I])
//    def linesTo[O: ByteSequence]: Stream[F, O] =
//      stream.through(new ByteSequenceToLinesStream[F, I, O]()) .................
