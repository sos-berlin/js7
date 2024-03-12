package js7.common.pekkohttp

import cats.effect.IO
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import fs2.Stream
import js7.base.utils.ScalaUtils.syntax.*

object ByteSequenceStreamExtensions:

  extension [O](stream: Stream[IO, O])

    def splitByteSequences[ByteSeq: ByteSequence](maxSize: Int)(using ByteSeq =:= O)
    : Stream[IO, ByteSeq] =
      stream.asInstanceOf[Stream[IO, ByteSeq]]
        .flatMap: byteSeq =>
          if byteSeq.length <= maxSize then
            Stream.emit(byteSeq)
          else
            Stream.unfold(0): i =>
              val slice = byteSeq.slice(i * maxSize, (i + 1) * maxSize)
              slice.nonEmpty ? (slice, i + 1)
