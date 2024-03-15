package js7.common.pekkohttp

import cats.effect.IO
import fs2.{Chunk, Stream}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.utils.ScalaUtils.syntax.*

object ByteSequenceStreamExtensions:

  extension [O](stream: Stream[IO, O])

    def splitByteSequences[ByteSeq: ByteSequence](maxSize: Int)(using ByteSeq =:= O)
    : Stream[IO, ByteSeq] =
      stream.asInstanceOf[Stream[IO, ByteSeq]]
        .map: byteSeq =>
          if byteSeq.length <= maxSize then
            Chunk.singleton(byteSeq)
          else
            Chunk.from:
              Vector.unfold(0): i =>
                val slice = byteSeq.slice(i * maxSize, (i + 1) * maxSize)
                slice.nonEmpty ? (slice, i + 1)
        .unchunks
