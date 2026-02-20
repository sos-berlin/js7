package js7.common.pekkohttp

import cats.effect.IO
import fs2.{Chunk, Stream}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.utils.ScalaUtils.syntax.*

object ByteSequenceStreamExtensions:

  extension [ByteSeq: ByteSequence, O](stream: Stream[IO, ByteSeq])

    def splitByteSequences(maxSize: Int)
    : Stream[IO, ByteSeq] =
      stream.mapChunks: chunk =>
        chunk.flatMap: byteSeq =>
          if byteSeq.length <= maxSize then
            Chunk.singleton(byteSeq)
          else
            Chunk.from:
              Vector.unfold(0): i =>
                val slice = byteSeq.slice(i * maxSize, (i + 1) * maxSize)
                slice.nonEmpty ? (slice, i + 1)
