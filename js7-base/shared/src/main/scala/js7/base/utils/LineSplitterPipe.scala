package js7.base.utils

import fs2.{Chunk, Pipe, Stream}
import js7.base.data.{ByteArray, ByteSequence}

/**
  * Splits a stream of UTF-8-encoded ByteSeqs into lines, separated by LF.
  */
final class LineSplitterPipe[F[_], ByteSeq](using ByteSeq: ByteSequence[ByteSeq])
extends Pipe[F, ByteSeq, ByteArray]:

  private val byteSeqToLines = LineSplitter[ByteSeq]

  def apply(stream: Stream[F, ByteSeq]): Stream[F, ByteArray] =
    stream.map: byteSeq =>
      Chunk.from(byteSeqToLines(byteSeq))
    .filter(_.nonEmpty)
    .unchunks
