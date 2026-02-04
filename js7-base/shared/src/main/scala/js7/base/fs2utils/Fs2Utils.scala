package js7.base.fs2utils

import cats.effect.IO
import cats.syntax.monoid.*
import fs2.{Chunk, Pipe}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*

object Fs2Utils:

  type StreamIO[A] = fs2.Stream[IO, A]
  type StreamPure[A] = fs2.Stream[fs2.Pure, A]

  def bytesToLines: Pipe[IO, Byte, String] =
    _.through(fs2.text.utf8.decode)
      .through(fs2.text.lines)

  private object End
  private val EndStream = fs2.Stream.emit(End)

  def combineByteSeqs[F[_], ByteSeq](limit: Int)(using ByteSeq: ByteSequence[ByteSeq])
  : fs2.Pipe[F, ByteSeq, ByteSeq] =
    _.append(EndStream)
      .scan((ByteSeq.newBuilder(limit), 0, Chunk.empty[ByteSeq])):
        case ((builder, 0, _), End) =>
          (builder.clear(), 0, Chunk.empty)

        case ((builder, _, _), End) =>
          val chunk = Chunk.singleton(builder.result())
          (builder.clear(), 0, chunk)

        case ((builder, builderLength, _), byteSeq: ByteSeq @unchecked) =>
          if byteSeq.isEmpty then
            (builder, builderLength, Chunk.empty)
          else
            val sum = builderLength + byteSeq.length
            if sum <= limit then
              (builder.append(byteSeq), sum, Chunk.empty)
            else if byteSeq.length > limit then
              val chunk =
                if builderLength == 0 then
                  Chunk.singleton(byteSeq)
                else
                  Chunk(builder.result(), byteSeq)
              (builder.clear(), 0, chunk)
            else
              val chunk = Chunk.singleton(builder.result())
              (builder.clear().append(byteSeq), byteSeq.length, chunk)
      .collect:
        case (builder, _, chunk) if chunk.nonEmpty => chunk
        //case x => Chunk.empty
      .unchunks
