package js7.common.files

import cats.effect.IO
import fs2.{Chunk, Stream}
import java.nio.file.{Files, Path}
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import scala.concurrent.duration.FiniteDuration

object GrowingFileStream:

  /** Reads Chunks of bytes from the endlessly growing file.
   *
   * Iff pollDuration is None, then terminate Stream at end of file. */
  def growingFileStream(
    file: Path,
    pollDuration: Option[FiniteDuration] = None,
    fromEnd: Boolean)
  : Stream[IO, Byte] =
    Stream
      .fromAutoCloseable(IO.interruptible:
        new ByteSeqFileReader[Chunk[Byte]](file, fromEnd = fromEnd))
      .flatMap: reader =>
        Stream
          .repeatEval(IO.interruptible:
            reader.read())
          .evalMap: chunk =>
            if chunk.nonEmpty then
              IO.pure(chunk)
            else // eof
              pollDuration match
                case None => IO.pure(null/*end of stream*/)
                case Some(d) =>
                  if Files.exists(file) then
                    IO.sleep(d).as(Chunk.empty) // Delay and try again
                  else
                    IO.pure(null/*end of stream*/)
          .takeWhile(_ ne null)
          .filter(_.nonEmpty)
          .unchunks
