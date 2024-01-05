package js7.common.files

import cats.effect.IO
import fs2.{Chunk, Stream}
import java.nio.file.{Files, Path}
import js7.base.data.ByteArray
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import scala.concurrent.duration.FiniteDuration

object GrowingFileStream:

  /** Reads Chunks of bytes from the endlessly growing file.
   *
   * Iff pollDuration is None, then terminate Stream at end of file. */
  def growingFileStream(file: Path, pollDuration: Option[FiniteDuration] = None): Stream[IO, Byte] =
    Stream
      .fromAutoCloseable(IO.interruptible:
        new ChunkFileReader[Chunk[Byte]](file, fromEnd = pollDuration.isDefined))
      .flatMap: reader =>
        Stream
          .eval(IO.interruptible:
            reader.read())
          .evalMap: chunk =>
            if chunk.isEmpty then
              pollDuration match
                case None => IO.pure(null) // End of Stream
                case Some(d) => IO.sleep(d).as(Chunk.empty) // Ignore and try again
            else
              IO.pure(chunk)
          .takeWhile(_ ne null)
          .filter(_.nonEmpty)
          //?.prefetch
          .unchunks
