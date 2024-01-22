package js7.launcher.process

import cats.effect.{IO, Outcome}
import cats.syntax.flatMap.*
import fs2.Stream
import fs2.concurrent.Channel
import fs2.io.file.{Files, Flags, Path}
import java.io.{InputStream, InputStreamReader, Reader}
import java.nio.charset.Charset
import java.nio.file.Path as JPath
import js7.base.log.Logger
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.launcher.StdWriter
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object InputStreamToStream:

  private val logger = Logger[this.type]

  def copyInputStreamToStream(
    in: InputStream,
    channel: Channel[IO, Either[Throwable, String]],
    encoding: Charset,
    charBufferSize: Int)
  : IO[Unit] =
      fs2.io
        .readInputStream(IO.pure(in), chunkSize = charBufferSize/*byte???*/, closeAfterUse = false)
        .chunks
        .through(fs2.text.decodeCWithCharset(encoding))
        .evalTap: chunk =>
          channel.send(Right(chunk)).void
        .compile
        .drain
        .guaranteeCase:
          case Outcome.Succeeded(_) =>
            channel.close.void

          case Outcome.Errored(t) =>
            channel.closeWithElement(Left(t)).void

          case Outcome.Canceled() =>
            logger.debug("Canceled")
            channel.close.void
