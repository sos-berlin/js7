package js7.launcher.process

import cats.effect.{IO, Outcome}
import fs2.concurrent.Channel
import java.io.InputStream
import java.nio.charset.Charset
import js7.base.io.process.StdoutOrStderr
import js7.base.log.Logger

object CopyInputStreamToStringChannel:

  private val logger = Logger[this.type]

  def copyInputStreamToStringChannel(
    outerr: StdoutOrStderr,
    in: InputStream,
    channel: Channel[IO, Either[Throwable, String]],
    encoding: Charset,
    charBufferSize: Int)
  : IO[Unit] =
      fs2.io
        .readInputStream(IO.pure(in), chunkSize = charBufferSize/*byte???*/, closeAfterUse = false)
        .chunks
        .through(fs2.text.decodeCWithCharset(encoding))
        .foreach: chunk =>
          channel.send(Right(chunk)).flatMap:
            case Left(Channel.Closed) =>
              IO.raiseError(new IllegalStateException(s"$outerr FS2 Channel closed"))
            case Right(()) => IO.unit
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
