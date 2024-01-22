package js7.launcher

import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.utils.LastLineKeeper
import cats.effect.IO
import cats.effect.Resource
import fs2.Stream
import fs2.concurrent.Channel
import js7.base.catsutils.UnsafeMemoizable.given
import js7.base.log.Logger
import js7.launcher.StdObservers.*

/** Provides a process' stdout and stdin as streams. */
final class StdObservers(
  private[launcher] val outChannel: Channel[IO, Elem],
  private[launcher] val errChannel: Channel[IO, Elem],
  val charBufferSize: Int,
  keepLastErrLine: Boolean):

  private val lastLineKeeper = keepLastErrLine ? new LastLineKeeper

  val out: StdWriter = StdWriter(outChannel)
  val err: StdWriter = StdWriter(errChannel)

  private[js7] val outStream: Stream[IO, String] =
    outChannel.stream.rethrow

  private[js7] val errStream: Stream[IO, String] =
    lastLineKeeper.fold(errChannel.stream.rethrow)(o => errChannel.stream.rethrow.through(o))

  private[js7] def errorLine: Option[String] =
    lastLineKeeper.flatMap(_.lastLine)

  def write(outerr: StdoutOrStderr, string: String): IO[Unit] =
    writer(outerr).write(string).void

  def writer(outerr: StdoutOrStderr): StdWriter =
    outerr match
      case Stdout => out
      case Stderr => err

  def error(outerr: StdoutOrStderr, throwable: Throwable): IO[Unit] =
    channel(outerr).closeWithElement(Left(throwable)).map:
      case Left(Channel.Closed) =>
        IO(logger.warn(s"After $outerr Channel has been closed: ${throwable.toStringWithCauses}"))
      case Right(()) =>
        IO.unit

  private def channel(outerr: StdoutOrStderr): Channel[IO, Elem] =
    outerr match
      case Stdout => outChannel
      case Stderr => errChannel

  private[js7] val close: IO[Unit] =
    IO.both(outChannel.close, errChannel.close)
      .as(())
      .unsafeMemoize


object StdObservers:

  private type Elem = Either[Throwable, String]

  private val logger = Logger[this.type]

  def resource(charBufferSize: Int, keepLastErrLine: Boolean): Resource[IO, StdObservers] =
    Resource.make(
      acquire =
        for
          outChannel <- Channel.bounded[IO, Elem](capacity = 1)
          errChannel <- Channel.bounded[IO, Elem](capacity = 1)
        yield
          new StdObservers(outChannel, errChannel, charBufferSize, keepLastErrLine))(
      release =
        _.close)
