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
final class StdObservers private(
  private[launcher] val outChannel: Channel[IO, Elem],
  private[launcher] val errChannel: Channel[IO, Elem],
  val charBufferSize: Int,
  useErrorLineLengthMax: Option[Int]):

  private val lastLineKeeper = useErrorLineLengthMax.map(LastLineKeeper(_))

  val out: StdWriter = StdWriter(outChannel)
  val err: StdWriter = StdWriter(errChannel)

  private[js7] val outStream: Stream[IO, String] =
    outChannel.stream.rethrow

  private[js7] val errStream: Stream[IO, String] =
    errChannel.stream.rethrow.through:
      lastLineKeeper getOrElse identity

  private[js7] def errorLine: Option[String] =
    lastLineKeeper.flatMap(_.lastLine)

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

  def resource(
    charBufferSize: Int,
    queueSize: Int = 1,
    useErrorLineLengthMax: Option[Int] = None)
  : Resource[IO, StdObservers] =
    Resource.make(
      acquire =
        for
          outChannel <- Channel.bounded[IO, Elem](capacity = queueSize)
          errChannel <- Channel.bounded[IO, Elem](capacity = queueSize)
        yield
          new StdObservers(outChannel, errChannel, charBufferSize, useErrorLineLengthMax))(
      release =
        _.close)
