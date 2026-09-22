package js7.launcher

import cats.effect.{IO, Resource, ResourceIO}
import fs2.concurrent.Channel
import fs2.{Chunk, Stream}
import java.io.InputStream
import java.nio.charset.Charset
import js7.base.catsutils.CatsEffectExtensions.{joinStd, startAndLogError}
import js7.base.fs2utils.StreamExtensions.{chunkWithin, convertToString, fromString}
import js7.base.io.ReaderStreams.inputStreamToByteStream
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.utils.AtomicStopper
import js7.base.utils.CatsUtils.syntax.{RichResource, logWhenItTakesLonger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.StdObservers.*
import js7.launcher.utils.LastLineKeeper
import scala.concurrent.duration.FiniteDuration

/** Provides a process' stdout and stdin as streams.
 *
 * Channels are only used by internal jobs.
 * A system process uses pumpInputStreamToSink instead.
 */
final class StdObservers private(
  outErrToSink: OutErrToSink,
  outChannel: Channel[IO, String],
  errChannel: Channel[IO, String],
  byteBufferSize: Int,
  chunkSize: Int,
  delay: FiniteDuration,
  useErrorLineLengthMax: Option[Int],
  val maxWaitForStdouterr: Option[FiniteDuration],
  val stdouterrStopper: AtomicStopper,
  label: String):

  private val lastLineKeeper = useErrorLineLengthMax.map(LastLineKeeper(_))

  val out: StdWriter = StdWriter(outChannel)
  val err: StdWriter = StdWriter(errChannel)

  private[js7] def errorLine: Option[String] =
    lastLineKeeper.flatMap(_.lastLine)

  def writer(outerr: StdoutOrStderr): StdWriter =
    outerr match
      case Stdout => out
      case Stderr => err

  private def channel(outerr: StdoutOrStderr): Channel[IO, String] =
    outerr match
      case Stdout => outChannel
      case Stderr => errChannel

  val closeChannels: IO[Unit] =
     IO.both(outChannel.close, errChannel.close).void

  private def pumpChannelsToSinkResource: ResourceIO[Unit] =
    Resource
      .make(
        acquire = pumpChannelsToSink.startAndLogError)(
        release = fiber => closeChannels *> fiber.joinStd)
      .void

  private def pumpChannelsToSink: IO[Unit] =
    IO
      .both(
        pumpChannelToSink(Stdout),
        pumpChannelToSink(Stderr))
      .void

  private def pumpChannelToSink(outErr: StdoutOrStderr): IO[Unit] =
    pumpToSink(outErr):
      channel(outErr).stream

  def pumpInputStreamToSink(outErr: StdoutOrStderr, in: InputStream, encoding: Charset): IO[Unit] =
    pumpToSink(outErr):
      inputStreamAsStream(outErr, in, encoding)

  private def inputStreamAsStream(outErr: StdoutOrStderr, in: InputStream, encoding: Charset)
  : Stream[IO, String] =
    inputStreamToByteStream(in, bufferSize = byteBufferSize, label = s"$label $outErr")
      .onFinalizeCase: exitCase =>
        IO.blocking:
          in.close()
        .logWhenItTakesLonger(s"$label closing $outErr")
      .through:
        fs2.text.decodeWithCharset(encoding)

  private def pumpToSink(outErr: StdoutOrStderr)(stream: Stream[IO, String]): IO[Unit] =
    outErrToSink(outErr, stdouterrStopper):
      stream
        .pipeIf(outErr == Stderr):
          _.through(lastLineKeeper getOrElse identity)
        .map(Chunk.fromString)
        .unchunks
        // TODO Don't cut through surrogates: 🌈
        .chunkWithin(chunkSize, delay)
        .map(_.convertToString)


object StdObservers:

  type OutErrToSink = (StdoutOrStderr, AtomicStopper) => Stream[IO, String] => IO[Unit]

  private val logger = Logger[this.type]

  def resource(
    outErrToSink: OutErrToSink,
    byteBufferSize: Int,
    chunkSize: Int,
    delay: FiniteDuration,
    queueSize: Int = 0,
    maxWaitForStdouterr: Option[FiniteDuration],
    useErrorLineLengthMax: Option[Int] = None,
    label: String)
  : ResourceIO[StdObservers] =
    for
      stdouterrStopper <- Resource.eval(AtomicStopper(label = s"$label stdout/stderr"))
      stdObservers <- Resource.eval:
        for
          outChannel <- Channel.bounded[IO, String](capacity = queueSize)
          errChannel <- Channel.bounded[IO, String](capacity = queueSize)
        yield
          StdObservers(outErrToSink, outChannel, errChannel,
            byteBufferSize = byteBufferSize,
            chunkSize = chunkSize, delay,
            useErrorLineLengthMax,
            maxWaitForStdouterr = maxWaitForStdouterr,
            stdouterrStopper,
            label)
      _ <- stdObservers.pumpChannelsToSinkResource
    yield
      stdObservers
