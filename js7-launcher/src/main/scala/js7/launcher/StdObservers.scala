package js7.launcher

import cats.effect.kernel.Resource.ExitCase
import cats.effect.{IO, Resource, ResourceIO}
import fs2.concurrent.Channel
import fs2.{Chunk, Pipe, Stream}
import java.io.InputStream
import java.nio.charset.Charset
import js7.base.catsutils.CatsEffectExtensions.{joinStd, startAndForget}
import js7.base.io.ReaderStreams.inputStreamToByteStream
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.thread.IOExecutor
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
  chunkSize: Int,
  charBufferSize: Int,
  delay: FiniteDuration,
  useErrorLineLengthMax: Option[Int],
  name: String):

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

  private def pumpChannelsToSinkResource: Resource[IO, Unit] =
    Resource
      .make(
        acquire = pumpChannelsToSink.start)(
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

  def pumpInputStreamToSink(outErr: StdoutOrStderr, in: InputStream, encoding: Charset)
    (using IOExecutor)
  : IO[Unit] =
    pumpToSink(outErr):
      inputStreamAsStream(outErr, in, encoding)

  private def inputStreamAsStream(outErr: StdoutOrStderr, in: InputStream, encoding: Charset)
    (using IOExecutor)
  : Stream[IO, String] =
    // Uninterruptible when waiting for next read (does it matter, do we cancel this?)
    //fs2.io
    //  .readInputStream(IO.pure(in), chunkSize = charBufferSize /*byte???*/)
    inputStreamToByteStream(in, bufferSize = charBufferSize/*TODO used for bytes*/)
      .onFinalizeCase:
        case exitCase @ ExitCase.Canceled =>
          // FIXME When cancelling the stream, io.blocking happens to block itself.
          //  It does not execute its body and instead waits forever. Why?
          //  ShellScriptProcess inhibits cancellation and instead waits (forever) for
          //  stream termination.
          IO.blocking(())
            .logWhenItTakesLonger:
              s"### $name $outErr $exitCase   🔥🔥🔥 IO.blocking(()) is blocking itself 🔥🔥🔥"
            .startAndForget *>
              IO.whenA(false): // Better, we don't close the file
                IO.blocking:
                  logger.trace(s"### $name $outErr $exitCase in.close!")
                  // Close may hang after sigkill ?
                  in.close()
                .logWhenItTakesLonger(s"$name $outErr.close() after cancellation")
        case _ =>
          IO
            .blocking:
              in.close()
            .logWhenItTakesLonger(s"$outErr close after cancellation") // Just in case
      .chunks
      .through(fs2.text.decodeCWithCharset(encoding))
      // Waits for LF, bad when LF is delayed: .through(fs2.text.lines)

  private def pumpToSink(outErr: StdoutOrStderr)(stream: Stream[IO, String]): IO[Unit] =
    outErrToSink(outErr)(
      stream
        //.evalTap(string => IO(logger.info(s"### string =${ByteArray(string)}")))
        // TODO .grouped tears surrogate pairs apart!
        .map(string => Chunk.iterator(string.grouped(chunkSize))) // SLOW ???
        .unchunks
        //.evalTap(string => IO(logger.info(s"### slice  =${ByteArray(string)}")))
        .groupWithin(chunkSize, delay)
        .map(chunk => chunk.toArray.mkString)
        //.evalTap(string => IO(logger.info(s"### grouped=${ByteArray(string)}")))
        .pipeIf(outErr == Stderr):
          _.through:
            lastLineKeeper getOrElse identity
    ).compile.drain


object StdObservers:

  type OutErrToSink = StdoutOrStderr => Pipe[IO, String, Nothing]

  private val logger = Logger[this.type]

  def resource(
    outErrToSink: OutErrToSink,
    charBufferSize: Int,
    chunkSize: Int,
    delay: FiniteDuration,
    queueSize: Int = 0,
    useErrorLineLengthMax: Option[Int] = None,
    name: String)
  : ResourceIO[StdObservers] =
    for
      stdObservers <- Resource.eval:
        for
          outChannel <- Channel.bounded[IO, String](capacity = queueSize)
          errChannel <- Channel.bounded[IO, String](capacity = queueSize)
        yield
          StdObservers(outErrToSink, outChannel, errChannel,
            charBufferSize, chunkSize, delay,
            useErrorLineLengthMax, name)
      _ <- stdObservers.pumpChannelsToSinkResource
    yield
      stdObservers
