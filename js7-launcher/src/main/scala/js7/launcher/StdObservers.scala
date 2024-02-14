package js7.launcher

import cats.effect.kernel.Resource.ExitCase
import cats.effect.{IO, Resource, ResourceIO}
import fs2.concurrent.Channel
import fs2.{Pipe, Stream}
import java.io.InputStream
import java.nio.charset.Charset
import js7.base.catsutils.CatsEffectExtensions.{defer, joinStd, startAndForget}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.fs2utils.StreamUtils
import js7.base.fs2utils.StreamUtils.inputStreamToByteStream
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.{RichResource, logWhenItTakesLonger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.StdObservers.*
import js7.launcher.utils.LastLineKeeper
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable
import scala.concurrent.duration.Deadline

/** Provides a process' stdout and stdin as streams.
 *
 * Channels are only used by internal jobs.
 * A system process uses pumpInputStreamToSink instead.
 */
final class StdObservers private(
  outErrToSink: OutErrToSink,
  outChannel: Channel[IO, String],
  errChannel: Channel[IO, String],
  @deprecated("Still used?")
  val charBufferSize: Int,
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
     IO.both(outChannel.close, errChannel.close)
      .as(())
      .unsafeMemoize

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

  def pumpInputStreamToSink(outErr: StdoutOrStderr, in: InputStream, encoding: Charset): IO[Unit] =
    pumpToSink(outErr):
      inputStreamAsStream(outErr, in, encoding)
    //val io = pumpToSink(outErr):
    //  inputStreamAsStream(outErr, in, encoding)
    //val timeout = 3.s
    //for
    //  fiber <- io.start
    //  _ <- fiber.joinStd
    //    .uncancelable.cancelable(IO.defer:
    //      val since = Deadline.now
    //      @volatile var logged = false
    //      IO
    //        .race(
    //          fiber.joinStd *> IO:
    //            if logged then
    //              logger.info(s"⚪️ $name $outErr finally closed after ${since.pretty}"),
    //          IO.sleep(timeout) *> IO:
    //            logged = true
    //            logger.warn:
    //              s"🟤 $name $outErr still not closed after cancellation ${since.pretty} ago")
    //        .startAndForget)
    //yield ()

  private def inputStreamAsStream(outErr: StdoutOrStderr, in: InputStream, encoding: Charset)
  : Stream[IO, String] =
    // Uninterruptible when waiting for next read (does it matter, do we cancel this?)
    //fs2.io
    //  .readInputStream(IO.pure(in), chunkSize = charBufferSize /*byte???*/)
    inputStreamToByteStream(in)
      .onFinalizeCase:
        case exitCase @ ExitCase.Canceled =>
          // FIXME When cancelling the stream, io.blocking happens to block itself.
          //  It does not execute its body and instead wats forever.
          //  Why?
          IO.blocking(())
            .logWhenItTakesLonger:
              s"### $name $outErr $exitCase   🔥🔥🔥 IO.blocking(()) is blocking itself 🔥🔥🔥"
            .startAndForget *>
              IO.whenA(false): // Better, we don't close the file
                logger.traceIO(s"### $name $outErr $exitCase in.close"):
                  IO
                    .blocking:
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
          StdObservers(outErrToSink, outChannel, errChannel, charBufferSize, useErrorLineLengthMax,
            name)
      _ <- stdObservers.pumpChannelsToSinkResource
    yield
      stdObservers

  @TestOnly
  def testSink(
    charBufferSize: Int = 8192,
    useErrorLineLengthMax: Option[Int] = None,
    name: String)
  : ResourceIO[TestSink] =
    Resource.defer:
      val out = mutable.Buffer.empty[String]
      val err = mutable.Buffer.empty[String]
      val outErrToSink: OutErrToSink = Map(
        Stdout -> (stream => stream.evalTap(string => IO(out.append(string))).drain),
        Stderr -> (stream => stream.evalTap(string => IO(err.append(string))).drain))
      for
        stdObservers <- resource(outErrToSink, charBufferSize,
          useErrorLineLengthMax = useErrorLineLengthMax,
          name = name)
      yield
        new TestSink(
          stdObservers,
          out = stdObservers.closeChannels *> IO(out.mkString),
          err = stdObservers.closeChannels *> IO(err.mkString))

  /** Provides an OutErrToSink which collects stdout and stderr each in a String. */
  @TestOnly
  final class TestSink private[StdObservers](
    val stdObservers: StdObservers,
    /** out and err implicitly close the channels via closeChannels. */
    val out: IO[String],
    val err: IO[String])
