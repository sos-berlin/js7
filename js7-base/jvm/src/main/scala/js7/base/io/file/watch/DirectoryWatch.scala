package js7.base.io.file.watch

import cats.effect.IO
import cats.syntax.flatMap.*
import fs2.{Chunk, Stream}
import java.nio.file.Path
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.fs2utils.StreamExtensions.tapEach
import js7.base.io.file.watch.BasicDirectoryWatch.repeatWhileIOException
import js7.base.io.file.watch.DirectoryWatch.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

private final class DirectoryWatch(
  readDirectory: IO[DirectoryState],
  directoryEventStream: Stream[IO, Seq[DirectoryEvent]],
  hotLoopBrake: FiniteDuration):

  private def readDirectoryThenStreamForever(state: DirectoryState)
  : Stream[IO, (Seq[DirectoryEvent], DirectoryState)] =
    logger.traceStream:
      state.tailRecM: state =>
        val since = now
        @volatile var lastState = state
        readDirectoryThenStream(state)
          .tapEach((_, state) => lastState = state)
          .map(Right(_))
          .append(Stream.eval:
            IO.left(lastState)
              .delayBy((since + hotLoopBrake).timeLeftOrZero))

  private[watch] def readDirectoryThenStream(state: DirectoryState)
  : Stream[IO, (Seq[DirectoryEvent], DirectoryState)] =
    Stream
      .eval(readDirectory map state.diffTo)
      .filter(_.nonEmpty)
      .++(directoryEventStream
        // BasicDirectoryWatch yields Nil when poll() timed out.
        // Then we end, allowing the caller to restart and
        // to handle an exchanged directory.
        .takeWhile(_.nonEmpty))
      .scan(state -> Seq.empty[DirectoryEvent]): (pair, events) =>
        pair._1.applyAndReduceEvents(events).swap
      .drop(1) // Drop initial value
      .map(_.swap)
      .filter(_._1.nonEmpty)


object DirectoryWatch:
  private val logger = Logger[this.type]

  def stream(
    directory: Path,
    directoryState: DirectoryState,
    settings: DirectoryWatchSettings,
    isRelevantFile: Path => Boolean = WatchOptions.everyFileIsRelevant)
    (using iox: IOExecutor)
  : Stream[IO, DirectoryEvent] =
    stream2(
      directoryState,
      settings.toWatchOptions(directory, isRelevantFile))

  private def stream2(state: DirectoryState, options: WatchOptions)
    (using iox: IOExecutor)
  : Stream[IO, DirectoryEvent] =
    logger.traceStream:
      Stream
        .resource(BasicDirectoryWatch.resource(options))
        .flatMap: basicWatch =>
          import options.directory
          // BasicDirectoryWatch has been started before reading directory,
          // so that no directory change will be overlooked.
          val readDirectory = repeatWhileIOException(
            options,
            IO.interruptible:
              DirectoryStateJvm.readDirectory(directory, options.isRelevantFile))
          Stream
            .resource(basicWatch.streamResource)
            .flatMap: stream =>
              new DirectoryWatch(readDirectory, stream, hotLoopBrake = options.retryDelays.head)
                .readDirectoryThenStreamForever(state)
                .map(_._1)
                .map(Chunk.from)
                .unchunks
