package js7.base.io.file.watch

import java.nio.file.Path
import js7.base.io.file.watch.BasicDirectoryWatch.repeatWhileIOException
import js7.base.io.file.watch.DirectoryWatch.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import cats.effect.IO
import cats.syntax.flatMap.*
import fs2.Stream
import js7.base.fs2utils.StreamExtensions.tapEach
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

private final class DirectoryWatch(
  readDirectory: IO[DirectoryState],
  directoryEventStream: Stream[IO, Seq[DirectoryEvent]],
  hotLoopBrake: FiniteDuration):

  private def readDirectoryAndObserveForever(state: DirectoryState)
  : Stream[IO, (Seq[DirectoryEvent], DirectoryState)] =
    logger.traceStream:
      state.tailRecM { state =>
        val since = now
        @volatile var lastState = state
        readDirectoryAndObserve(state)
          .tapEach((_, state) => lastState = state)
          .map(Right(_))
          .append(Stream.eval(
            IO(Left(lastState))
              .delayBy((since + hotLoopBrake).timeLeftOrZero)))
      }

  private[watch] def readDirectoryAndObserve(state: DirectoryState)
  : Stream[IO, (Seq[DirectoryEvent], DirectoryState)] =
    logger.traceStream:
      Stream
        .eval(readDirectory map state.diffTo)
        .filter(_.nonEmpty)
        .++(directoryEventStream
          // BasicDirectoryWatch yields Nil when poll() timed out.
          // Then we end, allowing the caller to restart and
          // to handle an exchanged directory.
          .takeWhile(_.nonEmpty))
        .scan(state -> Seq.empty[DirectoryEvent])((pair, events) =>
          pair._1.applyAndReduceEvents(events).swap)
        .map(_.swap)
        .filter(_._1.nonEmpty)


object DirectoryWatch:
  private val logger = Logger[this.type]

  def stream(
    directory: Path,
    directoryState: DirectoryState,
    settings: DirectoryWatchSettings,
    isRelevantFile: Path => Boolean = WatchOptions.everyFileIsRelevant)
    (implicit iox: IOExecutor)
  : Stream[IO, Seq[DirectoryEvent]] =
    DirectoryWatch.stream2(
      directoryState,
      settings.toWatchOptions(directory, isRelevantFile))

  private def stream2(state: DirectoryState, options: WatchOptions)
    (implicit iox: IOExecutor)
  : Stream[IO, Seq[DirectoryEvent]] =
    logger.traceStream:
      Stream
        .resource(BasicDirectoryWatch.resource(options))
        .flatMap { basicWatch =>
          import options.directory
          // BasicDirectoryWatch has been started before reading directory,
          // so that no directory change will be overlooked.
          val readDirectory = repeatWhileIOException(
            options,
            iox(IO(DirectoryStateJvm.readDirectory(directory, options.isRelevantFile))))
          Stream
            .resource(basicWatch.streamResource)
            .flatMap(stream =>
              new DirectoryWatch(readDirectory, stream, hotLoopBrake = options.retryDelays.head)
                .readDirectoryAndObserveForever(state)
                .map(_._1))
        }
