package js7.base.io.file.watch

import java.nio.file.Path
import js7.base.io.file.watch.BasicDirectoryWatch.repeatWhileIOException
import js7.base.io.file.watch.DirectoryWatch.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

private final class DirectoryWatch(
  readDirectory: Task[DirectoryState],
  directoryEventObservable: Observable[Seq[DirectoryEvent]],
  hotLoopBrake: FiniteDuration):
  private def readDirectoryAndObserveForever(state: DirectoryState)
  : Observable[(Seq[DirectoryEvent], DirectoryState)] =
    logger.traceObservable(
      Observable
        .tailRecM(state) { state =>
          val since = now
          @volatile var lastState = state
          readDirectoryAndObserve(state)
            .tapEach { case (_, state) => lastState = state }
            .map(Right(_)) ++
            Observable.evalDelayed(
              (since + hotLoopBrake).timeLeftOrZero,
              Left(lastState))
        })

  private[watch] def readDirectoryAndObserve(state: DirectoryState)
  : Observable[(Seq[DirectoryEvent], DirectoryState)] =
    logger.traceObservable(
      Observable
        .fromTask(readDirectory map state.diffTo)
        .filter(_.nonEmpty)
        .appendAll(directoryEventObservable
          // BasicDirectoryWatch yields Nil when poll() timed out.
          // Then we end, allowing the caller to restart and
          // to handle an exchanged directory.
          .takeWhile(_.nonEmpty))
        .scan(state -> Seq.empty[DirectoryEvent])((pair, events) =>
          pair._1.applyAndReduceEvents(events).swap)
        .map(_.swap)
        .filter(_._1.nonEmpty))

object DirectoryWatch:
  private val logger = Logger[this.type]

  def observable(
    directory: Path,
    directoryState: DirectoryState,
    settings: DirectoryWatchSettings,
    isRelevantFile: Path => Boolean = WatchOptions.everyFileIsRelevant)
    (implicit iox: IOExecutor)
  : Observable[Seq[DirectoryEvent]] =
    DirectoryWatch
      .observable2(
        directoryState,
        settings.toWatchOptions(directory, isRelevantFile))

  private def observable2(state: DirectoryState, options: WatchOptions)
    (implicit iox: IOExecutor)
  : Observable[Seq[DirectoryEvent]] =
    logger.traceObservable(
      Observable
        .fromResource(BasicDirectoryWatch.resource(options))
        .flatMap { basicWatch =>
          import options.directory
          // BasicDirectoryWatch has been started before reading directory,
          // so that no directory change will be overlooked.
          val readDirectory = repeatWhileIOException(
            options,
            iox(Task(DirectoryStateJvm.readDirectory(directory, options.isRelevantFile))))
          Observable
            .fromResource(basicWatch.observableResource)
            .flatMap(observable =>
              new DirectoryWatch(readDirectory, observable, hotLoopBrake = options.retryDelays.head)
                .readDirectoryAndObserveForever(state)
                .map(_._1))
        })
