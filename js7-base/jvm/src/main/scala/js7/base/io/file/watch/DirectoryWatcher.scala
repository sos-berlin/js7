package js7.base.io.file.watch

import js7.base.io.file.watch.BasicDirectoryWatcher.repeatWhileIOException
import js7.base.io.file.watch.DirectoryWatcher.*
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

private final class DirectoryWatcher(
  readDirectory: Task[DirectoryState],
  directoryEventObservable: Observable[Seq[DirectoryEvent]],
  hotLoopBrake: FiniteDuration)
{
  private def readDirectoryAndObserveForever(state: DirectoryState)
  : Observable[(Seq[DirectoryEvent], DirectoryState)] =
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
      }
      .guaranteeCase(exitCase => Task(
        logger.debug(s"readDirectoryAndObserveForever: $exitCase")))

  private[watch] def readDirectoryAndObserve(state: DirectoryState)
  : Observable[(Seq[DirectoryEvent], DirectoryState)] =
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
      .filter(_._1.nonEmpty)
      .guaranteeCase(exitCase => Task(
        logger.trace(s"readDirectoryAndObserve: $exitCase")))
}

object DirectoryWatcher
{
  private val logger = Logger[this.type]

  def observable(state: DirectoryState, options: WatchOptions)(implicit iox: IOExecutor)
  : Observable[Seq[DirectoryEvent]] =
    Observable
      .fromResource(BasicDirectoryWatcher.resource(options))
      .flatMap { basicWatcher =>
        import options.directory
        // BasicDirectoryWatcher has been started before reading directory,
        // so that no directory change will be overlooked.
        val readDirectory = repeatWhileIOException(
          options,
          iox(Task(DirectoryState.readDirectory(directory, options.matches))))
        Observable
          .fromResource(basicWatcher.observableResource)
          .flatMap(observable =>
            new DirectoryWatcher(readDirectory, observable, hotLoopBrake = options.retryDelays.head)
              .readDirectoryAndObserveForever(state)
              .map(_._1))
      }
}
