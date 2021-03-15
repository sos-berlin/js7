package js7.base.io.file.watch

import js7.base.io.file.watch.BasicDirectoryWatcher.repeatWhileIOException
import js7.base.io.file.watch.DirectoryWatcher.hotLoopBrake
import js7.base.monixutils.MonixBase.syntax._
import js7.base.thread.IOExecutor
import js7.base.thread.IOExecutor.ioTask
import js7.base.time.ScalaTime._
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now

private final class DirectoryWatcher(
  readDirectory: Task[DirectoryState],
  directoryEventObservable: Observable[Seq[DirectoryEvent]])
{
  private def readDirectoryAndObserveForever(state: DirectoryState)
  : Observable[(Seq[DirectoryEvent], DirectoryState)] =
    Observable.defer {
      val since = now
      @volatile var lastState = state
      readDirectoryAndObserve(state)
        .tapEach { case (_, state) =>
          lastState = state
        } ++
        readDirectoryAndObserveForever(lastState)
          .delayExecution((since + hotLoopBrake).timeLeftOrZero)
    }

  private[watch] def readDirectoryAndObserve(state: DirectoryState)
  : Observable[(Seq[DirectoryEvent], DirectoryState)] =
    Observable
      .fromTask(readDirectory map state.diffTo)
      .filter(_.nonEmpty)
      .appendAll(directoryEventObservable
        // BasicDirectoryWatch yield Nil when poll() timed out.
        // Then we end, allowing the caller to restart and
        // to handle an exchanged directory.
        .takeWhile(_.nonEmpty))
      .scan(state -> Seq.empty[DirectoryEvent])((pair, events) =>
        pair._1.applyAndReduceEvents(events).swap)
      .map(_.swap)
      .filterNot(_._1.isEmpty)
}

object DirectoryWatcher
{
  val hotLoopBrake = 1.s

  def observe(state: DirectoryState, options: WatchOptions)(implicit iox: IOExecutor)
  : Observable[Seq[DirectoryEvent]] =
    Observable
      .fromResource(
        BasicDirectoryWatcher.resource(options))
      .flatMap { basicWatcher =>
        import options.directory
        // BasicDirectoryWatcher has been started before reading directory,
        // so that no directory change will be overlooked.
        // TODO Race condition when a file is added or deleted now?
        val readDirectory = repeatWhileIOException(
          options,
          ioTask(DirectoryState.readDirectory(directory)))
        Observable.fromResource(basicWatcher.startObservable)
          .flatMap(observable =>
            new DirectoryWatcher(readDirectory, observable)
              .readDirectoryAndObserveForever(state)
              .map(_._1))
      }
}
