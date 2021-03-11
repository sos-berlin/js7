package js7.base.io.file.watch

import cats.Show
import cats.effect.{ExitCase, Resource}
import java.nio.file.{ClosedWatchServiceException, Path, WatchEvent}
import java.util.concurrent.TimeUnit.MILLISECONDS
import js7.base.io.file.watch.DirectoryWatchEvent.DirectoryWatchOverflow
import js7.base.io.file.watch.SimpleDirectoryWatcher._
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.{RichDeadline, RichDuration}
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichThrowable}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

final class SimpleDirectoryWatcher(options: WatchOptions)(implicit iox: IOExecutor)
extends AutoCloseable
{
  import options.{directory, kinds, pollDuration}

  // TODO Use a single thread (or single poll) for all directories of the same file system!
  private val watchService = directory.getFileSystem.newWatchService()
  @volatile private var canceled = false

  closeOnError(watchService) {
    logger.debug(s"register watchService $kinds, ${highSensitivity.mkString(",")}")
    directory.register(watchService, kinds.toArray: Array[WatchEvent.Kind[_]], highSensitivity: _*)
  }

  def stop(canceled: Boolean = false): Task[Unit] =
    Task {
      this.canceled |= canceled
      close()
    }

  def close() = {
    logger.debug("watchService.close()")
    watchService.close()
  }

  private[watch] def observe: Observable[Seq[DirectoryEvent]] =
    (true +: Observable.repeat(false))
      .map(isFirst => poll
        .pipeIf(!isFirst)(_.delayExecution(options.delay)/*collect more events per context switch*/))
      .flatMap(Observable.fromTask)
      .takeWhile(events => !events.contains(DirectoryWatchOverflow))
      .map(_.asInstanceOf[Seq[DirectoryEvent]])
      .onErrorRecoverWith {
        case _: ClosedWatchServiceException => Observable.empty
      }

  private val poll: Task[Seq[DirectoryWatchEvent]] =
    Task.defer {
      lazy val msg = s"Blocking '${Thread.currentThread.getName}' thread for '$directory' ..."
      val since = now
      logger.trace(msg)
      try {
        val events = pollWatchKey()
        logger.trace(s"$msg ${since.elapsed.pretty} => ${events.mkString(", ")}")
        Task.pure(events)
      } catch { case NonFatal(t) if canceled =>
        logger.trace(s"$msg ${since.elapsed.pretty} => canceled (${t.toStringWithCauses})")
        // Ignore the error, otherwise it would be logged by the thread pool.
        Task.never  // because the task is canceled
      }
    }.executeOn(iox.scheduler)

  private def pollWatchKey(): Seq[DirectoryWatchEvent] =
    pollDuration.fold(watchService.take())(d => watchService.poll(d.toMillis, MILLISECONDS)) match {
      case null => Nil
      case watchKey =>
        try watchKey.pollEvents().asScala.view
          .map(_.asInstanceOf[WatchEvent[Path]])
          .map(DirectoryWatchEvent.fromJava)
          .toVector
        finally watchKey.reset()
    }
}

object SimpleDirectoryWatcher
{
  private val logger = Logger(getClass)

  private val highSensitivity: Array[WatchEvent.Modifier] =
    if (isMac)
      try Array(com.sun.nio.file.SensitivityWatchEventModifier.HIGH/*2s instead of 10s*/)
      catch { case t: Throwable =>
        logger.debug(s"SensitivityWatchEventModifier.HIGH => ${t.toStringWithCauses}")
        Array.empty
      }
    else
      Array.empty

  def resource(options: WatchOptions)(implicit iox: IOExecutor)
  : Resource[Task, SimpleDirectoryWatcher] =
    Resource
      .makeCase(
        acquire = Task(new SimpleDirectoryWatcher(options)))(
        release = (directoryWatcher, exitCase) =>
          directoryWatcher.stop(
            canceled = exitCase == ExitCase.Canceled))

  private implicit val watchEventShow: Show[WatchEvent[_]] = e =>
    s"${e.kind.name} ${e.count}× ${e.context}"
}
