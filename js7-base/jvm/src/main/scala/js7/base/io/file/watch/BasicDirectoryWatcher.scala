package js7.base.io.file.watch

import cats.effect.{ExitCase, Resource}
import java.io.IOException
import java.nio.file.{ClosedWatchServiceException, NotDirectoryException, Path, WatchEvent, WatchKey}
import java.util.concurrent.TimeUnit.MILLISECONDS
import js7.base.io.file.watch.BasicDirectoryWatcher.*
import js7.base.io.file.watch.DirectoryWatchEvent.Overflow
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichThrowable}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final class BasicDirectoryWatcher(options: WatchOptions)(implicit iox: IOExecutor)
extends AutoCloseable
{
  import options.{directory, kinds, pollTimeout}

  // TODO Use a single WatchService with central polling for all directories, and occupy only one thread!
  logger.debug(s"newWatchService $directory")
  private val watchService = directory.getFileSystem.newWatchService()

  def stop(canceled: Boolean = false): Task[Unit] =
    Task {
      close()
    }

  def close() = {
    logger.debug("watchService.close()")
    watchService.close()
  }

  private[watch] def observableResource: Resource[Task, Observable[Seq[DirectoryEvent]]] =
    directoryWatchResource.map(_ => Observable.defer {
      @volatile var canceled = false
      (true +: Observable.repeat(false))
        .doOnSubscriptionCancel(Task { canceled = true })
        .map(isFirst => poll(canceled)
          .pipeIf(!isFirst)(_.delayExecution(options.delay)/*collect more events per context switch*/))
        .flatMap(Observable.fromTask)
        .takeWhile(events => !events.contains(Overflow))
        .map(_.asInstanceOf[Seq[DirectoryEvent]])
    })

  private def directoryWatchResource: Resource[Task, WatchKey] =
    Resource.make(
      repeatWhileIOException(options, Task {
        logger.debug(s"register watchService $kinds, ${highSensitivity.mkString(",")} $directory")
        directory.register(watchService, kinds.toArray: Array[WatchEvent.Kind[?]], highSensitivity*)
      })
    )(release = watchKey => Task {
      logger.debug(s"watchKey.cancel() $directory")
      watchKey.cancel()
    })

  private def poll(canceled: => Boolean): Task[Seq[DirectoryWatchEvent]] =
    Task.defer {
      val since = now
      def prefix = s"pollEvents ... ${since.elapsed.pretty} => "
      try {
        val events = pollWatchKey()
        logger.trace(prefix + (if (events.isEmpty) "timed out" else events.mkString(", ")))
        Task.pure(events)
      } catch {
        case NonFatal(t) if canceled =>
          logger.trace(s"${prefix}canceled (${t.toStringWithCauses})")
          // Ignore the error, otherwise it would be logged by the thread pool.
          Task.never  // because the task is canceled

       case NonFatal(t: ClosedWatchServiceException) =>
         // At least for testing, check ClosedWatchServiceException exception as early as here,
         // otherwise RejectedExecutionException may be thrown later.
         logger.debug(s"${t.toStringWithCauses}")
         Task.pure(Nil)
      }
    }.executeOn(iox.scheduler)

  private def pollWatchKey(): Seq[DirectoryWatchEvent] =
    watchService.poll(pollTimeout.toMillis, MILLISECONDS) match {
      case null => Nil
      case watchKey =>
        try watchKey.pollEvents().asScala.view
          .collect {
            case o: WatchEvent[Path @unchecked]
              if o.context.isInstanceOf[Path] && options.matches(o.context) => o
          }
          .map(DirectoryWatchEvent.fromJava)
          .toVector
        finally watchKey.reset()
    }
}

object BasicDirectoryWatcher
{
  private val logger = Logger(getClass)

  val systemWatchDelay = if (isMac/*polling*/) 2.s else 0.s

  private val highSensitivity: Array[WatchEvent.Modifier] =
    if (isMac) // https://bugs.openjdk.java.net/browse/JDK-7133447
      try Array(com.sun.nio.file.SensitivityWatchEventModifier.HIGH/*2s instead of 10s*/)
      catch { case t: Throwable =>
        logger.debug(s"SensitivityWatchEventModifier.HIGH => ${t.toStringWithCauses}")
        Array.empty
      }
    else
      Array.empty

  def resource(options: WatchOptions)(implicit iox: IOExecutor)
  : Resource[Task, BasicDirectoryWatcher] =
    Resource
      .makeCase(
        acquire = Task(new BasicDirectoryWatcher(options)))(
        release = (directoryWatcher, exitCase) =>
          directoryWatcher.stop(
            canceled = exitCase == ExitCase.Canceled))

  //private implicit val watchEventShow: Show[WatchEvent[?]] = e =>
  //  s"${e.kind.name} ${e.count}× ${e.context}"

  def repeatWhileIOException[A](options: WatchOptions, task: Task[A]): Task[A] =
    Task.defer {
      val delayIterator = options.retryDelays.iterator ++ Iterator.continually(options.retryDelays.last)
      task
        .onErrorRestartLoop(now) {
          case (t @ (_: IOException | _: NotDirectoryException), since, restart) =>
            Task.defer {
              val delay = (since + delayIterator.next()).timeLeftOrZero
              logger.warn(s"${options.directory}: delay ${delay.pretty} after error: ${t.toStringWithCauses} ")
              Task.sleep(delay) >> restart(now)
            }
          case (t, _, _) => Task.raiseError(t)
        }
    }
}
