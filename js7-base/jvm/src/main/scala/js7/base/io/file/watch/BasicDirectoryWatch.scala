package js7.base.io.file.watch

import cats.effect.Resource
import java.io.IOException
import java.nio.file.{ClosedWatchServiceException, NotDirectoryException, Path, WatchEvent, WatchKey}
import java.util.concurrent.TimeUnit.MILLISECONDS
import js7.base.io.file.watch.BasicDirectoryWatch.*
import js7.base.io.file.watch.DirectoryWatchEvent.Overflow
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.service.Service
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.continueWithLast
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final class BasicDirectoryWatch private(options: WatchOptions)(implicit iox: IOExecutor)
extends Service.StoppableByRequest:
  import options.{directory, kinds, pollTimeout}

  // TODO Use a single WatchService with central polling for all directories, and occupy only one thread!
  logger.debug(s"newWatchService $directory")
  private val watchService = directory.getFileSystem.newWatchService()

  protected def start =
    startService(
      untilStopRequested
        .*>(Task {
          logger.debug(s"watchService.close() — $directory")
          watchService.close()
        }))

  private[watch] def observableResource: Resource[Task, Observable[Seq[DirectoryEvent]]] =
    for _ <- directoryWatchResource yield
      (true +: Observable.repeat(false))
        .mapEval(isFirst => Task
          .unless(isFirst)(Task.sleep(options.watchDelay)/*collect more events per context switch*/)
          .*>(poll)
          .raceFold(untilStopRequested.as(Nil)))
        .takeWhile(events => !events.contains(Overflow))
        .map(_.asInstanceOf[Seq[DirectoryEvent]])

  private def directoryWatchResource: Resource[Task, WatchKey] =
    Resource.make(
      acquire = failWhenStopRequested(repeatWhileIOException(options, Task {
        logger.debug(
          s"register watchService $kinds, ${highSensitivity.mkString(",")} $directory")
        directory.register(watchService, kinds.toArray: Array[WatchEvent.Kind[?]], highSensitivity*)
      })))(
      release = watchKey => Task {
        logger.debug(s"watchKey.cancel() $directory")
        try watchKey.cancel()
        catch { case t: java.nio.file.ClosedWatchServiceException =>
          logger.debug(s"watchKey.cancel() => ${t.toStringWithCauses}")
        }
      })

  private def poll: Task[Seq[DirectoryWatchEvent]] =
    Task.defer {
      val since = now
      def prefix = s"pollEvents($directory) ... ${since.elapsed.pretty} => "
      try
        val events = pollWatchKey()
        logger.trace(prefix + (if events.isEmpty then "timed out" else events.mkString(", ")))
        Task.pure(events)
      catch
       case NonFatal(t: ClosedWatchServiceException) if isStopping =>
         logger.debug(s"$prefix => ${t.toStringWithCauses}")
         // This may execute after service stopped, and the tread pool may be closed too.
         // Therefore we never continue.
         // May this let block cancellation in cats-effect 3 ???
         // This is not a little memory leak, or ???
         Task.never
    }.executeOn(iox.scheduler)

  private def pollWatchKey(): Seq[DirectoryWatchEvent] =
    watchService.poll(pollTimeout.toMillis, MILLISECONDS) match
      case null => Nil
      case watchKey =>
        try watchKey.pollEvents().asScala.view
          .collect:
            case o: WatchEvent[Path @unchecked]
              if o.context.isInstanceOf[Path] && options.isRelevantFile(o.context) => o
          .map(DirectoryWatchEvent.fromJava)
          .toVector
        finally
          watchKey.reset()

  override def toString =
    s"BasicDirectoryWatch($directory)"

object BasicDirectoryWatch:
  private val logger = Logger[this.type]

  val systemWatchDelay = if isMac/*polling*/ then 2.s else 0.s

  private val highSensitivity: Array[WatchEvent.Modifier] =
    if isMac then // https://bugs.openjdk.java.net/browse/JDK-7133447
      try Array(com.sun.nio.file.SensitivityWatchEventModifier.HIGH/*2s instead of 10s*/)
      catch { case t: Throwable =>
        logger.debug(s"❗ SensitivityWatchEventModifier.HIGH => ${t.toStringWithCauses}")
        Array.empty
      }
    else
      Array.empty

  def resource(options: WatchOptions)(implicit iox: IOExecutor)
  : Resource[Task, BasicDirectoryWatch] =
    Service.resource(Task(new BasicDirectoryWatch(options)))

  //private implicit val watchEventShow: Show[WatchEvent[?]] = e =>
  //  s"${e.kind.name} ${e.count}× ${e.context}"

  def repeatWhileIOException[A](options: WatchOptions, task: Task[A]): Task[A] =
    Task.defer:
      val delayIterator = continueWithLast(options.retryDelays)
      task
        .onErrorRestartLoop(now):
          case (t @ (_: IOException | _: NotDirectoryException), since, restart) =>
            Task.defer:
              val delay = (since + delayIterator.next()).timeLeftOrZero
              logger.warn(
                s"${options.directory}: delay ${delay.pretty} after error: ${t.toStringWithCauses}")
              Task.sleep(delay) >> restart(now)
          case (t, _, _) => Task.raiseError(t)
