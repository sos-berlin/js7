package js7.base.io.file.watch

import cats.effect.{IO, Resource, ResourceIO}
import fs2.Stream
import java.io.IOException
import java.nio.file.{ClosedWatchServiceException, NotDirectoryException, Path, WatchEvent, WatchKey}
import java.util.concurrent.TimeUnit.MILLISECONDS
import js7.base.fs2utils.StreamExtensions.+:
import js7.base.io.file.watch.BasicDirectoryWatch.*
import js7.base.io.file.watch.DirectoryWatchEvent.Overflow
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.raceMerge
import js7.base.service.Service
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.Delayer.extensions.onFailureRestartWithDelayer
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichThrowable}
import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

final class BasicDirectoryWatch private(options: WatchOptions)
extends Service.StoppableByRequest:

  import options.{directory, kinds, pollTimeout}

  // TODO Use a single WatchService with central polling for all directories, and occupy only one thread!
  logger.debug(s"newWatchService $directory")
  private val watchService = directory.getFileSystem.newWatchService()

  protected def start =
    startService:
      untilStopRequested
        .*>(IO:
          logger.debug(s"watchService.close() — $directory")
          watchService.close())

  private[watch] def streamResource: ResourceIO[Stream[IO, Seq[DirectoryEvent]]] =
    for _ <- directoryWatchResource yield
      (true +: Stream.constant(false))
        .evalMap(isFirst => IO
          .unlessA(isFirst)(IO.sleep(options.watchDelay)/*collect more events per context switch*/)
          .*>(poll)
          .raceMerge(untilStopRequested.as(Nil)))
        .takeWhile(events => !events.contains(Overflow))
        .map(_.asInstanceOf[Seq[DirectoryEvent]])

  private def directoryWatchResource: ResourceIO[WatchKey] =
    Resource.makeFull[IO, WatchKey](
      acquire = poll =>
        poll:
          cancelOnStopRequest:
            repeatWhileIOException(options):
              IO.blocking:
                logger.debug(s"register watchService $kinds, ${modifiers.mkString(",")} $directory")
                directory.register(watchService, kinds.toArray[WatchEvent.Kind[?]], modifiers*)
        .logWhenItTakesLonger(s"Registering $directory directory in WatchService"))(
      release = watchKey => IO:
        logger.debug(s"watchKey.cancel() $directory")
        try watchKey.cancel()
        catch case t: ClosedWatchServiceException =>
          logger.debug(s"watchKey.cancel() => ${t.toStringWithCauses}"))

  private def poll: IO[Seq[DirectoryWatchEvent]] =
    pollWatchKey().recover:
      case t: ClosedWatchServiceException =>
        logger.debug(s"${t.toStringWithCauses}")
        // This may execute after service stopped, and the tread pool may be closed too.
        // Therefore we never continue.
        // May this let block cancellation in cats-effect 3 ???
        // This is not a little memory leak, or ???
        Nil

  /** Wait for events. */
  private def pollWatchKey(): IO[Seq[DirectoryWatchEvent]] =
    interruptibleVirtualThread:
      watchService.poll(pollTimeout.toMillis, MILLISECONDS) match
        case null => Nil
        case watchKey =>
          val result = retrieveEvents(watchKey)
          logger.trace(s"WatchService.poll() $directory => ${result.mkString(" ")}")
          result


  private def retrieveEvents(watchKey: WatchKey): Seq[DirectoryWatchEvent] =
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

  // https://bugs.openjdk.java.net/browse/JDK-7133447
  @nowarn("cat=deprecation")
  private val sensitivity = isMac ? com.sun.nio.file.SensitivityWatchEventModifier.HIGH

  @nowarn("cat=deprecation")
  val systemWatchDelay: FiniteDuration =
    sensitivity.fold(0.s):
      _.sensitivityValueInSeconds.s/*2s*/

  @nowarn("cat=deprecation")
  private val modifiers: Array[WatchEvent.Modifier] =
    sensitivity match
      case None => Array.empty
      case Some(sensitivity) =>
        try Array(sensitivity)
        catch case t: Throwable =>
          logger.debug(s"❗ SensitivityWatchEventModifier.HIGH => ${t.toStringWithCauses}")
          Array.empty

  def resource(options: WatchOptions): ResourceIO[BasicDirectoryWatch] =
    Service.resource:
      new BasicDirectoryWatch(options)

  //private implicit val watchEventShow: Show[WatchEvent[?]] = e =>
  //  s"${e.kind.name} ${e.count}× ${e.context}"

  def repeatWhileIOException[A](options: WatchOptions)(body: IO[A]): IO[A] =
    body.onFailureRestartWithDelayer(options.delayConf):
      case (delayer, t @ (_: IOException | _: NotDirectoryException)) =>
        delayer.peekNextDelay.flatMap: delay =>
          IO(logger.warn:
            s"${options.directory}: delay ${delay.pretty} after error: ${t.toStringWithCauses}")
      case (_, t) => IO.raiseError(t)
