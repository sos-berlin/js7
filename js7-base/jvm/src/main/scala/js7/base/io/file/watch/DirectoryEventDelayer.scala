package js7.base.io.file.watch

import cats.syntax.traverse.*
import cats.syntax.option.*
import DirectoryEventDelayer.*
import cats.data.NonEmptySeq
import cats.effect.{FiberIO, IO}
import fs2.{Pipe, Stream}
import java.io.IOException
import java.nio.file.Files.size
import java.nio.file.Path
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsDeadline.now
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileAddedOrModified, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryEventDelayer.*
import js7.base.log.Logger
import js7.base.monixutils.AsyncVariable
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsUtils.continueWithLast
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Try

/** A special delay line for DirectoryEvents.
  *
  * FileAdded is delayed,
  * FileModified delays FileAdded further,
  * FileDeleted is not delayed (and thus forcing a maybe pending FileAdded).
  *
  * Yields only FileAdded and FileDeleted events.
  *
  * Derived from Monix' `DelayByTimespanStream`.
  */
private final class DirectoryEventDelayer(
  directory: Path,
  delay: FiniteDuration,
  logDelays: NonEmptySeq[FiniteDuration])
extends Pipe[IO, Seq[DirectoryEvent], Seq[DirectoryEvent]]:
  self =>

  private val indexToEntry = mutable.TreeMap.empty[Long, Entry] // input queue
  private val pathToIndex = mutable.Map.empty[Path, Long]
  private val outputQueue = new VectorBuilder[DirectoryEvent]
  private val delayer = Atomic(none[FiberIO[Unit]]) //AsyncVariable[Fiber[?]]
  private var statsAdded = 0L
  private var statsModified = 0L
  private var statsTimer = 0L

  def apply(in: Stream[IO, Seq[DirectoryEvent]]): Stream[IO, Seq[DirectoryEvent]] =
    in.map(_
        .traverse(event => processEvent(event))
        .map(_.flatten))
      .flatMap(Stream.eval)

  private def processEvent(directoryEvent: DirectoryEvent): IO[Seq[DirectoryEvent]] =
    // Queue incoming events and keep FileAdded events until delay is elapsed
    CatsDeadline.now.flatMap(now =>
      directoryEvent match
        case fileAdded @ FileAdded(path) =>
          statsAdded += 1
          self.synchronized:
            for index <- pathToIndex.remove(path) do
              indexToEntry -= index
            enqueue(Entry(fileAdded, now))

        case fileModified @ FileModified(path) =>
          statsModified += 1
          self.synchronized:
            pathToIndex.remove(path) match
              case None =>
                enqueue(Entry(fileModified, now))

              case Some(previousIndex) =>
                indexToEntry.remove(previousIndex) match
                  case None => IO.pure(Nil)
                  case Some(entry) =>
                    val now_ = now
                    entry.logStillWritten(now_)
                    entry.delayUntil = now_ + delay
                    enqueue(entry)

        case deleted: FileDeleted =>
          self.synchronized:
            pathToIndex.remove(deleted.relativePath) match
              case None =>
                outputQueue += deleted
                flush

              case Some(previousIndex) =>
                indexToEntry -= previousIndex
                IO.pure(Nil)
    )

  private def enqueue(entry: Entry): IO[Seq[DirectoryEvent]] =
    val isFirst = indexToEntry.isEmpty
    val index = indexToEntry.lastOption.fold(0L)(_._1) + 1
    indexToEntry.update(index, entry)
    pathToIndex(entry.path) = index

    if isFirst then
      delayedFlush
    else
      IO.pure(Nil)


  // ⬇︎ ASYNCHRONOUSLY CALLED CODE ⬇

  private def delayedFlush: IO[Seq[DirectoryEvent]] =
    for
      deferred <- IO.deferred[Seq[DirectoryEvent]]
      fiber <-
        IO.sleep(delay)
          .*>(flush.flatMap(deferred.complete))
          .void
          .start
      _ <- delayer.getAndSet(fiber.some).fold(IO.unit)(_.cancel)
      result <- deferred.get
    yield result

  private def flush: IO[Seq[DirectoryEvent]] =
    IO:
      val result = outputQueue.result()
      outputQueue.clear()
      result

  //@tailrec
  //private def forward(): IO[Seq[DirectoryEvent]] =
  //  //if forwardReentrant.incrementAndGet() == 1 then
  //    @tailrec def loop(): Unit = {
  //      self.synchronized {
  //        dequeueTo(outputQueue)
  //      }
  //      sendOutputQueue()
  //      self.synchronized {
  //        if callerCompleted && indexToEntry.isEmpty && outputQueue.isEmpty then {
  //          outOnComplete()
  //        }
  //      }
  //      if forwardReentrant.decrementAndGet() > 0 then {
  //        loop()
  //      }
  //    }
  //
  //    loop()
  //
  //    self.synchronized {
  //      if timer.isEmpty && !hasError then
  //        for entry <- indexToEntry.values.headOption yield
  //          entry.delayUntil.timeLeftOrZero
  //      else
  //        None
  //    } match {
  //      case None =>
  //      case Some(ZeroDuration) => forward()
  //      case Some(nextDelay) => setTimer(nextDelay)
  //    }

  //private def dequeueTo(growable: mutable.Growable[FileAddedOrModified]): Unit =
  //  if !hasError then {
  //    val now_ = now
  //
  //    @tailrec def loop(): Unit =
  //      indexToEntry.headOption match {
  //        case Some((index, entry)) if entry.delayUntil <= now_ =>
  //          indexToEntry -= index
  //          pathToIndex -= entry.path
  //          growable += entry.event
  //          loop()
  //        case _ =>
  //      }
  //
  //    loop()
  //  }

  //private def sendOutputQueue(): Unit =
  //  ack = ack.syncFlatMapOnContinue {
  //    val events = self.synchronized {
  //      val events = outputQueue.result()
  //      outputQueue.clear()
  //      events
  //    }
  //    if events.isEmpty then
  //      Continue
  //    else {
  //      for event <- events do logger.trace(s"out.onNext $event")
  //      out.onNext(events)
  //    }
  //  }


  private final class Entry(
    val event: FileAddedOrModified,
    val since: CatsDeadline):

    var delayUntil = since + delay
    val logDelayIterator = continueWithLast(logDelays)
    var lastLoggedAt = since
    var lastLoggedSize: Long =
      Try(size(directory.resolve(path))) getOrElse 0
    var logDelay = delay + logDelayIterator.next()

    def path = event.relativePath

    def logStillWritten(now: CatsDeadline): Unit =
      if lastLoggedAt + logDelay <= now then
        switchLogDelay()
        lastLoggedAt = now
        val file = directory.resolve(path)
        val growth = try
          val s = size(file)
          val growth = s - lastLoggedSize
          lastLoggedSize = s
          toKBGB(growth)
        catch case _: IOException => "?"
        logger.info:
          s"Watched file is still being modified for ${(now - since).pretty}: $file +$growth"

    private def switchLogDelay(): Unit =
      if logDelayIterator.hasNext then
        logDelay = logDelayIterator.next()

    override def toString = s"$path delayUntil=$delayUntil"


object DirectoryEventDelayer:
  private val logger = Logger[this.type]

  def pipe(
    directory: Path,
    delay: FiniteDuration,
    logDelays: NonEmptySeq[FiniteDuration])
  : Pipe[IO, Seq[DirectoryEvent], Seq[DirectoryEvent]] =
    self =>
      if delay.isPositive then
        self.through(new DirectoryEventDelayer(directory, delay, logDelays))
      else
        self //Monix??? .bufferIntrospective(1024)  // Similar to DirectoryEventDelayer, which buffers without limit


  object syntax:
    extension(self: Stream[IO, Seq[DirectoryEvent]])
      @deprecated("2.7", "Use .through(DirectoryEventDelayer.pipe(...))")
      def delayFileAdded(
        directory: Path, delay: FiniteDuration, logDelays: NonEmptySeq[FiniteDuration])
      : Stream[IO, Seq[DirectoryEvent]] =
        self.through(pipe(directory, delay, logDelays))
