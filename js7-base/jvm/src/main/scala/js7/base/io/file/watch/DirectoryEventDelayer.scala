package js7.base.io.file.watch

import java.io.IOException
import java.nio.file.Files.size
import java.nio.file.Path
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryEventDelayer.*
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixAckFuture
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.time.ScalaTime.*
import js7.base.utils.ByteUnits.toKBGB
import monix.execution.Ack.{Continue, Stop}
import monix.execution.atomic.Atomic
import monix.execution.{Ack, Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
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
  * Derived from Monix' `DelayByTimespanObservable`.
  */
private final class DirectoryEventDelayer(
  source: Observable[DirectoryEvent],
  directory: Path,
  delay: FiniteDuration,
  logDelays: Seq[FiniteDuration])
extends Observable[Seq[DirectoryEvent]]
{
  def unsafeSubscribeFn(out: Subscriber[Seq[DirectoryEvent]]): Cancelable =
    source.subscribe(new Subscriber[DirectoryEvent] { self =>
      implicit val scheduler: Scheduler = out.scheduler
      private var addedEventCount = 0L
      private var modifiedEventCount = 0L
      private var timerCount = 0L
      private var timer: Option[Cancelable] = None
      private val forwardReentrant = Atomic(0)
      private var callerCompleted = false
      private var hasError = false
      private val isDone = Atomic(false)
      private var ack: Future[Ack] = Continue
      private val indexToEntry = mutable.TreeMap.empty[Long, Entry]  // input queue
      private val pathToIndex = mutable.Map.empty[Path, Long]
      private val outputQueue = new VectorBuilder[DirectoryEvent]

      def onNext(directoryEvent: DirectoryEvent) = {
        logger.trace(s"onNext $directoryEvent")
        directoryEvent match {
          case fileAdded @ FileAdded(path) =>
            addedEventCount += 1
            self.synchronized {
              for (index <- pathToIndex.remove(path)) {
                indexToEntry -= index
              }
              val isFirst = indexToEntry.isEmpty
              enqueue(new Entry(fileAdded, now))
              if (isFirst) setTimer(delay)
            }

          case FileModified(path) =>
            modifiedEventCount += 1
            self.synchronized {
              for (previousIndex <- pathToIndex.remove(path)) {
                for (entry <- indexToEntry.remove(previousIndex)) {
                  val now_ = now
                  entry.logStillWritten(now_)
                  entry.delayUntil = now_ + delay
                  enqueue(entry)
                }
              }
            }

          case deleted: FileDeleted =>
            self.synchronized {
              pathToIndex.remove(deleted.relativePath) match {
                case None =>
                  outputQueue += deleted
                  forward()

                case Some(previousIndex) =>
                  indexToEntry -= previousIndex
              }
            }
        }
        // Swallow all incoming events and keep FileAdded events until delay is elapsing !!!
        if (ack == Stop) Stop else Continue
      }

      private def enqueue(entry: Entry): Unit = {
        val index = indexToEntry.lastOption.fold(0L)(_._1) + 1
        indexToEntry.update(index, entry)
        pathToIndex(entry.path) = index

        if (delay.isZero) {
          forward()
        }
      }

      def onComplete(): Unit = {
        logger.trace(s"onComplete")
        self.synchronized {
          if (indexToEntry.nonEmpty) {
            callerCompleted = true
          } else
            outOnComplete()
        }
      }

      def onError(throwable: Throwable): Unit =
        self.synchronized {
          if (!isDone.getAndSet(true)) {
            hasError = true
            try out.onError(throwable)
            finally {
              ack = Stop
              timer.foreach(_.cancel())
            }
          }
        }

      // ⬇︎ ASYNCHRONOUSLY CALLED CODE ⬇

      private def setTimer(nextDelay: FiniteDuration): Unit =
        self.synchronized {
          if (timer.isEmpty && !hasError) {
            logger.trace(s"⏰ scheduleOnce ${nextDelay.pretty}")
            timer = Some(
              scheduler.scheduleOnce(nextDelay) {
                logger.trace(s"🔔 Timer event")
                self.synchronized {
                  timer = None
                  timerCount += 1
                }
                forward()
              })
          }
        }

      @tailrec
      private def forward(): Unit =
        if (forwardReentrant.incrementAndGet() == 1) {
          @tailrec def loop(): Unit = {
            self.synchronized {
              dequeueTo(outputQueue)
            }
            send()
            self.synchronized {
              if (callerCompleted && indexToEntry.isEmpty && outputQueue.isEmpty) {
                outOnComplete()
              }
            }
            if (forwardReentrant.decrementAndGet() > 0) {
              loop()
            }
          }

          loop()

          self.synchronized {
            if (timer.isEmpty && !hasError)
              for (entry <- indexToEntry.values.headOption) yield
                entry.delayUntil.timeLeftOrZero
            else
              None
          } match {
            case None =>
            case Some(ZeroDuration) => forward()
            case Some(nextDelay) => setTimer(nextDelay)
          }
        }

      private def dequeueTo(growable: mutable.Growable[FileAdded]): Unit =
        if (!hasError) {
          val now_ = now

          @tailrec def loop(): Unit =
            indexToEntry.headOption match {
              case Some((index, entry)) if entry.delayUntil <= now_ =>
                indexToEntry -= index
                pathToIndex -= entry.path
                growable += entry.fileAdded
                loop()
              case _ =>
            }

          loop()
        }

      private def send(): Unit =
        ack = ack.syncFlatMapOnContinue {
          val events = self.synchronized {
            val events = outputQueue.result()
            outputQueue.clear()
            events
          }
          if (events.isEmpty)
            Continue
          else {
            for (event <- events) logger.trace(s"out.onNext $event")
            out.onNext(events)
          }
        }

      private def outOnComplete(): Unit =
        ack.syncTryFlatten.syncOnContinue {
          if (!isDone.getAndSet(true)) {
            logger.debug(
              s"$addedEventCount×FileAdded · $modifiedEventCount×FileModified · $timerCount timer events")
            try out.onComplete()
            finally timer.foreach(_.cancel())  // Just to be sure
          }
        }

      private final class Entry(
        val fileAdded: FileAdded,
        val since: MonixDeadline)
      {
        var delayUntil = since + delay
        val logDelayIterator = logDelays.iterator
        var lastLoggedAt = since
        var lastLoggedSize: Long =
          Try(size(directory.resolve(path))) getOrElse 0
        var logDelay = delay +
          (if (logDelayIterator.hasNext) logDelayIterator.next() else 0.s)

        def path = fileAdded.relativePath

        def logStillWritten(now: MonixDeadline): Unit = {
          if (lastLoggedAt + logDelay <= now) {
            switchLogDelay()
            lastLoggedAt = now
            val file = directory.resolve(path)
            val growth = try {
              val s = size(file)
              val growth = s - lastLoggedSize
              lastLoggedSize = s
              toKBGB(growth)
            } catch {
              case _: IOException => "?"
            }
            logger.info(
              s"Watched file is still being modified for ${(now - since).pretty}: $file +$growth")
          }
        }

        private def switchLogDelay(): Unit =
          if (logDelayIterator.hasNext) {
            logDelay = logDelayIterator.next()
          }

        override def toString = s"$path ${since.elapsed.pretty} delayUntil=$delayUntil"
      }
    })
}

object DirectoryEventDelayer
{
  private val logger = Logger(getClass)

  object syntax {
    implicit final class RichDelayLineObservable(private val self: Observable[DirectoryEvent])
    extends AnyVal {
      def delayFileAdded(directory: Path, delay: FiniteDuration, logDelays: Seq[FiniteDuration])
      : Observable[Seq[DirectoryEvent]] =
        if (delay.isPositive)
          new DirectoryEventDelayer(self, directory, delay, logDelays)
        else
          self.bufferIntrospective(1024)  // Similar to DirectoryEventDelayer, which buffers without limit
    }
  }
}
