package js7.base.io.file.watch

import java.nio.file.Path
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryEventDelayer.*
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixAckFuture
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.time.ScalaTime.*
import monix.execution.Ack.{Continue, Stop}
import monix.execution.atomic.Atomic
import monix.execution.{Ack, Cancelable}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.*

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
private final class DirectoryEventDelayer(source: Observable[DirectoryEvent], delay: FiniteDuration)
extends Observable[Seq[DirectoryEvent]]
{
  def unsafeSubscribeFn(out: Subscriber[Seq[DirectoryEvent]]): Cancelable =
    source.subscribe(new Subscriber[DirectoryEvent] { self =>
      implicit val scheduler = out.scheduler
      private val roundUp = (delay / 10) max 10.ms min 1.s
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
        directoryEvent match {
          case fileAdded: FileAdded =>
            self.synchronized {
              for (index <- pathToIndex.remove(fileAdded.relativePath)) {
                indexToEntry -= index
              }
              val first = indexToEntry.isEmpty
              enqueue(fileAdded)
              if (first) setTimer(delay)
            }

          case FileModified(path) =>
            self.synchronized {
              for (previousIndex <- pathToIndex.remove(path)) {
                for (entry <- indexToEntry.remove(previousIndex)) {
                  enqueue(entry.fileAdded)
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
        Continue
      }

      private def enqueue(fileAdded: FileAdded): Unit = {
        val index = indexToEntry.lastOption.fold(0L)(_._1) + 1
        indexToEntry.update(index, Entry(fileAdded, now + delay))
        pathToIndex(fileAdded.relativePath) = index
        logger.trace(s"#$index + $fileAdded")

        if (delay.isZero) {
          forward()
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

          nextTimerDelay() match {
            case None =>
            case Some(ZeroDuration) => forward()
            case Some(nextDelay) => setTimer(nextDelay)
          }
        }

      private def dequeueTo(growable: mutable.Growable[FileAdded]): Unit =
        if (!hasError) {
          val now_ = now

          @tailrec def loop(): Unit = {
            indexToEntry.headOption match {
              case Some((index, entry)) if entry.delayUntil <= now_ =>
                logger.trace(s"#$index - ${entry.fileAdded}")
                indexToEntry -= index
                pathToIndex -= entry.fileAdded.relativePath
                growable += entry.fileAdded
                loop()
              case _ =>
            }
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
            logger.trace(s"forward ${events.size} events")
            out.onNext(events)
          }
        }

      private def nextTimerDelay(): Option[FiniteDuration] =
        self.synchronized {
          if (timer.isEmpty && !hasError)
            for (entry <- indexToEntry.values.headOption) yield
              entry.delayUntil.timeLeftOrZero.roundUpToNext(roundUp)
          else
            None
        }

      private def setTimer(nextDelay: FiniteDuration): Unit =
        self.synchronized {
          if (timer.isEmpty && !hasError) {
            logger.trace(s"⏰ scheduleOnce ${nextDelay.pretty}")
            timer = Some(
              scheduler.scheduleOnce(nextDelay) {
                self.synchronized {
                  timer = None
                  timerCount += 1
                }
                forward()
              })
          }
        }

      def onComplete(): Unit =
        self.synchronized {
          if (indexToEntry.nonEmpty) {
            callerCompleted = true
          } else
            outOnComplete()
        }

      private def outOnComplete(): Unit =
        ack.syncTryFlatten.syncOnContinue {
          if (!isDone.getAndSet(true)) {
            logger.debug(s"$timerCount timer events")
            try out.onComplete()
            finally timer.foreach(_.cancel())  // Just to be sure
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
    })
}

object DirectoryEventDelayer
{
  private val logger = Logger(getClass)
  private case class Entry(fileAdded: FileAdded, delayUntil: MonixDeadline)

  object syntax {
    implicit final class RichDelayLineObservable(private val self: Observable[DirectoryEvent])
    extends AnyVal {
      def delayFileAdded(delay: FiniteDuration): Observable[Seq[DirectoryEvent]] = {
        if (delay.isPositive)
          new DirectoryEventDelayer(self, delay)
        else
          self.bufferIntrospective(1024)  // Similar to DirectoryEventDelayer, which buffers without limit
      }
    }
  }
}
