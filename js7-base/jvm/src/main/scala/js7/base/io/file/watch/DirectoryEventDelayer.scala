package js7.base.io.file.watch

import cats.data.NonEmptySeq
import java.io.IOException
import java.nio.file.Files.size
import java.nio.file.Path
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileAddedOrModified, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryEventDelayer.*
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixAckFuture
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsUtils.continueWithLast
import monix.execution.Ack.{Continue, Stop}
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
  logDelays: NonEmptySeq[FiniteDuration])
extends Observable[Seq[DirectoryEvent]]:

  def unsafeSubscribeFn(out: Subscriber[Seq[DirectoryEvent]]): Cancelable =
    source.subscribe(new Subscriber[DirectoryEvent] { self =>
      implicit val scheduler: Scheduler = out.scheduler
      private var timer: Option[Cancelable] = None
      private val forwardReentrant = Atomic(0)
      private var callerCompleted = false
      private var hasError = false
      private val isDone = Atomic(false)
      private var ack: Future[Ack] = Continue
      private val indexToEntry = mutable.TreeMap.empty[Long, Entry]  // input queue
      private val pathToIndex = mutable.Map.empty[Path, Long]
      private val outputQueue = new VectorBuilder[DirectoryEvent]
      private var statsAdded = 0L
      private var statsModified = 0L
      private var statsTimer = 0L

      def onNext(directoryEvent: DirectoryEvent) = {
        logger.trace(s"onNext $directoryEvent")

        directoryEvent match {
          case fileAdded @ FileAdded(path) =>
            statsAdded += 1
            self.synchronized {
              for index <- pathToIndex.remove(path) do {
                indexToEntry -= index
              }
              enqueue(new Entry(fileAdded, now))
            }

          case fileModified @ FileModified(path) =>
            statsModified += 1
            self.synchronized {
              pathToIndex.remove(path) match {
                case None =>
                  enqueue(new Entry(fileModified, now))

                case Some(previousIndex) =>
                  for entry <- indexToEntry.remove(previousIndex) do {
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
        if ack == Stop then Stop else Continue
      }

      private def enqueue(entry: Entry): Unit = {
        val isFirst = indexToEntry.isEmpty
        val index = indexToEntry.lastOption.fold(0L)(_._1) + 1
        indexToEntry.update(index, entry)
        pathToIndex(entry.path) = index

        if delay.isZero then {
          forward()
        } else if isFirst then {
          setTimer(delay)
        }
      }

      def onComplete(): Unit = {
        logger.trace("onComplete")
        self.synchronized {
          if indexToEntry.nonEmpty then {
            callerCompleted = true
          } else
            outOnComplete()
        }
      }

      def onError(throwable: Throwable): Unit =
        self.synchronized {
          if !isDone.getAndSet(true) then {
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
          if timer.isEmpty && !hasError then {
            logger.trace(s"⏰ scheduleOnce ${nextDelay.pretty}")
            timer = Some(
              scheduler.scheduleOnce(nextDelay) {
                logger.trace("🔔 Timer event")
                self.synchronized {
                  timer = None
                  statsTimer += 1
                }
                forward()
              })
          }
        }

      @tailrec
      private def forward(): Unit =
        if forwardReentrant.incrementAndGet() == 1 then {
          @tailrec def loop(): Unit = {
            self.synchronized {
              dequeueTo(outputQueue)
            }
            sendOutputQueue()
            self.synchronized {
              if callerCompleted && indexToEntry.isEmpty && outputQueue.isEmpty then {
                outOnComplete()
              }
            }
            if forwardReentrant.decrementAndGet() > 0 then {
              loop()
            }
          }

          loop()

          self.synchronized {
            if timer.isEmpty && !hasError then
              for entry <- indexToEntry.values.headOption yield
                entry.delayUntil.timeLeftOrZero
            else
              None
          } match {
            case None =>
            case Some(ZeroDuration) => forward()
            case Some(nextDelay) => setTimer(nextDelay)
          }
        }

      private def dequeueTo(growable: mutable.Growable[FileAddedOrModified]): Unit =
        if !hasError then {
          val now_ = now

          @tailrec def loop(): Unit =
            indexToEntry.headOption match {
              case Some((index, entry)) if entry.delayUntil <= now_ =>
                indexToEntry -= index
                pathToIndex -= entry.path
                growable += entry.event
                loop()
              case _ =>
            }

          loop()
        }

      private def sendOutputQueue(): Unit =
        ack = ack.syncFlatMapOnContinue {
          val events = self.synchronized {
            val events = outputQueue.result()
            outputQueue.clear()
            events
          }
          if events.isEmpty then
            Continue
          else {
            for event <- events do logger.trace(s"out.onNext $event")
            out.onNext(events)
          }
        }

      private def outOnComplete(): Unit =
        ack.syncTryFlatten.syncOnContinue {
          if !isDone.getAndSet(true) then {
            logger.debug(
              s"$statsAdded×FileAdded · $statsModified×FileModified · $statsTimer timer events")
            try out.onComplete()
            finally timer.foreach(_.cancel())  // Just to be sure
          }
        }

      private final class Entry(
        val event: FileAddedOrModified,
        val since: MonixDeadline)
      {
        var delayUntil = since + delay
        val logDelayIterator = continueWithLast(logDelays)
        var lastLoggedAt = since
        var lastLoggedSize: Long =
          Try(size(directory.resolve(path))) getOrElse 0
        var logDelay = delay + logDelayIterator.next()

        def path = event.relativePath

        def logStillWritten(now: MonixDeadline): Unit =
          if lastLoggedAt + logDelay <= now then {
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

        private def switchLogDelay(): Unit =
          if logDelayIterator.hasNext then {
            logDelay = logDelayIterator.next()
          }

        override def toString = s"$path ${since.elapsed.pretty} delayUntil=$delayUntil"
      }
    })


object DirectoryEventDelayer:
  private val logger = Logger[this.type]

  object syntax:
    implicit final class RichDelayLineObservable(private val self: Observable[DirectoryEvent])
    extends AnyVal:
      def delayFileAdded(
        directory: Path, delay: FiniteDuration, logDelays: NonEmptySeq[FiniteDuration])
      : Observable[Seq[DirectoryEvent]] =
        if delay.isPositive then
          new DirectoryEventDelayer(self, directory, delay, logDelays)
        else
          self.bufferIntrospective(1024)  // Similar to DirectoryEventDelayer, which buffers without limit
