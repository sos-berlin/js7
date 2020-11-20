package js7.core.event.journal

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import js7.base.circeutils.typed.TypedJsonCodec.typeName
import js7.base.generic.Accepted
import js7.base.monixutils.MonixBase.syntax.RichScheduler
import js7.base.problem.{Checked, ProblemException}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.common.akkautils.ReceiveLoggingActor
import js7.common.scalautil.Futures.promiseFuture
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.promiseTask
import js7.core.event.journal.JournalingActor._
import js7.data.event.{AnyKeyedEvent, Event, EventId, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, Scheduler}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[S <: JournaledState[S], E <: Event]
extends Actor with Stash with ActorLogging with ReceiveLoggingActor
{
  protected def journalActor: ActorRef @@ JournalActor.type
  protected def journalConf: JournalConf
  protected def scheduler: Scheduler

  private var stashingCount = 0
  private val persistStatistics = new PersistStatistics
  private var _persistedEventId = EventId.BeforeFirst
  private var journalingTimer = SerialCancelable()

  become("receive")(receive)

  protected final def persistedEventId = _persistedEventId

  protected final def persistedEventId_=(eventId: EventId) = _persistedEventId = eventId  // TODO Used for recovery, should not be mutable

  override protected def become(stateName: String)(receive: Receive) =
    super.become(stateName)(journaling orElse receive)

  override def postStop(): Unit = {
    journalingTimer.cancel()
    super.postStop()
  }

  // TODO Inhibit bedeutet gehemmt, beeintrichtigt. Besser etwas wie 'stop'
  protected def inhibitJournaling(): Unit = {
    if (stashingCount > 0) throw new IllegalStateException("inhibitJournaling called while a persist operation is active?")
    stashingCount = Inhibited
  }

  protected final def persistKeyedEventTask[A](keyedEvent: KeyedEvent[E], async: Boolean = false)
    (callback: (Stamped[KeyedEvent[E]], S) => A)
  : Task[Checked[A]] =
    promiseTask[Checked[A]] { promise =>
      self ! Persist(keyedEvent, async = async, callback, promise)
    }

  /** Fast lane for events not affecting the journaled state. */
  protected final def persistKeyedEventAcceptEarly[EE <: E](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    delay: FiniteDuration = Duration.Zero)
  : Future[Checked[Accepted]] =
    promiseFuture[Checked[Accepted]] { promise =>
      start(async = true, "persistKeyedEventAcceptEarly")
      val timestamped = Timestamped(keyedEvent, timestamp) :: Nil
      journalActor.forward(
        JournalActor.Input.Store(timestamped, self, acceptEarly = true, transaction = false,
          delay = delay, alreadyDelayed = Duration.Zero, since = now,
          Deferred(async = true, checked => promise.success(checked))))
    }

  protected final def persistKeyedEvent[EE <: E, A](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    async: Boolean = false)(
    callback: (Stamped[KeyedEvent[EE]], S) => A)
  : Future[A] =
    persistKeyedEvents(Timestamped(keyedEvent, timestamp) :: Nil, async = async) { (events, journaledState) =>
      assertThat(events.sizeIs == 1)
      callback(events.head, journaledState)
    }

  protected final def persistKeyedEvents[EE <: E, A](
    timestamped: Seq[Timestamped[EE]],
    transaction: Boolean = false,
    delay: FiniteDuration = Duration.Zero,
    alreadyDelayed: FiniteDuration = Duration.Zero,
    async: Boolean = false)(
    callback: (Seq[Stamped[KeyedEvent[EE]]], S) => A)
  : Future[A] =
    promiseFuture[A] { promise =>
      start(async = async, timestamped.headOption.fold("empty")(_.keyedEvent.event.getClass.simpleScalaName))
      if (TraceLog && logger.underlying.isTraceEnabled)
        for (t <- timestamped) logger.trace(s"“$toString” Store ${t.keyedEvent.key} <-: ${typeName(t.keyedEvent.event.getClass)}")
      journalActor.forward(
        JournalActor.Input.Store(timestamped, self, acceptEarly = false, transaction = transaction,
          delay = delay, alreadyDelayed = alreadyDelayed, since = now,
          EventsCallback[S](
            async = async,
            (stampedSeq, journaledState) => promise.complete(
              try Success(callback(stampedSeq.asInstanceOf[Seq[Stamped[KeyedEvent[EE]]]], journaledState))
              catch { case NonFatal(t) =>
                // TODO Ein Fehler sollte zum Abbruch führen? Aber dann?
                logger.error(s"“$toString” ${t.toStringWithCauses}\n" + s"persistKeyedEvents(${timestamped.map(_.keyedEvent)})", t)
                throw t
                //Failure(t)
              }))))
    }

  protected final def defer(callback: => Unit): Unit =
    defer_(async = false, callback)

  protected final def deferAsync(callback: => Unit): Unit =
    defer_(async = true, callback)

  private def defer_(async: Boolean, callback: => Unit): Unit = {
    start(async = async, "defer")
    journalActor.forward(
      JournalActor.Input.Store(Nil, self, acceptEarly = false, transaction = false,
        delay = Duration.Zero, alreadyDelayed = Duration.Zero, since = now,
        Deferred(async = async, {
          case Left(problem) => throw problem.throwable.appendCurrentStackTrace
          case Right(Accepted) => callback
        })))
  }

  private def start(async: Boolean, firstName: => String): Unit = {
    if (stashingCount == Inhibited) throw new IllegalStateException("Journaling is inhibited")  // Avoid deadlock when waiting for response of dead JournalActor
    if (!async) {
      // async = false (default) lets Actor stash all messages but JournalActor.Output.Stored.
      // async = true means, message Store is intermixed with other messages.
      beginStashing(firstName)
    }
  }

  protected[journal] def journaling: Receive = {
    case Persist(keyedEvent, async, callback, promise) =>
      promise.completeWith(
        persistKeyedEvent(keyedEvent, async = async)((stampedEvents, state) =>
          try Right(callback(stampedEvents, state))
          catch {
            case ProblemException(problem) => Left(problem)
            case t: Throwable => throw t.appendCurrentStackTrace
          }))

    case JournalActor.Output.Stored(stampedSeq, journaledState, item: Item) =>
      // sender() is from persistKeyedEvent or deferAsync
      persistStatistics.onStored(stampedSeq.size)
      stampedSeq.lastOption foreach { last =>
        _persistedEventId = last.eventId
      }
      if (!item.async) {
        endStashing(stampedSeq)
      }
      (stampedSeq, item) match {
        case (_, eventsCallback: EventsCallback[S]) =>
          if (TraceLog && logger.underlying.isTraceEnabled) for (st <- stampedSeq)
            logger.trace(s"“$toString” Stored ${EventId.toString(st.eventId)} ${st.value.key} <-: ${typeName(st.value.event.getClass)}$stashingCountRemaining")
          eventsCallback.callback(stampedSeq.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]], journaledState.asInstanceOf[S])

        case (Nil, Deferred(_, callback)) =>
          if (TraceLog) logger.trace(s"“$toString” Stored (no event)$stashingCountRemaining")
          callback(Right(Accepted))

        case _ => sys.error(s"JournalActor.Output.Stored(${stampedSeq.length}×) message does not match item '$item'")
      }

    case JournalActor.Output.Accepted(item: Item) =>
      // sender() is from persistKeyedEvent or deferAsync
      if (!item.async) {
        endStashing(Nil)
      }
      item match {
        case Deferred(_, callback) =>
          if (TraceLog) logger.trace(s"“$toString” Stored (events are written, not flushed)$stashingCountRemaining")
          callback(Right(Accepted))

        case _ => sys.error(s"JournalActor.Output.Accepted message does not match item '$item'")
      }

    case JournalActor.Output.StoreFailure(problem, item) =>
      // Let the calling Actor crash ???
      logger.warn(s"“$toString” Event could not be stored: $problem")
      throw problem.throwable.appendCurrentStackTrace

    case msg if stashingCount > 0 =>
      if (TraceLog) logger.trace(s"“$toString” Still waiting for event commit: stash $msg")
      super.stash()
  }

  private def beginStashing(firstName: => String): Unit = {
    stashingCount += 1
    if (stashingCount == 1) {
      persistStatistics.beginStashing(firstName)
      logBecome("journaling")
      context.become(journaling, discardOld = false)
      logger.whenWarnEnabled {
        val since = now
        journalingTimer := scheduler.scheduleAtFixedRates(journalConf.persistWarnDurations) {
          // Under load it may be normal to be busy for some time ???
          logger.warn(s"“$toString” is still busy with persisting for ${since.elapsed.pretty} ($stashingCount persist operations in progress)")
        }
      }
    }
  }

  private def endStashing(stamped: Seq[Stamped[AnyKeyedEvent]]): Unit = {
    if (stashingCount == 0) {
      val msg = s"Journal Stored message received (duplicate? stash in callback?) but stashingCount=$stashingCount: $stamped"
      logger.error(s"“$toString” $msg")
      throw new RuntimeException(msg)
    }
    stashingCount -= 1
    if (stashingCount == 0) {
      journalingTimer := Cancelable.empty
      unstashAll()
      persistStatistics.endStashing()
      logger.trace(s"“$toString” unbecome")
      context.unbecome()
    }
  }

  private def stashingCountRemaining = (stashingCount > 0) ?? s", $stashingCount remaining"

  protected def toTimestamped[EE <: E](keyEvents: Iterable[KeyedEvent[EE]]): Seq[Timestamped[EE]] =
    keyEvents.view.map(e => Timestamped(e)).toVector

  protected type Timestamped[+EE <: E] = JournalingActor.Timestamped[EE]

  protected final def Timestamped[EE <: E](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None) =
    JournalingActor.Timestamped(keyedEvent, timestamp)

  private case class EventsCallback[S <: JournaledState[S]](
    async: Boolean,
    callback: (Seq[Stamped[KeyedEvent[E]]], S) => Unit)
  extends Item {
    override def toString = s"EventsCallback(${async ?? "async"})"
  }

  private case class Deferred(async: Boolean, callback: Checked[Accepted] => Unit) extends Item {
    override def toString = s"Deferred${async ?? "async"}"
  }

  private case class Persist[A](
    keyedEvent: KeyedEvent[E],
    async: Boolean = false,
    callback: (Stamped[KeyedEvent[E]], S) => A,
    promise: Promise[Checked[A]])

  private class PersistStatistics {
    private var persistStartedAt = now/*dummy*/
    private var persistCount = 0
    private var eventCount = 0
    private var firstName = ""

    def beginStashing(firstName: String): Unit = {
      this.firstName = firstName
      persistStartedAt = now
      eventCount = 0
      persistCount = 0
    }

    def onStored(eventCount: Int): Unit = {
      persistCount += 1
      this.eventCount += eventCount
    }

    def endStashing(): Unit = {
      val duration = persistStartedAt.elapsed
      if (duration >= BigStoreThreshold) {
        logger.debug(s"“${JournalingActor.this.toString}” Long persist completed ($persistCount×, $firstName ...) - " +
          itemsPerSecondString(duration, eventCount, "events"))
      }
    }
  }
}

object JournalingActor
{
  private val Inhibited = -1
  private val logger = Logger(getClass)
  private val TraceLog = true
  private val BigStoreThreshold = 1.s

  final case class Timestamped[+E <: Event](keyedEvent: KeyedEvent[E], timestamp: Option[Timestamp] = None)
  extends JournalActor.Timestamped

  private sealed trait Item extends JournalActor.CallersItem {
    def async: Boolean
  }
}
