package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorLogging, ActorRef, DeadLetterSuppression, Stash}
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec.typeName
import com.sos.jobscheduler.base.generic.Accepted
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.akkautils.SimpleStateActor
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.promiseTask
import com.sos.jobscheduler.core.event.journal.JournalingActor._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import monix.eval.Task
import scala.collection.immutable.{Iterable, Seq}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[E <: Event] extends Actor with Stash with ActorLogging with SimpleStateActor
{
  protected def journalActor: ActorRef
  protected def snapshots: Future[Iterable[Any]]

  private var stashingCount = 0
  private var _persistedEventId = EventId.BeforeFirst

  import context.dispatcher

  become("receive")(receive)

  protected final def persistedEventId = _persistedEventId

  protected final def persistedEventId_=(eventId: EventId) = _persistedEventId = eventId  // TODO Used for recovery, should not be mutable

  override protected def become(stateName: String)(receive: Receive) =
    super.become(stateName)(journaling orElse receive)

  override def preStart() = {
    // FIXME Race condition: A snapshot before JournalActor has received RegisterMe, will not include this Actor
    journalActor ! JournalActor.Input.RegisterMe
    super.preStart()
  }

  // TODO Inhibit bedeutet gehemmt, beeintrichtigt. Besser etwas wie 'stop'
  protected def inhibitJournaling(): Unit = {
    if (stashingCount > 0) throw new IllegalStateException("inhibitJournaling called while a persist operation is active?")
    stashingCount = Inhibited
  }

  protected final def persistKeyedEventTask[A](keyedEvent: KeyedEvent[E])(callback: Stamped[KeyedEvent[E]] => A): Task[A] =
    promiseTask[A] { promise =>
      self ! Persist(keyedEvent, callback, promise)
    }

  protected final def persistKeyedEventAcceptEarly[EE <: E](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    delay: FiniteDuration = Duration.Zero)
  : Future[Accepted] =
    promiseFuture[Accepted] { promise =>
      start(async = true)
      val timestamped = Timestamped(keyedEvent, timestamp) :: Nil
      journalActor.forward(
        JournalActor.Input.Store(timestamped, self, acceptEarly = true, transaction = false,
          delay = delay, alreadyDelayed = Duration.Zero,
          Deferred(async = true, () => promise.success(Accepted))))
    }

  protected final def persistKeyedEvent[EE <: E, A](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    async: Boolean = false)(
    callback: Stamped[KeyedEvent[EE]] => A)
  : Future[A] =
    persistKeyedEvents(Timestamped(keyedEvent, timestamp) :: Nil, async = async) { events =>
      assert(events.size == 1)
      callback(events.head)
    }

  protected final def persistKeyedEvents[EE <: E, A](
    timestamped: Seq[Timestamped[EE]],
    transaction: Boolean = false,
    delay: FiniteDuration = Duration.Zero,
    alreadyDelayed: FiniteDuration = Duration.Zero,
    async: Boolean = false)(
    callback: Seq[Stamped[KeyedEvent[EE]]] => A)
  : Future[A] =
    promiseFuture[A] { promise =>
      start(async = async)
      if (TraceLog && logger.underlying.isTraceEnabled)
        for (t <- timestamped) logger.trace(s"“$toString” Store ${t.keyedEvent.key} <-: ${typeName(t.keyedEvent.event.getClass)}")
      journalActor.forward(
        JournalActor.Input.Store(timestamped, self, acceptEarly = false, transaction = transaction,
          delay = delay, alreadyDelayed = alreadyDelayed,
          EventsCallback(
            async = async,
            stampedSeq => promise.complete(
              try Success(callback(stampedSeq.asInstanceOf[Seq[Stamped[KeyedEvent[EE]]]]))
              catch { case NonFatal(t) =>
                // TODO Ein Fehler sollte zum Abbruch führen? Aber dann?
                logger.error(s"“$toString” ${t.toStringWithCauses}\n" + s"persistKeyedEvents(${timestamped.map(_.keyedEvent)})", t)
                Failure(t)
              }))))
    }

  protected final def defer(callback: => Unit): Unit =
    defer_(async = false, callback)

  protected final def deferAsync(callback: => Unit): Unit =
    defer_(async = true, callback)

  private def defer_(async: Boolean, callback: => Unit): Unit = {
    start(async = async)
    journalActor.forward(JournalActor.Input.Store(Nil, self, acceptEarly = false, transaction = false,
      delay = Duration.Zero, alreadyDelayed = Duration.Zero,
      Deferred(async = async, () => callback)))
  }

  private def start(async: Boolean): Unit = {
    if (stashingCount == Inhibited) throw new IllegalStateException("Journaling is inhibited")  // Avoid deadlock when waiting for response of dead JournalActor
    if (!async) {
      // async = false (default) lets Actor stash all messages but JournalActor.Output.Stored.
      // async = true means, message Store is intermixed with other messages.
      beginStashing()
    }
  }

  protected[journal] def journaling: Receive = {
    case Persist(keyedEvent, callback, promise) =>
      promise.completeWith(
        persistKeyedEvent(keyedEvent)(callback))

    case JournalActor.Output.Stored(stampedSeq, item: Item) =>
      // sender() is from persistKeyedEvent or deferAsync
      stampedSeq.lastOption foreach { last =>
        _persistedEventId = last.eventId
      }
      if (!item.async) {
        endStashing(stampedSeq)
      }
      (stampedSeq, item) match {
        case (stamped, EventsCallback(_, callback)) =>
          if (TraceLog && logger.underlying.isTraceEnabled) for (st <- stamped)
            logger.trace(s"“$toString” Stored ${EventId.toString(st.eventId)} ${st.value.key} <-: ${typeName(st.value.event.getClass)}$stashingCountRemaining")
          callback(stamped.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]])

        case (Nil, Deferred(_, callback)) =>
          if (TraceLog) logger.trace(s"“$toString” Stored (no event)$stashingCountRemaining")
          callback()

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
          callback()

        case _ => sys.error(s"JournalActor.Output.Accepted message does not match item '$item'")
      }

    case Input.GetSnapshot =>
      val sender = this.sender()
      snapshots onComplete {
        case Success(o) =>
          sender ! Output.GotSnapshot(o)
        case Failure(t) =>
          val tt = t.appendCurrentStackTrace
          logger.error(s"“$toString” $t", tt)
          throw tt  // ???
      }

    case _ if stashingCount > 0 =>
      super.stash()
  }

  private def beginStashing(): Unit = {
    stashingCount += 1
    if (stashingCount == 1) {
      context.become(journaling, discardOld = false)
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
      context.unbecome()
      unstashAll()
    }
  }

  private def stashingCountRemaining = if (stashingCount > 0) s", $stashingCount remaining" else ""

  protected def toTimestamped[EE <: E](keyEvents: collection.Iterable[KeyedEvent[EE]]): Seq[Timestamped[EE]] =
    keyEvents.view.map(e => Timestamped(e)).to[Vector]

  protected type Timestamped[+EE <: E] = JournalingActor.Timestamped[EE]

  protected final def Timestamped[EE <: E](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None) =
    JournalingActor.Timestamped(keyedEvent, timestamp)

  private case class EventsCallback(async: Boolean, callback: Seq[Stamped[KeyedEvent[E]]] => Unit) extends Item {
    override def toString = s"EventsCallback(${if (async) "async" else ""})"
  }

  private case class Deferred(async: Boolean, callback: () => Unit) extends Item {
    override def toString = s"Deferred${if (async) "async" else ""}"
  }

  private case class Persist[A](keyedEvent: KeyedEvent[E], callback: Stamped[KeyedEvent[E]] => A, promise: Promise[A])
}

object JournalingActor
{
  private val Inhibited = -1
  private val logger = Logger(getClass)
  private val TraceLog = true

  object Input {
    private[journal] final case object GetSnapshot extends DeadLetterSuppression  // Actor may terminate
  }

  object Output {
    private[journal] final case class GotSnapshot(snapshots: Iterable[Any])
  }

  final case class Timestamped[+E <: Event](keyedEvent: KeyedEvent[E], timestamp: Option[Timestamp] = None)
  extends JournalActor.Timestamped

  private sealed trait Item extends JournalActor.CallersItem {
    def async: Boolean
  }
}
