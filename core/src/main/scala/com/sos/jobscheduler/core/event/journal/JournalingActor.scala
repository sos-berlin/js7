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
import com.sos.jobscheduler.core.event.journal.JournalingActor._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import scala.collection.immutable.{Iterable, Seq}
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[E <: Event] extends Actor with Stash with ActorLogging with SimpleStateActor {

  protected def journalActor: ActorRef
  protected def snapshots: Future[Iterable[Any]]

  private var stashingCount = 0
  private var _actorStateName: String = ""
  private var _persistedEventId = EventId.BeforeFirst

  import context.dispatcher

  become("receive")(receive)

  final def actorStateName = _actorStateName

  final def persistedEventId = _persistedEventId

  final def persistedEventId_=(eventId: EventId) = _persistedEventId = eventId  // TODO Used for recovery, should not be mutable

  protected def become(stateName: String)(recv: Receive) = {
    _actorStateName = stateName
    context.become(journaling orElse recv)
  }

  override def preStart() = {
    journalActor ! JournalActor.Input.RegisterMe
    super.preStart()
  }

  protected def inhibitJournaling(): Unit = {
    if (stashingCount > 0) throw new IllegalStateException("inhibitJournaling while persist operation is active?")
    stashingCount = Inhibited
  }

  private[event] final def persistKeyedEventAcceptEarly[EE <: E](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    delay: FiniteDuration = Duration.Zero)
  : Future[Accepted] =
    promiseFuture[Accepted] { promise ⇒
      start(async = true)
      val timestamped = Timestamped(keyedEvent, timestamp) :: Nil
      journalActor.forward(
        JournalActor.Input.Store(timestamped, self, acceptEarly = true, transaction = false, delay = delay,
          Deferred(async = true, () ⇒ promise.success(Accepted))))
    }

  private[event] final def persistKeyedEvent[EE <: E, A](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    async: Boolean = false)(
    callback: Stamped[KeyedEvent[EE]] ⇒ A)
  : Future[A] =
    persistKeyedEvents(Timestamped(keyedEvent, timestamp) :: Nil, async = async) { events ⇒
      assert(events.size == 1)
      callback(events.head)
    }

  private[event] final def persistKeyedEvents[EE <: E, A](
    timestamped: Seq[Timestamped[EE]],
    transaction: Boolean = false,
    delay: FiniteDuration = Duration.Zero,
    async: Boolean = false)(
    callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A)
  : Future[A] =
    promiseFuture[A] { promise ⇒
      start(async = async)
      if (logger.underlying.isTraceEnabled)
        for (t ← timestamped) logger.trace(s"“$toString” Store ${t.keyedEvent.key} <-: ${typeName(t.keyedEvent.event.getClass)}")
      journalActor.forward(
        JournalActor.Input.Store(timestamped, self, acceptEarly = false, transaction = transaction, delay = delay,
          EventsCallback(
            async = async,
            events ⇒ promise.complete(
              try Success(callback(events.asInstanceOf[Seq[Stamped[KeyedEvent[EE]]]]))
              catch { case NonFatal(t) ⇒
                // TODO Ein Fehler sollte zum Abbruch führen? Aber dann?
                logger.error(s"“$toString” ${t.toStringWithCauses}\n" + s"persistKeyedEvents(${timestamped.map(_.keyedEvent)})", t)
                Failure(t)
              }))))
    }

  protected final def defer(callback: ⇒ Unit): Unit =
    defer_(async = false, callback)

  protected final def deferAsync(callback: ⇒ Unit): Unit =
    defer_(async = true, callback)

  private def defer_(async: Boolean, callback: ⇒ Unit): Unit = {
    start(async = async)
    journalActor.forward(JournalActor.Input.Store(Nil, self, acceptEarly = false, transaction = false, delay = Duration.Zero,
      Deferred(async = async, () ⇒ callback)))
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
    case JournalActor.Output.Stored(stampedSeq, item: Item) ⇒
      // sender() is from persistKeyedEvent or deferAsync
      stampedSeq.lastOption foreach { last ⇒
        _persistedEventId = last.eventId
      }
      if (!item.async) {
        endStashing(stampedSeq)
      }
      (stampedSeq, item) match {
        case (stamped, EventsCallback(_, callback)) ⇒
          if (logger.underlying.isTraceEnabled) for (st ← stamped)
            logger.trace(s"“$toString” Stored ${EventId.toString(st.eventId)} ${st.value.key} <-: ${typeName(st.value.event.getClass)}$stashingCountRemaining")
          callback(stamped.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]])

        case (Nil, Deferred(_, callback)) ⇒
          logger.trace(s"“$toString” Stored (no event)$stashingCountRemaining")
          callback()

        case _ ⇒ sys.error(s"JournalActor.Output.Stored(${stampedSeq.length}×) message does not match item '$item'")
      }

    case JournalActor.Output.Accepted(item: Item) ⇒
      // sender() is from persistKeyedEvent or deferAsync
      if (!item.async) {
        endStashing(Nil)
      }
      item match {
        case Deferred(_, callback) ⇒
          logger.trace(s"“$toString” Stored (events are written, not flushed)$stashingCountRemaining")
          callback()

        case _ ⇒ sys.error(s"JournalActor.Output.Accepted message does not match item '$item'")
      }

    case Input.GetSnapshot ⇒
      val sender = this.sender()
      snapshots onComplete {
        case Success(o) ⇒
          sender ! Output.GotSnapshot(o)
        case Failure(t) ⇒
          val tt = t.appendCurrentStackTrace
          logger.error(s"“$toString” $t", tt)
          throw tt  // ???
      }

    case _ if stashingCount > 0 ⇒
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
    keyEvents.view.map(e ⇒ Timestamped(e)).to[Vector]

  protected type Timestamped[+EE <: E] = JournalingActor.Timestamped[EE]

  protected final def Timestamped[EE <: E](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None) =
    JournalingActor.Timestamped(keyedEvent, timestamp)

  private case class EventsCallback(async: Boolean, callback: Seq[Stamped[KeyedEvent[E]]] ⇒ Unit) extends Item {
    override def toString = s"EventsCallback(${if (async) "async" else ""})"
  }

  private case class Deferred(async: Boolean, callback: () ⇒ Unit) extends Item {
    override def toString = s"Deferred${if (async) "async" else ""}"
  }
}

object JournalingActor {
  private val Inhibited = -1
  private val logger = Logger(getClass)

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
