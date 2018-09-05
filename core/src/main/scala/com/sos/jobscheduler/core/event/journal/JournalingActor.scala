package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorLogging, ActorRef, DeadLetterSuppression, Stash}
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec.typeName
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.akkautils.SimpleStateActor
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.JournalingActor._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import scala.collection.immutable.{Iterable, Seq}
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[E <: Event] extends Actor with Stash with ActorLogging with SimpleStateActor {

  protected def journalActor: ActorRef
  protected def snapshots: Future[Iterable[Any]]

  private var stashingCount = 0

  import context.dispatcher

  become("receive")(receive)

  protected def become(state: String)(recv: Receive) =
    context.become(journaling orElse recv)

  override def preStart() = {
    journalActor ! JournalActor.Input.RegisterMe
    super.preStart()
  }

  protected def inhibitJournaling(): Unit = {
    if (stashingCount > 0) throw new IllegalStateException("inhibitJournaling while persist operation is active?")
    stashingCount = Inhibited
  }

  private[event] final def persistKeyedEvent[EE <: E, A](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    noSync: Boolean = false,
    async: Boolean = false)(
    callback: Stamped[KeyedEvent[EE]] ⇒ A)
  : Future[A] = {
    promiseFuture[A] { promise ⇒
      start(async = async)
      logger.trace(s"“$toString” Store ${keyedEvent.key} <-: ${typeName(keyedEvent.event.getClass)}")
      journalActor.forward(
        JournalActor.Input.Store(Timestamped(keyedEvent, timestamp) :: Nil, self, noSync = noSync, transaction = false,
          EventCallback(
            async = async,
            event ⇒ promise.complete(
              try Success(callback(event.asInstanceOf[Stamped[KeyedEvent[EE]]]))
              catch { case NonFatal(t) ⇒
                // TODO Ein Fehler sollte zum Abbruch führen?
                logger.debug(s"“$toString” ${t.toStringWithCauses}\n" + s"persistKeyedEvent($keyedEvent)", t)
                Failure(t)
              }))))
    }
  }

  private[event] final def persistKeyedEvents[EE <: E, A](
    timestamped: Seq[Timestamped[EE]],
    noSync: Boolean = false,
    async: Boolean = false,
    transaction: Boolean = false)(
    callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A)
  : Future[A] =
    promiseFuture[A] { promise ⇒
      start(async = async)
      if (logger.underlying.isTraceEnabled)
        for (t ← timestamped) logger.trace(s"“$toString” Store ${t.keyedEvent.key} <-: ${typeName(t.keyedEvent.event.getClass)}")
      journalActor.forward(
        JournalActor.Input.Store(timestamped, self, noSync = noSync, transaction = transaction,
          EventSeqCallback(
            async = async,
            events ⇒ promise.complete(
              try Success(callback(events.asInstanceOf[Seq[Stamped[KeyedEvent[EE]]]]))
              catch { case NonFatal(t) ⇒
                // TODO Ein Fehler sollte zum Abbruch führen?
                logger.debug(s"“$toString” ${t.toStringWithCauses}\n" + s"persistKeyedEvents(${timestamped.map(_.keyedEvent)})", t)
                Failure(t)
              }))))
    }

  protected final def defer(callback: ⇒ Unit): Unit =
    defer_(async = false, callback)

  protected final def deferAsync(callback: ⇒ Unit): Unit =
    defer_(async = true, callback)

  private def defer_(async: Boolean, callback: ⇒ Unit): Unit = {
    start(async = async)
    journalActor.forward(JournalActor.Input.Store(Nil, self, noSync = true, transaction = false, Deferred(async = async, () ⇒ callback)))
  }

  private def start(async: Boolean): Unit = {
    if (stashingCount == Inhibited) throw new IllegalStateException("Journaling is inhibited")  // Avoid deadlock when waiting for response of dead JournalActor
    if (!async) {
      // async = false (default) lets Actor stash all messages but JournalActor.Output.Stored.
      // async = true means, message Store is intermixed with other messages.
      stashingCount += 1
      if (stashingCount == 1) {
        context.become(journaling, discardOld = false)
      }
    }
  }

  protected[journal] def journaling: Receive = {
    case JournalActor.Output.Stored(stampedOptions, item: Item) ⇒
      // sender() is from persistKeyedEvent or deferAsync
      if (!item.async) {
        if (stashingCount == 0) {
          val msg = s"Journal Stored message received (duplicate? stash in callback?) but stashingCount=$stashingCount: $stampedOptions"
          logger.error(s"“$toString” $msg")
          throw new RuntimeException(msg)
        }
        stashingCount -= 1
        if (stashingCount == 0) {
          context.unbecome()
          unstashAll()
        }
      }
      def remaining = if (stashingCount > 0) s", $stashingCount remaining" else ""
      (stampedOptions, item) match {
        case (Seq(stamped), EventCallback(_, callback)) ⇒
          logger.trace(s"“$toString” Stored ${EventId.toString(stamped.eventId)} ${stamped.value.key} <-: ${typeName(stamped.value.event.getClass)}$remaining")
          callback(stamped.asInstanceOf[Stamped[KeyedEvent[E]]])

        case (stamped, EventSeqCallback(_, callback)) ⇒
          if (logger.underlying.isTraceEnabled) for (st ← stamped)
            logger.trace(s"“$toString” Stored ${EventId.toString(st.eventId)} ${st.value.key} <-: ${typeName(st.value.event.getClass)}$remaining")
          callback(stamped.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]])

        case (Nil, Deferred(_, callback)) ⇒
          logger.trace(s"“$toString” Stored (no event)$remaining")
          callback()

        case _ ⇒ sys.error(s"JournalActor.Output.Stored(${stampedOptions.length}×) message does not match item '$item'")
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

  protected def toTimestamped[EE <: E](keyEvents: collection.Iterable[KeyedEvent[EE]]): Seq[Timestamped[EE]] =
    keyEvents.view.map(e ⇒ Timestamped(e)).to[Vector]

  protected type Timestamped[+EE <: E] = JournalingActor.Timestamped[EE]

  protected final def Timestamped[EE <: E](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None) =
    JournalingActor.Timestamped(keyedEvent, timestamp)

  private case class EventCallback(async: Boolean, callback: Stamped[KeyedEvent[E]] ⇒ Unit) extends Item {
    override def toString = s"EventCallback(${if (async) "async" else ""})"
  }

  private case class EventSeqCallback(async: Boolean, callback: Seq[Stamped[KeyedEvent[E]]] ⇒ Unit) extends Item {
    override def toString = s"EventSeqCallback(${if (async) "async" else ""})"
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
