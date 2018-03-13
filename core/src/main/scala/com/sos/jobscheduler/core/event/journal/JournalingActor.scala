package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, RichThrowable}
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.JournalingActor._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import scala.collection.immutable.Iterable
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[E <: Event] extends Actor with Stash with ActorLogging {

  protected def journalActor: ActorRef
  protected def snapshots: Future[Iterable[Any]]

  private var stashingCount = 0

  import context.dispatcher

  protected def inhibitJournaling(): Unit = {
    if (stashingCount > 0) throw new IllegalStateException("inhibitJournaling while persist operation is active?")
    stashingCount = Inhibited
  }

  private[event] final def persistAsyncKeyedEvent[EE <: E, A](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ A): Future[A] =
    persistKeyedEvent(keyedEvent, async = true)(callback)

  private[event] final def persistKeyedEvent[EE <: E, A](
    keyedEvent: KeyedEvent[EE],
    timestamp: Option[Timestamp] = None,
    noSync: Boolean = false,
    async: Boolean = false)(
    callback: Stamped[KeyedEvent[EE]] ⇒ A)
  : Future[A] =
    promiseFuture[A] { promise ⇒
      start(async = async)
      logger.trace(s"“$toString” Store ${keyedEvent.key} ${keyedEvent.event.getClass.simpleScalaName}")
      journalActor.forward(
        JournalActor.Input.Store(Some(keyedEvent) :: Nil, self, timestamp, noSync = noSync,
          EventCallback(async = async, event ⇒
            promise complete Try { callback(event.asInstanceOf[Stamped[KeyedEvent[EE]]]) })))
    }

  protected final def defer(callback: ⇒ Unit): Unit =
    defer_(async = false, callback)

  protected final def deferAsync(callback: ⇒ Unit): Unit =
    defer_(async = true, callback)

  private def defer_(async: Boolean, callback: ⇒ Unit): Unit = {
    start(async = async)
    journalActor.forward(JournalActor.Input.Store(None :: Nil, self, timestamp = None, noSync = false,
      Deferred(async = async, () ⇒ callback)))
  }

  private def start(async: Boolean): Unit = {
    if (stashingCount == Inhibited) throw new IllegalStateException("Journaling has been stopped")  // Avoid deadlock when waiting for response of dead JournalActor
    if (!async) {
      // async = false (default) lets Actor stash all messages but JournalActor.Output.Stored.
      // async = true means, message Store is intermixed with other messages.
      stashingCount += 1
      if (stashingCount == 1) {
        context.become(journaling, discardOld = false)
      }
    }
  }

  final def journaling: Receive = {
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
      logger.trace(s"“$toString” Stored, stashingCount=$stashingCount")
      for (stampedOption ← stampedOptions) {
        (stampedOption, item) match {
          case (Some(stamped), EventCallback(_, callback)) ⇒
            logger.trace(s"“$toString” Stored ${EventId.toString(stamped.eventId)} ${stamped.value.key} ${stamped.value.event.getClass.simpleScalaName} -> $item")
            try callback(stamped.asInstanceOf[Stamped[KeyedEvent[E]]])
            catch { case NonFatal(t) ⇒
              logger.error(t.toStringWithCauses, t)
              logger.error(s"Actor stop - $toString")
              context.stop(self)
            }

          case (None, Deferred(_, callback)) ⇒
            callback()

          case x ⇒ sys.error(s"Bad Stored message received: $x")
        }
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

  private case class EventCallback(async: Boolean, callback: Stamped[KeyedEvent[E]] ⇒ Unit) extends Item
  private case class Deferred(async: Boolean, callback: () ⇒ Unit) extends Item
}

object JournalingActor {
  private val Inhibited = -1
  private val logger = Logger(getClass)

  object Input {
    private[journal] final case object GetSnapshot
  }

  object Output {
    private[journal] final case class GotSnapshot(snapshots: Iterable[Any])
  }

  private sealed trait Item extends JournalActor.CallersItem {
    def async: Boolean
  }
}
