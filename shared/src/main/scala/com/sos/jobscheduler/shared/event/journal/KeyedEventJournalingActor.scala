package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait KeyedEventJournalingActor[E <: Event] extends JournalingActor[E] {

  override def preStart() = {
    journalActor ! JournalActor.Input.RegisterMe(None)
    super.preStart()
  }

  protected final def persistFuture[EE <: E, A](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ A): Future[A] =
    promiseFuture[A] { promise ⇒
      persist(keyedEvent) { event ⇒
        promise.success(callback(event))  // Exception should not be handled in Future
      }
    }

  protected final def persistAsync[EE <: E](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ Unit): Unit =
    persist(keyedEvent, async = true)(callback)

  protected final def persist[EE <: E](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None, async: Boolean = false)
    (callback: Stamped[KeyedEvent[EE]] ⇒ Unit)
  : Unit =
    super.persistKeyedEvent(keyedEvent, timestamp, async)(callback)
}
