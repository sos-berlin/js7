package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.generic.Accepted
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * @author Joacim Zschimmer
  */
trait KeyedJournalingActor[S <: JournaledState[S], E <: Event]
extends JournalingActor[S, E]
{
  protected def key: E#Key
  protected def snapshot: Option[Any]
  protected def recoverFromSnapshot(snapshot: Any): Unit
  protected def recoverFromEvent(event: E): Unit
  protected def finishRecovery() = {}

  protected final def snapshots: Future[Iterable[Any]] =
    Future.successful(snapshot.toList)

  protected final def persistTask[A](event: E)(callback: (Stamped[KeyedEvent[E]], S) => A): Task[A] =
    persistKeyedEventTask(KeyedEvent[E](key, event))(callback)

  protected final def persist[EE <: E, A](event: EE, async: Boolean = false)(callback: (EE, S) => A): Future[A] =
    super.persistKeyedEvent(KeyedEvent(key, event), async = async) { (stampedEvent, journaledState) =>
      callback(stampedEvent.value.event.asInstanceOf[EE], journaledState)
    }

  /** Fast lane for events not affecting the journaled state. */
  protected final def persistAcceptEarly[EE <: E, A](event: EE, delay: FiniteDuration = Duration.Zero): Future[Accepted] =
    super.persistKeyedEventAcceptEarly(KeyedEvent(key, event), delay = delay)

  protected final def persistTransaction[EE <: E, A](events: Seq[EE], async: Boolean = false)
    (callback: (Seq[EE], S) => A)
  : Future[A] =
    super.persistKeyedEvents(events.map(e => Timestamped(KeyedEvent(key, e))), async = async, transaction = true) {
      (stampedEvents, journaledState) => callback(stampedEvents.map(_.value.event.asInstanceOf[EE]), journaledState)
    }

  override def journaling = super.journaling orElse {
    case Input.RecoverFromSnapshot(o) =>
      recoverFromSnapshot(o)

    case Input.RecoverFromEvent(Stamped(_, _, KeyedEvent(k, event))) =>
      assert(k == key)
      recoverFromEvent(event.asInstanceOf[E])

    case Input.FinishRecovery =>
      callFinishRecovery()
      sender() ! KeyedJournalingActor.Output.RecoveryFinished
  }

  private def callFinishRecovery(): Unit = {
    finishRecovery()
    val snapshot = this.snapshot
    if (snapshot == null) sys.error(s"Actor (${getClass.getSimpleName}) for '$key': snapshot must not be null")
  }

  override def toString = s"${getClass.simpleScalaName}($key)"
}

object KeyedJournalingActor {
  object Input {
    final case class RecoverFromSnapshot(snapshot: Any)
    final case class RecoverFromEvent(eventStamped: Stamped[AnyKeyedEvent])
    final case object FinishRecovery
  }

  object Output {
    private[journal] case object RecoveryFinished
  }
}
