package js7.core.event.journal

import js7.base.generic.Accepted
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
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

  protected final def snapshots: Future[Iterable[Any]] =
    Future.successful(snapshot.toList)

  protected final def persistTask[A](event: E, async: Boolean = false)(callback: (Stamped[KeyedEvent[E]], S) => A): Task[Checked[A]] =
    persistKeyedEventTask(KeyedEvent[E](key, event), async = async)(callback)

  protected final def persist[EE <: E, A](event: EE, async: Boolean = false)(callback: (EE, S) => A): Future[A] =
    super.persistKeyedEvent(KeyedEvent(key, event), async = async) { (stampedEvent, journaledState) =>
      callback(stampedEvent.value.event.asInstanceOf[EE], journaledState)
    }

  /** Fast lane for events not affecting the journaled state. */
  protected final def persistAcceptEarly[EE <: E, A](event: EE, delay: FiniteDuration = Duration.Zero): Future[Checked[Accepted]] =
    super.persistKeyedEventAcceptEarly(KeyedEvent(key, event), delay = delay)

  protected final def persistTransaction[EE <: E, A](events: Seq[EE], async: Boolean = false)
    (callback: (Seq[EE], S) => A)
  : Future[A] =
    super.persistKeyedEvents(events.map(e => Timestamped(KeyedEvent(key, e))), async = async, transaction = true) {
      (stampedEvents, journaledState) => callback(stampedEvents.map(_.value.event.asInstanceOf[EE]), journaledState)
    }

  override def toString = s"${getClass.simpleScalaName}($key)"
}
