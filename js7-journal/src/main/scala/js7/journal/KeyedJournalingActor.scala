package js7.journal

import js7.base.generic.Accepted
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait KeyedJournalingActor[S <: JournaledState[S], E <: Event]
  extends JournalingActor[S, E] {
  protected def key: E#Key

  protected final def persistTask[A](event: E, async: Boolean = false)
    (callback: (Stamped[KeyedEvent[E]], S) => A)
  : Task[Checked[A]] =
    persistKeyedEventTask(KeyedEvent[E](key, event), async = async)(callback)

  protected final def persist[EE <: E, A](event: EE, async: Boolean = false)(callback: (EE, S) => A): Future[A] =
    super.persistKeyedEvent(KeyedEvent(key, event), async = async) { (stampedEvent, journaledState) =>
      callback(stampedEvent.value.event.asInstanceOf[EE], journaledState)
    }

  /** Fast lane for events not affecting the journaled state. */
  protected final def persistAcceptEarlyTask[EE <: E, A](
    event: EE,
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[Accepted]] =
    super.persistKeyedEventAcceptEarlyTask(KeyedEvent(key, event) :: Nil, options = options)

  protected final def persistTransaction[EE <: E, A](events: Seq[EE], async: Boolean = false)
    (callback: (Seq[EE], S) => A)
  : Future[A] =
    super.persistKeyedEvents(
      events.map(e => Timestamped(KeyedEvent(key, e))),
      CommitOptions(transaction = true),
      async = async) {
      (stampedEvents, journaledState) => callback(stampedEvents.map(_.value.event.asInstanceOf[EE]), journaledState)
    }

  protected final def persistTransactionReturnChecked[EE <: E, A](
    events: Seq[EE], async: Boolean = false)
    (callback: (Seq[EE], S) => A)
  : Future[Checked[A]] =
    super.persistKeyedEventsReturnChecked(
      events.map(e => Timestamped(KeyedEvent(key, e))),
      CommitOptions(transaction = true),
      async = async) {
      (stampedEvents, journaledState) => callback(stampedEvents.map(_.value.event.asInstanceOf[EE]), journaledState)
    }

  override def toString = s"${ getClass.simpleScalaName }($key)"
}
