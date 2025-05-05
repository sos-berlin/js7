package js7.journal

import cats.effect.IO
import js7.base.generic.Accepted
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.base.utils.Tests.isStrict
import js7.data.event.{Event, EventCalc, JournaledState, KeyedEvent, Stamped}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait KeyedJournalingActor[S <: JournaledState[S], E <: Event]
  (using protected val E: Event.KeyCompanion[? >: E])
extends JournalingActor[S, E]:

  protected def key: E.Key

  @TestOnly
  protected final def persistEvent[EE <: E, A](event: EE, async: Boolean = false)(callback: (EE, S) => A): Future[A] =
    super.persistKeyedEvent(toKeyedEvent(event), async = async) { (stampedEvent, journaledState) =>
      callback(stampedEvent.value.event.asInstanceOf[EE], journaledState)
    }

  protected final def persist[EE <: E, A](
    keyedEvent: KeyedEvent[EE])
    (callback: (Stamped[KeyedEvent[EE]], S) => A)
  : Future[A] =
    persistKeyedEvent(keyedEvent)(callback)

  /** Fast lane for events not affecting the journaled state. */
  @TestOnly
  protected final def persistAcceptEarlyIO[EE <: E, A](
    event: EE,
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[Accepted]] =
    super.persistKeyedEventAcceptEarlyIO(EventCalc.pure(toKeyedEvent(event)), options = options)

  @TestOnly
  protected final def persistTransaction[EE <: E, A](events: Seq[EE], async: Boolean = false)
    (callback: (Seq[EE], S) => A)
  : Future[A] =
    super.persist(
      EventCalc.pure(events.map(toKeyedEvent)),
      CommitOptions(transaction = true),
      async = async):
      persisted => callback(
        persisted.stampedKeyedEvents.map(_.value.event.asInstanceOf[EE]),
        persisted.aggregate)

  private def toKeyedEvent(event: E): KeyedEvent[E] =
    if isStrict then assert(event.keyCompanion eq E)
    key.asInstanceOf[event.keyCompanion.Key] <-: event

  protected final def persistTransactionReturnChecked[EE <: E, A](
    events: Seq[EE],
    async: Boolean = false)
    (callback: (Seq[EE], S) => A)
  : Future[Checked[A]] =
    super.persistKeyedEventsReturnChecked(
      EventCalc.pure:
        events.map(e => key.asInstanceOf[e.keyCompanion.Key/*???*/] <-: e),
      CommitOptions(transaction = true),
      async = async):
      persisted => callback(
        persisted.stampedKeyedEvents.map(_.value.event.asInstanceOf[EE]),
        persisted.aggregate)

  override def toString = s"${ getClass.simpleScalaName }($key)"
