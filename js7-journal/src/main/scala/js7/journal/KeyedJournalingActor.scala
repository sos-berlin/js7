package js7.journal

import js7.base.generic.Accepted
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.base.utils.Tests.isTest
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import cats.effect.IO
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait KeyedJournalingActor[S <: JournaledState[S], E <: Event]
  (using protected val E: Event.KeyCompanion[? >: E])
extends JournalingActor[S, E]:

  protected def key: E.Key

  //protected final def persistIO[A](event: E, async: Boolean = false)
  //  (callback: (Stamped[KeyedEvent[E]], S) => A)
  //: IO[Checked[A]] =
  //  persistKeyedEventIO(toKeyedEvent(event), async = async)(callback)

  protected final def persist[EE <: E, A](event: EE, async: Boolean = false)(callback: (EE, S) => A): Future[A] =
    super.persistKeyedEvent(toKeyedEvent(event), async = async) { (stampedEvent, journaledState) =>
      callback(stampedEvent.value.event.asInstanceOf[EE], journaledState)
    }

  /** Fast lane for events not affecting the journaled state. */
  protected final def persistAcceptEarlyIO[EE <: E, A](
    event: EE,
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[Accepted]] =
    super.persistKeyedEventAcceptEarlyIO((toKeyedEvent(event)) :: Nil, options = options)

  protected final def persistTransaction[EE <: E, A](events: Seq[EE], async: Boolean = false)
    (callback: (Seq[EE], S) => A)
  : Future[A] =
    super.persistKeyedEvents(
      events.map(e => Timestamped(toKeyedEvent(e))),
      CommitOptions(transaction = true),
      async = async):
      (stampedEvents, journaledState) => callback(stampedEvents.map(_.value.event.asInstanceOf[EE]), journaledState)

  private def toKeyedEvent(event: E): KeyedEvent[E] =
    if isTest then assert(event.keyCompanion eq E)
    key.asInstanceOf[event.keyCompanion.Key] <-: event

  protected final def persistTransactionReturnChecked[EE <: E, A](
    events: Seq[EE], async: Boolean = false)
    (callback: (Seq[EE], S) => A)
  : Future[Checked[A]] =
    super.persistKeyedEventsReturnChecked(
      events.map(e => Timestamped(key.asInstanceOf[e.keyCompanion.Key/*???*/] <-: e)),
      CommitOptions(transaction = true),
      async = async):
      (stampedEvents, journaledState) => callback(stampedEvents.map(_.value.event.asInstanceOf[EE]), journaledState)

  override def toString = s"${ getClass.simpleScalaName }($key)"
