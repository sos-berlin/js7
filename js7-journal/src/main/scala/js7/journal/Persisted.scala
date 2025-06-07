package js7.journal

import cats.effect.IO
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.event.{Event, EventDrivenState, KeyedEvent, Stamped}
import scala.collection.SeqView
/** Persisted events.
  * @param originalAggregate The aggregate before the events have been applied.
  * @param stampedKeyedEvents
  * @param aggregate The aggregate with applied events. */
final case class Persisted[S <: EventDrivenState[S, E], +E <: Event](
  originalAggregate: S,
  stampedKeyedEvents: Vector[Stamped[KeyedEvent[E]]],
  aggregate: S):

  def keyedEvents: SeqView[KeyedEvent[E]] =
    stampedKeyedEvents.view.map(_.value)

  def isEmpty: Boolean =
    stampedKeyedEvents.isEmpty

  inline def nonEmpty: Boolean =
    !isEmpty

  def checkedSingle: Checked[(Stamped[KeyedEvent[E]], S)] =
    if stampedKeyedEvents.sizeIs != 1 then
      Problem(s"Persisted.checkedSingle: Expected one event, got ${stampedKeyedEvents.size}")
    else
      Right(stampedKeyedEvents.head -> aggregate)

  /** Return the body if an event has been persisted. */
  def ifNonEmpty[U <: Unit](body: IO[U]): IO[Checked[this.type]] =
    if isEmpty then
      IO.right(this)
    else
      body.as(Right(this))

  override def toString =
    s"Persisted(${stampedKeyedEvents.mkString("[", ", ", "]")})"


object Persisted:

  def empty[S <: EventDrivenState[S, E], E <: Event](aggregate: S): Persisted[S, E] =
    Persisted(aggregate, Vector.empty, aggregate)

  extension [S <: EventDrivenState[S, E], E <: Event](io: IO[Checked[Persisted[S, E]]])
    /** Return the body if an event has been persisted. */
    def ifPersisted[U <: Unit](body: Persisted[S, E] => IO[U]): IO[Checked[Persisted[S, E]]] =
      io.flatMapT: persisted =>
        persisted.ifNonEmpty:
          body(persisted)
