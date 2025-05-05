package js7.journal

import js7.base.problem.{Checked, Problem}
import js7.data.event.{Event, EventDrivenState, KeyedEvent, Stamped}
import scala.collection.SeqView
/** Persisted events.
  * @param originalAggregate The aggregate before the events have been applied.
  * @param stampedKeyedEvents
  * @param aggregate The aggregate with applied events. */
final case class Persisted[S <: EventDrivenState[S, E], E <: Event](
  originalAggregate: S,
  stampedKeyedEvents: Vector[Stamped[KeyedEvent[E]]],
  aggregate: S):

  def keyedEvents: SeqView[KeyedEvent[E]] =
    stampedKeyedEvents.view.map(_.value)

  def isEmpty: Boolean =
    stampedKeyedEvents.isEmpty

  def checkedSingle: Checked[(Stamped[KeyedEvent[E]], S)] =
    if stampedKeyedEvents.sizeIs != 1 then
      Problem(s"Persisted.checkedSingle: Expected one event, got ${stampedKeyedEvents.size}")
    else
      Right(stampedKeyedEvents.head -> aggregate)

object Persisted:
  def empty[S <: EventDrivenState[S, E], E <: Event](aggregate: S): Persisted[S, E] =
    Persisted(aggregate, Vector.empty, aggregate)
