package js7.journal

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


object Persisted:
  def empty[S <: EventDrivenState[S, E], E <: Event](aggregate: S): Persisted[S, E] =
    Persisted(aggregate, Vector.empty, aggregate)
