package js7.data.event

import js7.base.utils.Assertions.assertThat
import js7.base.utils.CloseableIterator
import js7.data.event.EventSeq.{Empty, NonEmpty}

/**
  * @author Joacim Zschimmer
  */
sealed trait TearableEventSeq[+M[_], +E]


object TearableEventSeq:
  /** Requested event is no longer available.
    * `tornEventId` is for testing only.
    */
  final case class Torn(after: EventId)
  extends TearableEventSeq[Nothing, Nothing]:
    override def toString = s"EventSeq.Torn($after)"

  implicit final class Closed[E](private val underlying: TearableEventSeq[CloseableIterator, E]) extends AnyVal:
    def close(): Unit = underlying match
      case NonEmpty(stampeds) => stampeds.close()
      case _ =>

    /** Converts a lazy `NonEmpty[Traversable]` to a strict `NonEmpty[Vector]`. */
    def strict: TearableEventSeq[Seq, E] = underlying match
      case Torn(eventId) => Torn(eventId)
      case Empty(eventId) => Empty(eventId)
      case NonEmpty(stampeds) =>
        try NonEmpty(stampeds.toVector)
        finally stampeds.close()



sealed trait EventSeq[+M[_], +E] extends TearableEventSeq[M, E]


object EventSeq:
  final case class NonEmpty[M[_] <: IterableOnce[?], E](stamped: M[Stamped[E]])
  extends EventSeq[M, E]:
    assertThat(stamped.knownSize != 0)

    override def toString = "EventSeq.NonEmpty(" +
      (if stamped.knownSize >= 0 then s"${stamped.knownSize} events" else "lazy") +
      ")"

  final case class Empty(lastEventId: EventId)
  extends EventSeq[Nothing, Nothing]:
    override def toString = s"EventSeq.Empty($lastEventId)"
