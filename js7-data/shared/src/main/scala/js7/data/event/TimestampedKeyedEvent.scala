package js7.data.event

import js7.base.time.Timestamp

final case class TimestampedKeyedEvent[+E <: Event](
  keyedEvent: KeyedEvent[E],
  millisSinceEpoch: Long):

  def timestamp: Timestamp =
    Timestamp.ofEpochMilli(millisSinceEpoch)

  def toShortString = s"${keyedEvent.toShortString} @ $timestamp"

  override def toString = s"${keyedEvent.toShortString} @ $timestamp"


object TimestampedKeyedEvent:

  extension [E <: Event](maybe: MaybeTimestampedKeyedEvent[E])

    def maybeMillisSinceEpoch: Option[Long] =
      maybe match
        case _: KeyedEvent[E] => None
        case o: TimestampedKeyedEvent[E] => Some(o.millisSinceEpoch)

    def keyedEvent: KeyedEvent[E] =
      maybe match
        case o: KeyedEvent[E] => o
        case o: TimestampedKeyedEvent[E] => o.keyedEvent

    def toShortString: String =
      maybe match
        case o: KeyedEvent[E] => o.toShortString
        case o: TimestampedKeyedEvent[E] => o.toShortString


type MaybeTimestampedKeyedEvent[+E <: Event] = TimestampedKeyedEvent[E] | KeyedEvent[E]

object MaybeTimestampedKeyedEvent:
  def apply[E <: Event](keyedEvent: KeyedEvent[E], maybeMillisSinceEpoch: Option[Long])
  : MaybeTimestampedKeyedEvent[E] =
    maybeMillisSinceEpoch match
      case None => keyedEvent
      case Some(o) => TimestampedKeyedEvent(keyedEvent, o)
