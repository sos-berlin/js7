package js7.data.event

import js7.base.time.Timestamp

/** Timestamped KeyedEvent. */
final case class TsKeyedEvent[+E <: Event](
  keyedEvent: KeyedEvent[E],
  epochMilli: Long):

  def timestamp: Timestamp =
    Timestamp.ofEpochMilli(epochMilli)

  def toShortString = s"${keyedEvent.toShortString} @ $timestamp"

  override def toString = s"${keyedEvent.toShortString} @ $timestamp"


object TsKeyedEvent:

  extension [E <: Event](maybe: MaybeTsKeyedEvent[E])

    def maybeEpochMilli: Option[Long] =
      maybe match
        case _: KeyedEvent[E] => None
        case o: TsKeyedEvent[E] => Some(o.epochMilli)

    def keyedEvent: KeyedEvent[E] =
      maybe match
        case o: KeyedEvent[E] => o
        case o: TsKeyedEvent[E] => o.keyedEvent

    def toShortString: String =
      maybe match
        case o: KeyedEvent[E] => o.toShortString
        case o: TsKeyedEvent[E] => o.toShortString


/** Maybe timestamped KeyedEvent. */
type MaybeTsKeyedEvent[+E <: Event] = TsKeyedEvent[E] | KeyedEvent[E]

/** Maybe timestamped KeyedEvent. */
object MaybeTsKeyedEvent:
  def apply[E <: Event](keyedEvent: KeyedEvent[E], maybeMillisSinceEpoch: Option[Long])
  : MaybeTsKeyedEvent[E] =
    maybeMillisSinceEpoch match
      case None => keyedEvent
      case Some(o) => TsKeyedEvent(keyedEvent, o)
