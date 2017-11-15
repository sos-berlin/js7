package com.sos.jobscheduler.master.gui.data.event

import io.circe.{Decoder, HCursor}

/**
  * A [[Event]] enriched with a `key` designating the respective object.
  *
  * @author Joacim Zschimmer
  */
final case class KeyedEvent[+E <: Event](key: E#Key, event: E)

object KeyedEvent {
  private[event] val KeyFieldName = "key"

  sealed trait NoKey
  case object NoKey extends NoKey {
    override def toString = "NoKey"
  }

  def apply[E <: Event](event: E)(key: event.Key) = new KeyedEvent(key, event)

  def apply[E <: Event { type Key = NoKey }](event: E) = new KeyedEvent[E](NoKey, event)

  implicit def jsonDecoder[E <: Event](implicit decoder: Decoder[E], keyDecoder: Decoder[E#Key]): Decoder[KeyedEvent[E]] = (c: HCursor) ⇒ {
    val key = c.downField("key").as[E#Key] getOrElse NoKey.asInstanceOf[E#Key]
    for (event ← c.as[E]) yield
      KeyedEvent(key, event)
  }
}
