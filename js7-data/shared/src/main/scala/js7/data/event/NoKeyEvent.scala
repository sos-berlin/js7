package js7.data.event

import js7.data.event.KeyedEvent.NoKey
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait NoKeyEvent extends Event.IsKeyBase[NoKeyEvent] {
  val keyCompanion: NoKeyEvent.type = NoKeyEvent
}

object NoKeyEvent extends Event.CompanionForKey[NoKey, NoKeyEvent] {
  implicit val implicitSelf: NoKeyEvent.type = this

  trait OnlySingleton extends NoKeyEvent

  implicit def toKeyedEvent[E <: NoKeyEvent](event: E): KeyedEvent[E] =
    KeyedEvent(event)
}
