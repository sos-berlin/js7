package js7.data.event

import js7.data.event.KeyedEvent.NoKey
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait NoKeyEvent extends Event.IsKeyBase[NoKeyEvent]:
  val keyCompanion: NoKeyEvent.type = NoKeyEvent


object NoKeyEvent extends Event.CompanionForKey[NoKey, NoKeyEvent]:
  given implicitSelf: NoKeyEvent.type = this

  implicit def toKeyedEvent[E <: NoKeyEvent](event: E): KeyedEvent[E] =
    NoKey <-: event
  //Usage makes compiler warn: Use of implicit conversion given instance given_Conversion_E_KeyedEvent in object NoKeyEvent should be enabled
  //given [E <: NoKeyEvent] => Conversion[E, KeyedEvent[E]] =
  //  NoKey <-: _
