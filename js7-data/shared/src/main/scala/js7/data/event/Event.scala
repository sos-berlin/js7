package js7.data.event

import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import scala.annotation.targetName

/**
  * An object's event.
  * <p>
  *   See [[KeyedEvent]] for the key of object.
  *
  * @author Joacim Zschimmer
  */
trait Event:
  type KeyBase <: Event

  val keyCompanion: Event.KeyCompanion[KeyBase]

  def isSucceeded: Boolean = true

  def toShortString = toString

  @targetName("toKeyedEvent")
  final def <-:(key: keyCompanion.Key): KeyedEvent[this.type] =
    new KeyedEvent(this)(key)

  /** Ignores the key's type and returns an AnyKeyedEvent. */
  @targetName("toAnyKeyedEvent")
  final def <~:(key: Any): AnyKeyedEvent =
    KeyedEvent.any(key, this)

  def isMinor = false


object Event:
  /** This trait (but not it's derived traits) is the common trait of Events with a common key. */
  transparent trait IsKeyBase[E <: IsKeyBase[E]] extends Event:
    final type KeyBase = E

  trait KeyCompanion[E <: Event]:
    type Key

    implicit def implicitSelf: KeyCompanion[E]

    override def toString = getClass.shortClassName

  transparent trait CompanionForKey[K, E <: Event] extends KeyCompanion[E]:
    type Key = K
