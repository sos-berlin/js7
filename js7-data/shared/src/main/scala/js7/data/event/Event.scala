package js7.data.event

/**
  * An object's event.
  * <p>
  *   See [[KeyedEvent]] for the key of object.
  *
  * @author Joacim Zschimmer
  */
trait Event {
  /**
    * The type of the key in [[KeyedEvent]] or [[KeyedEvent.NoKey]].
    */
  type Key

  def isFailed: Boolean = false

  def toShortString = toString

  final def <-:(key: Key) =
    new KeyedEvent[this.type](key, this)
}

object Event
{
  trait ForScala3[E <: Event] extends Event {
    implicit val companion: Companion[E]
    type Key = companion.Key
  }

  trait Companion[E <: Event]
  {
    type Key
  }
}
