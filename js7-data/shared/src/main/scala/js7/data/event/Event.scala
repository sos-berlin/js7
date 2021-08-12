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

  def toShortString = toString

  final def <-:(key: Key) = new KeyedEvent[this.type](key, this)
}
