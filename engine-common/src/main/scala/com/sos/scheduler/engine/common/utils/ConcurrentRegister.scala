package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.base.utils.HasKey
import com.sos.scheduler.engine.common.scalautil.{DuplicateKeyException ⇒ BaseDuplicateKeyException, ScalaConcurrentHashMap}
import java.util.concurrent.atomic.AtomicInteger

/**
  * @author Joacim Zschimmer
  */
trait ConcurrentRegister[V <: HasKey] {

  private type Key = V#Key

  private val keyToValue = new ScalaConcurrentHashMap[Key, V] {
    override def default(key: Key) = throwNoSuchId(key)
  }
  private val counter = new AtomicInteger

  final def isEmpty = keyToValue.isEmpty

  final def nonEmpty = keyToValue.nonEmpty

  final def size = keyToValue.size

  final def apply(key: Key) = keyToValue(key)

  final def foreach(body: V ⇒ Unit) = keyToValue.values foreach body

  final def map[A](f: V ⇒ A) = keyToValue.values map f

  final def +=(value: V): Unit = add(value)

  final def add(value: V): Unit = {
    val existingValue = keyToValue.delegate.putIfAbsent(value.key, value)
    if (existingValue != null) throw new DuplicateKeyException(value)
    counter.incrementAndGet()
    onAdded(value)
  }

  def onAdded(value: V) = {}

  final def -=(key: Key): Unit =
    remove(key) match {
      case Some(removed) ⇒ onRemoved(removed)
      case None ⇒
    }

  final def remove(key: Key): Option[V] = keyToValue remove key

  def onRemoved(value: V) = {}

  final def totalCount = counter.get

  protected def throwNoSuchId(key: Key) = throw new NoSuchElementException(s"Unknown '$key'")

  final class DuplicateKeyException(val value: V) extends BaseDuplicateKeyException("'$key' is already registered") {
    def key = value.key
  }
}

object ConcurrentRegister {
  final def apply[V <: HasKey](): ConcurrentRegister[V] = new Standard[V]

  private class Standard[V <: HasKey] extends ConcurrentRegister[V]
}
