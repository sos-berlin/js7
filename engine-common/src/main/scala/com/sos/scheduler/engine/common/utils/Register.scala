package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.base.utils.HasKey
import com.sos.scheduler.engine.common.scalautil.{DuplicateKeyException, ScalaConcurrentHashMap}
import java.util.concurrent.atomic.AtomicInteger

/**
  * @author Joacim Zschimmer
  */
trait Register[V <: HasKey] {

  private type Id = V#Key

  private val keyToValue = new ScalaConcurrentHashMap[Id, V] {
    override def default(key: Id) = throwNoSuchId(key)
  }
  private val counter = new AtomicInteger

  final def isEmpty = keyToValue.isEmpty

  final def nonEmpty = keyToValue.nonEmpty

  final def size = keyToValue.size

  final def apply(key: Id) = keyToValue(key)

  final def foreach(body: V ⇒ Unit) = keyToValue.values foreach body

  final def map[A](f: V ⇒ A) = keyToValue.values map f

  final def +=(value: V): Unit = add(value)

  final def add(value: V): Unit = {
    synchronized {
      if (keyToValue contains value.key) throw new DuplicateIdException(value)
      keyToValue += value.key → value
    }
    counter.incrementAndGet()
    onAdded(value)
  }

  def onAdded(value: V) = {}

  final def -=(key: Id): Unit =
    remove(key) match {
      case Some(removed) ⇒ onRemoved(removed)
      case None ⇒
    }

  final def remove(key: Id): Option[V] = keyToValue remove key

  def onRemoved(value: V) = {}

  final def totalCount = counter.get

  protected def throwNoSuchId(key: Id) = throw new NoSuchElementException(s"Unknown '$key'")

  final class DuplicateIdException(val value: V) extends DuplicateKeyException("'$key' is already registered") {
    def key = value.key
  }
}

object Register {
  final def apply[V <: HasKey](): Register[V] = new Standard[V]

  private class Standard[V <: HasKey] extends Register[V]
}
