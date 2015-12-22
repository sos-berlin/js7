package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.base.utils.HasKey
import com.sos.scheduler.engine.common.scalautil.ScalaConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

/**
  * @author Joacim Zschimmer
  */
trait ConcurrentRegister[V <: HasKey] {

  private type Key = V#Key

  private val keyToValue = new ScalaConcurrentHashMap[Key, V] {
    override def default(key: Key) = throwNoSuchKey(key)
  }
  private val counter = new AtomicInteger

  final def isEmpty = keyToValue.isEmpty

  final def nonEmpty = keyToValue.nonEmpty

  final def size = keyToValue.size

  final def apply(key: Key) = keyToValue(key)

  final def get(key: Key): Option[V] = keyToValue.get(key)

  final def getOrElse[A >: V](key: Key, default: ⇒ A): A = keyToValue.getOrElse(key, default)

  final def foreach(body: V ⇒ Unit) = keyToValue.values foreach body

  final def map[A](f: V ⇒ A) = keyToValue.values map f

  final def insert(value: V): Unit = {
    keyToValue.insert(value.key, value)
    counter.incrementAndGet()
    onAdded(value)
  }

  def onAdded(value: V) = {}

  final def -=(key: Key): Unit = remove(key)

  final def remove(key: Key): Option[V] = {
    val removedOption = keyToValue remove key
    removedOption match {
      case Some(removed) ⇒ onRemoved(removed)
      case None ⇒
    }
    removedOption
  }

  def onRemoved(value: V) = {}

  final def totalCount = counter.get

  protected def throwNoSuchKey(key: Key) = throw new NoSuchElementException(s"Unknown '$key'")
}

object ConcurrentRegister {
  final def apply[V <: HasKey](): ConcurrentRegister[V] = new Standard[V]

  private class Standard[V <: HasKey] extends ConcurrentRegister[V]
}
