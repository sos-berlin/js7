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

  def isEmpty = keyToValue.isEmpty

  def nonEmpty = keyToValue.nonEmpty

  def size = keyToValue.size

  def apply(key: Id) = keyToValue(key)

  def foreach(body: V ⇒ Unit) = keyToValue.values foreach body

  def map[A](f: V ⇒ A) = keyToValue.values map f

  def +=(value: V): Unit = {
    synchronized {
      if (keyToValue contains value.key) throw new DuplicateIdException(value)
      keyToValue += value.key → value
    }
    counter.incrementAndGet()
  }

  def -=(key: Id) = keyToValue remove key

  def totalCount = counter.get

  protected def throwNoSuchId(key: Id) = throw new NoSuchElementException(s"Unknown '$key'")

  final class DuplicateIdException(val value: V) extends DuplicateKeyException("'$key' is already registered") {
    def key = value.key
  }
}

object Register {
  def apply[V <: HasKey](): Register[V] = new Standard[V]

  private class Standard[V <: HasKey] extends Register[V]
}
