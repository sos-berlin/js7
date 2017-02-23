package com.sos.scheduler.engine.common.scalautil

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.runtime.BoxedUnit
import scala.runtime.BoxedUnit.UNIT

/**
 * Scala adapter for [[java.util.concurrent.ConcurrentHashMap]].
 * @author Joacim Zschimmer
 */
class ScalaConcurrentHashSet[A] extends mutable.Set[A]{
  val delegate = new java.util.concurrent.ConcurrentHashMap[A, BoxedUnit]

  final def insert(key: A): this.type = {
    val existingValue = delegate.putIfAbsent(key, UNIT)
    if (existingValue != null) throw new DuplicateKeyException(s"'$key' has already been registered")
    this
  }

  final override def +=(a: A) = {
    delegate.put(a, UNIT)
    this
  }

  final override def -=(a: A) = {
    delegate.remove(a)
    this
  }

  final def iterator = delegate.keys.toIterator

  final def contains(a: A) = delegate containsKey a

  override final def isEmpty = delegate.isEmpty
}
