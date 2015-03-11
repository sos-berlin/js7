package com.sos.scheduler.engine.common.scalautil

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Scala adapter for [[java.util.concurrent.ConcurrentHashMap]].
 * @author Joacim Zschimmer
 */
class ScalaConcurrentHashMap[K, V] extends mutable.Map[K, V]{
  protected val delegate = new java.util.concurrent.ConcurrentHashMap[K, V]

  final override def +=(kv: (K, V)) = {
    delegate.put(kv._1, kv._2)
    this
  }

  final override def -=(key: K) = {
    delegate.remove(key)
    this
  }

  final def iterator = delegate.entrySet().toIterator map { o ⇒ o.getKey → o.getValue }

  final def get(key: K) = Option(delegate.get(key))

  override final def contains(key: K) = delegate containsKey key

  override final def isEmpty = delegate.isEmpty

  override final def keysIterator = delegate.keySet.iterator

  override final def keySet = delegate.keySet

  override final def valuesIterator = delegate.values.iterator

  override final def values = delegate.values
}
