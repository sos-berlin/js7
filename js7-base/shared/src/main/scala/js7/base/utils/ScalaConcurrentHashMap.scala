package js7.base.utils

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Scala adapter for [[java.util.concurrent.ConcurrentHashMap]].
 * @author Joacim Zschimmer
 */
class ScalaConcurrentHashMap[K, V] extends mutable.Map[K, V]{
  val delegate = new java.util.concurrent.ConcurrentHashMap[K, V]

  final def insert(kv: (K, V)): this.type = insert(kv._1, kv._2)

  final def insert(key: K, value: V): this.type = {
    val existingValue = delegate.putIfAbsent(key, value)
    if (existingValue != null) throw new DuplicateKeyException(s"'$key' has already been registered")
    this
  }

  final override def addOne(kv: (K, V)) = {
    delegate.put(kv._1, kv._2)
    this
  }

  final override def subtractOne(key: K) = {
    delegate.remove(key)
    this
  }

  final def iterator = delegate.entrySet().iterator.asScala.map(o => o.getKey -> o.getValue)

  final def get(key: K) = Option(delegate.get(key))

  override final def contains(key: K) = delegate containsKey key

  override final def getOrElse[V1 >: V](key: K, default: => V1): V1 =
    delegate.get(key) match {
      case null => default
      case v => v
    }


  override final def isEmpty = delegate.isEmpty

  override final def keysIterator = delegate.keySet.iterator.asScala

  override final def keySet = delegate.keySet.asScala

  override final def valuesIterator = delegate.values.iterator.asScala

  override final def values = delegate.values.asScala
}
