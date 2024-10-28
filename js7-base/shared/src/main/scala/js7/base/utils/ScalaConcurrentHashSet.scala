package js7.base.utils

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.runtime.BoxedUnit
import scala.runtime.BoxedUnit.UNIT

/**
 * Scala adapter for [[java.util.concurrent.ConcurrentHashMap]].
 * @author Joacim Zschimmer
 */
class ScalaConcurrentHashSet[A] extends mutable.Set[A]:
  val delegate = new java.util.concurrent.ConcurrentHashMap[A, BoxedUnit]

  final def insert(key: A): this.type =
    val existingValue = delegate.putIfAbsent(key, UNIT)
    if existingValue != null then throw new DuplicateKeyException(s"'$key' has already been registered")
    this

  final def clear(): Unit =
    delegate.clear()

  final override def addOne(a: A): this.type =
    delegate.put(a, UNIT)
    this

  final override def subtractOne(a: A): this.type =
    delegate.remove(a)
    this

  final def iterator: Iterator[A] =
    delegate.keys.asScala

  final def contains(a: A): Boolean =
    delegate.containsKey(a)

  override final def isEmpty: Boolean =
    delegate.isEmpty
