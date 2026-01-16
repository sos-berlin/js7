package js7.base.utils

import java.util.concurrent.ConcurrentHashMap as JConcurrentHashMap
import scala.collection.{MapFactory, MapFactoryDefaults, StrictOptimizedIterableOps, StrictOptimizedMapOps, mutable}
import scala.jdk.CollectionConverters.*
import scala.util.NotGiven

final class ConcurrentHashMap[K, V](jmap: JConcurrentHashMap[K, V])
  (using NotGiven[K <:< Null], NotGiven[V <:< Null])
extends
  mutable.AbstractMap[K, V],
  mutable.MapOps[K, V, ConcurrentHashMap, ConcurrentHashMap[K, V]],
  StrictOptimizedIterableOps[(K, V), mutable.Iterable, ConcurrentHashMap[K, V]],
  StrictOptimizedMapOps[K, V, ConcurrentHashMap, ConcurrentHashMap[K, V]],
  MapFactoryDefaults[K, V, ConcurrentHashMap, mutable.Iterable],
  collection.concurrent.Map[K, V],
  Serializable:

  def this(initialCapacity: Int = DefaultInitialCapacity) =
    this(new JConcurrentHashMap(initialCapacity))

  override def mapFactory: MapFactory[ConcurrentHashMap] =
    ConcurrentHashMap

  override def isEmpty: Boolean =
    jmap.isEmpty

  override def size: Int =
    jmap.size

  override def knownSize: Int =
    jmap.size

  override def apply(key: K): V =
    jmap.get(key) match
      case null => default(key)
      case v => v

  def get(key: K): Option[V] =
    Option(jmap.get(key))

  override def getOrElse[V1 >: V](key: K, defaultValue: => V1): V1 =
    jmap.get(key) match
      case null => defaultValue
      case v => v

  def iterator: Iterator[(K, V)] =
    jmap.entrySet.iterator.asScala.map: entry =>
      entry.getKey -> entry.getValue

  override def keySet: collection.Set[K] =
    jmap.keySet.asScala

  override def keysIterator: Iterator[K] =
    jmap.keySet.iterator.asScala

  override def values: Iterable[V] =
    jmap.values.asScala

  override def valuesIterator: Iterator[V] =
    jmap.values.iterator.asScala

  override def contains(k: K): Boolean =
    jmap.containsKey(k)

  def putIfAbsent(k: K, v: V): Option[V] =
    Option:
      jmap.putIfAbsent(k, v)

  override def getOrElseUpdate(k: K, defaultValue: => V): V =
    jmap.computeIfAbsent(k, _ => defaultValue)

  override def updateWith(k: K)(f: Option[V] => Option[V]): Option[V] =
    Option:
      jmap.compute(k, (_, v) => f(Option(v)).getOrElse(null.asInstanceOf[V]))

  override def update(key: K, value: V): Unit =
    jmap.put(key, value)

  def addOne(element: (K, V)): ConcurrentHashMap.this.type =
    jmap.put(element._1, element._2)
    this

  def subtractOne(element: K): ConcurrentHashMap.this.type =
    jmap.remove(element)
    this

  override def remove(key: K): Option[V] =
    Option:
      jmap.remove(key)

  def remove(key: K, value: V): Boolean =
    jmap.remove(key, value)

  override def clear(): Unit =
    jmap.clear()


  def replace(key: K, oldValue: V, newValue: V): Boolean =
    jmap.replace(key, oldValue, newValue)

  def replace(key: K, value: V): Option[V] =
    Option:
      jmap.replace(key, value)


object ConcurrentHashMap extends MapFactory[ConcurrentHashMap]:

  private final val DefaultInitialCapacity = 16

  def empty[K, V]: ConcurrentHashMap[K, V] =
    new ConcurrentHashMap

  def from[K, V](it: IterableOnce[(K, V)]): ConcurrentHashMap[K, V] =
    val jmap = it.knownSize match
      case -1 => new JConcurrentHashMap[K, V]
      case n => new JConcurrentHashMap[K, V](n)
    it match
      case map: collection.Map[K @unchecked, V @unchecked] => jmap.putAll(map.asJava)
      case _ =>
        it.iterator.foreach: (k, v) =>
          jmap.put(k, v)
    new ConcurrentHashMap(jmap)

  def newBuilder[K, V]: mutable.Builder[(K, V), ConcurrentHashMap[K, V]] =
    newBuilder()

  def newBuilder[K, V](initialCapacity: Int = DefaultInitialCapacity)
  : mutable.Builder[(K, V), ConcurrentHashMap[K, V]] =
    new mutable.GrowableBuilder[(K, V), ConcurrentHashMap[K, V]](
      new ConcurrentHashMap[K, V](initialCapacity)
    ):
      override def sizeHint(size: Int) =
        elems.sizeHint(size)
