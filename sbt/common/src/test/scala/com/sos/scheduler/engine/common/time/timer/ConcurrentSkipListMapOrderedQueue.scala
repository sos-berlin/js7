package com.sos.scheduler.engine.common.time.timer

import java.util.concurrent.atomic.AtomicReference
import java.util.function.BiFunction
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
private[timer] final class ConcurrentSkipListMapOrderedQueue[K <: java.lang.Comparable[K], V: ClassTag](toKey_ : V ⇒ K)
extends OrderedQueue.Implement[K, V] {

  private val keyToValues = new java.util.concurrent.ConcurrentSkipListMap[K, Vector[V]]()

  def isEmpty = keyToValues.isEmpty

  def size = (keyToValues.values map { _.size }).sum

  def clear(): Unit = keyToValues.clear()

  def add(value: V): Unit = {
    val key = toKey(value)
    keyToValues.compute(key, new BiFunction[K, Vector[V], Vector[V]] {
      def apply(k: K, values: Vector[V]) = (if (values != null) values else Vector[V]()) :+ value
    })
  }

  def remove(value: V): Boolean = {
    val key = toKey(value)
    var removed = false
    keyToValues.compute(key, new BiFunction[K, Vector[V], Vector[V]] {
      def apply(k: K, values: Vector[V]) = {
        if (values != null) {
          values indexOf value match {
            case -1 ⇒
              removed = false
              values
            case i ⇒
              removed = true
              if (values.size == 1) null
              else values.slice(0, i) ++ values.slice(i + 1, values.size)  // Slow ???
          }
        } else {
          removed = false
          null
        }
      }
    })
    removed
  }

  def foreach(body: V ⇒ Unit): Unit = toSeq foreach body

  def toSeq = keyToValues.values.toArray(new Array[Vector[V]](0)).toIndexedSeq.flatten

  def headOption: Option[V] = Option(keyToValues.firstEntry) map { _.getValue.head }

  def lastOption: Option[V] = Option(keyToValues.lastEntry) map { _.getValue.last }

  protected def removeHead(): V = {
    val result = new AtomicReference[V]  // Does not compile: val result: V = _
    keyToValues.compute(keyToValues.firstKey, new BiFunction[K, Vector[V], Vector[V]] {
      def apply(k: K, values: Vector[V]) = {
        result.set(values.head)
        if (values.size == 1) null else values.tail
      }
    })
    result.get
  }

  protected def toKey(value: V) = toKey_(value)

  protected def lt(a: K, b: K) = (a compareTo b) < 0
}
