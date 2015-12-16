package com.sos.scheduler.engine.common.time.timer

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
private[timer] final class TreeMapOrderedQueue[K <: java.lang.Comparable[K], V: ClassTag](toKey_ : V ⇒ K)
extends OrderedQueue.Implement[K, V] {

  private val treeMap = new java.util.TreeMap[K, mutable.ListBuffer[V]]()

  def isEmpty = treeMap.isEmpty

  def size = (treeMap.values map { _.size }).sum

  def clear(): Unit = treeMap.clear()

  def add(value: V): Unit = {
    val key = toKey(value)
    var buffer = treeMap.get(key)
    if (buffer == null) {
      buffer = new mutable.ListBuffer[V]()
      treeMap.put(key, buffer)
    }
    buffer += value
  }

  def remove(key: K, value: V): Boolean =
    treeMap.get(key) match {
      case null ⇒ false
      case buffer ⇒
        buffer indexOf value match {
          case -1 ⇒ false
          case i ⇒
            buffer.remove(i)
            if (buffer.isEmpty) treeMap.remove(key)
            true
        }
    }

  def foreach(body: V ⇒ Unit): Unit = toSeq foreach body

  def toSeq = treeMap.values.toArray(new Array[mutable.ListBuffer[V]](0)).toIndexedSeq.flatten

  def headOption: Option[V] = Option(treeMap.firstEntry) map { _.getValue.head }

  def lastOption: Option[V] = Option(treeMap.lastEntry) map { _.getValue.last }

  protected def removeHead(): V = {
    val first = treeMap.firstEntry
    val buffer = first.getValue
    val result = buffer.remove(0)
    if (buffer.isEmpty) {
      treeMap.remove(first.getKey)
    }
    result
  }

  protected def toKey(value: V) = toKey_(value)

  protected def lt(a: K, b: K) = (a compareTo b) < 0
}
