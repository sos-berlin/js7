package com.sos.scheduler.engine.common.time.alarm

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
private[alarm] final class TreeMapOrderedQueue[K <: java.lang.Comparable[K], V: ClassTag](toKey_ : V ⇒ K)
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

  def foreach(body: V ⇒ Unit): Unit = toSeq foreach body

  private[alarm] def toSeq = treeMap.values.toArray.toIndexedSeq.asInstanceOf[IndexedSeq[mutable.ListBuffer[V]]].flatten

  def head: V =
    treeMap.firstEntry match {
      case null ⇒ throw new NoSuchElementException
      case e ⇒ e.getValue.head
    }

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
