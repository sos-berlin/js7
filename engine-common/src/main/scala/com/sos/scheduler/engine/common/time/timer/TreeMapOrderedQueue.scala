package com.sos.scheduler.engine.common.time.timer

import TreeMapOrderedQueue._
import com.sos.scheduler.engine.common.scalautil.Logger
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
      val old = treeMap.put(key, buffer)
      if (old != null) {
        logger.error(s"Concurrent TreeMapOrderedQueue.add: $old")
        throw new AssertionError("Concurrent TreeMapOrderedQueue.add")
      }
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
            if (buffer.isEmpty) {
              val b = treeMap.remove(key)
              if (b.nonEmpty) {
                logger.error(s"Concurrent access during TreeMapOrderedQueue.remove: $buffer")
                throw new AssertionError("Concurrent access during TreeMapOrderedQueue.remove")
              }
            }
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

object TreeMapOrderedQueue {
  private val logger = Logger(getClass)
}
