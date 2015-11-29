package com.sos.scheduler.engine.common.time.alarm

import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
trait OrderedQueue[K, V] {

  def isEmpty: Boolean

  def size: Int

  def clear(): Unit

  def add(value: V): Unit

  def foreach(body: V ⇒ Unit): Unit

  def head: V

  def popNext(untilIncluding: K): Either[K, V]

  @TestOnly
  private[alarm] def toSeq: Seq[V]
}

object OrderedQueue {
  trait Implement[K, V] extends OrderedQueue[K, V] {
    protected def toKey(value: V): K

    protected def lt(a: K, b: K): Boolean

    protected def removeHead(): V

    final def popNext(untilIncluding: K): Either[K, V] = {
      val value = head
      val key = toKey(value)
      if (lt(untilIncluding, key)) Left(key)
      else Right(removeHead() ensuring { _ == value })
    }
  }
}
