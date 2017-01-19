package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.time.timer.OrderedQueue._
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
trait OrderedQueue[K, V] {

  def isEmpty: Boolean

  def size: Int

  def clear(): Unit

  def add(value: V): Unit

  def remove(key: K, value: V): Boolean

  def foreach(body: V ⇒ Unit): Unit

  def head: V = headOption getOrElse { throw new EmptyQueueException }

  def headOption: Option[V]

  def lastOption: Option[V]

  def popNext(untilIncluding: K): Either[K, V]

  def toSeq: immutable.Seq[V]
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

  final class EmptyQueueException private[OrderedQueue] extends NoSuchElementException
}
