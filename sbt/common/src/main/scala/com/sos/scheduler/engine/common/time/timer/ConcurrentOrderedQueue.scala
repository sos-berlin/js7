package com.sos.scheduler.engine.common.time.timer

import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
private[timer] class ConcurrentOrderedQueue[K: Ordering, V](delegate: OrderedQueue[K, V])
extends OrderedQueue[K, V]{

  final def isEmpty = synchronized { delegate.isEmpty }

  final def size = synchronized { delegate.size }

  final def clear(): Unit = synchronized { delegate.clear() }

  final def add(v: V): Unit = synchronized { delegate.add(v) }

  final def remove(value: V) = synchronized { delegate.remove(value) }

  final def foreach(body: V ⇒ Unit): Unit = (delegate.toSeq: immutable.Seq[V]) foreach body

  final def map[A](body: V ⇒ A): immutable.Seq[A] = toSeq map body

  final def toSeq: immutable.Seq[V] = synchronized { delegate.toSeq }

  final def popNext(untilIncluding: K): Either[K, V] = synchronized { delegate.popNext(untilIncluding) }

  final override def head: V = synchronized { delegate.head }

  final def headOption: Option[V] = synchronized { delegate.headOption }

  final def lastOption: Option[V] = synchronized { delegate.lastOption }
}
