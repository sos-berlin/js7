package com.sos.scheduler.engine.common.time.alarm

/**
  * @author Joacim Zschimmer
  */
private[alarm] class ConcurrentOrderedQueue[K: Ordering, V](delegate: OrderedQueue[K, V]) {

  final def size = synchronized { delegate.size }

  final def clear(): Unit = synchronized { delegate.clear() }

  final def add(v: V): Unit = synchronized { delegate.add(v) }

  final def foreach(body: V ⇒ Unit): Unit = synchronized { delegate.toSeq foreach body }

  final def popNext(untilIncluding: K): Either[K, V] = synchronized { delegate.popNext(untilIncluding) }

  final def head: V = synchronized { delegate.head }
}
