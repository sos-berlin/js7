package js7.subagent.director.priority

import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec

/** Manages a round-robin index 0 to n-1. */
private final class MutableRoundRobin:

  private val _nextIndex = Atomic(0)

  @TestOnly
  private[director] def nextIndex = _nextIndex.get()

  def reset(): Unit =
    _nextIndex := 0

  /** Returns an incremented index >= 0 and index < n.
   * <p>
   * May be called concurrently.
   */
  @tailrec
  def next(n: Int): Int =
    if n <= 1 then
      _nextIndex := n
      0
    else
      val index =_nextIndex.getAndIncrement()
      if index < n then
        index
      else if _nextIndex.compareAndSet(index + 1, 1) then
        0
      else
        next(n)

  override def toString = "MutableRoundRobin"
