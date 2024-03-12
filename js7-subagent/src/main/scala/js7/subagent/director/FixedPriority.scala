package js7.subagent.director

import js7.base.utils.Atomic
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec

private final class FixedPriority:

  private val _nextIndex = Atomic(0)

  @TestOnly
  private[director] def nextIndex = _nextIndex.get()

  @tailrec
  def next(n: Int, isEquivalent: (Int, Int) => Boolean): Int =
    if n <= 0 then
      0
    else
      val i = _nextIndex.get()
      val result = if i < n then i else 0
      var nxt = result + 1
      if nxt >= n then nxt = 0
      if i < n && !isEquivalent(i, nxt) then nxt = 0
      if !_nextIndex.compareAndSet(i, nxt) then
        next(n, isEquivalent)
      else
        result

  override def toString = s"FixedPriority(nextIndex=${_nextIndex.get()})"
