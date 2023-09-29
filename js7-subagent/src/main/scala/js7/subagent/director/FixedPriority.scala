package js7.subagent.director

import monix.execution.atomic.Atomic
import scala.annotation.tailrec

private final class FixedPriority:
  private val nextIndex = Atomic(0)

  @tailrec
  def next[P](n: Int, isEquivalent: (Int, Int) => Boolean): Int =
    if n == 0 then
      0
    else
      val i = nextIndex.get()
      val result = if i < n then i else 0
      var nxt = result + 1
      if nxt == n then nxt = 0
      if nxt != result && !isEquivalent(i, nxt) then nxt = 0
      if !nextIndex.compareAndSet(i, nxt) then
        next(n, isEquivalent)
      else
        result

  override def toString = s"FixedPriority(nextIndex=${nextIndex.get()})"
