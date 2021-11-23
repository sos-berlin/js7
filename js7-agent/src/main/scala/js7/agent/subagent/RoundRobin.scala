package js7.agent.subagent

import monix.execution.atomic.Atomic
import scala.annotation.tailrec

final class RoundRobin
{
  private val index = Atomic(-1)

  @tailrec
  def next(n: Int): Int = {
    val i = index.get()
    val j = if (i < n - 1) i + 1 else 0
    if (!index.compareAndSet(i, j))
      next(n)
    else
      j
  }
}
