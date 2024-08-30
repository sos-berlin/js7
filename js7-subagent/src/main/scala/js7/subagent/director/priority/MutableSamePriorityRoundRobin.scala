package js7.subagent.director.priority

import js7.base.utils.Atomic
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec

/** Manages a round-robin index as long as the index is associated with the same priority. */
private final class MutableSamePriorityRoundRobin:

  private val _nextIndex = Atomic(0)

  @TestOnly
  private[director] def nextIndex = _nextIndex.get()

  /** Returns an incremented index as while as priorities are equal and index < n.
   * <p>
   *   <ul>
   *      <li>The first call returns the index 0.
   *      <li>The next call return an increment index if
   *      <ul>
   *        <li>the index < n and the priority associated with the index is
   *        same as the predecessor (round-robin).
   *        <li>Otherwise, 0 is returned.
   *   </ul>
   * </ul>
   * May be called concurrently.
   */
  @tailrec
  def next(n: Int, samePriority: (Int, Int) => Boolean): Int =
    if n <= 0 then
      0
    else
      val i = _nextIndex.get()
      val result = if i < n then i else 0
      var nxt = result + 1
      if nxt >= n then nxt = 0
      if i < n && !samePriority(i, nxt) then nxt = 0
      if !_nextIndex.compareAndSet(i, nxt) then
        next(n, samePriority)
      else
        result

  override def toString = s"MutableSamePriorityRoundRobin(nextIndex=${_nextIndex.get()})"
