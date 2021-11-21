package js7.subagent

import js7.base.time.ScalaTime._
import js7.base.utils.ByteUnits.toKBGB
import js7.subagent.OutErrStatistics._
import monix.eval.Task
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

private final class OutErrStatistics
{
  private val runningSince = now
  private var blockedNanos = 0L
  private var messageCount = 0
  private var size = 0L

  def count[A](length: Int, task: Task[A]): Task[A] = {
    messageCount += 1
    size += length
    task.timed.map { case (duration, a) =>
      blockedNanos += duration.toNanos
      a
    }
  }

  def isRelevant = blockedNanos >= RelevantBlockedNanos

  override def toString =
    s"$messageCount chunks" +
      (if (size == 0) "" else {
        val blocked = blockedNanos.nanoseconds
        val duration = runningSince.elapsed.toNanos
        val percentage = if (duration == 0) 1 else 100 * blocked.toNanos / duration
        s" (${toKBGB(size)}), blocked ${blocked.pretty} " +
          s"${(blocked / messageCount).pretty}/chunk ($percentage%)"
      })  // This is the time an unbuffered stdout/stderr pipe is blocked
}

private object OutErrStatistics
{
  private val RelevantBlockedNanos = 1.s.toNanos
}
