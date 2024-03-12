package js7.subagent

import js7.base.time.ScalaTime.*
import js7.base.utils.ByteUnits.toKBGB
import js7.subagent.OutErrStatistics.*
import cats.effect.IO
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.*

private final class OutErrStatistics:
  private val runningSince = now
  private var blockedNanos = 0L
  private var messageCount = 0
  private var size = 0L

  def count[A](totalLength: Int, n: Int = 1)(io: IO[A]): IO[A] =
    messageCount += n
    size += totalLength
    io.timed.map { case (duration, a) =>
      blockedNanos += duration.toNanos
      a
    }

  def isRelevant = blockedNanos >= RelevantBlockedNanos

  override def toString =
    s"$messageCount chunks" +
      (if size == 0 then "" else {
        val blocked = blockedNanos.nanoseconds
        val duration = runningSince.elapsed.toNanos
        val percentage = if duration == 0 then 1 else 100 * blocked.toNanos / duration
        s" (${toKBGB(size)}), blocked ${blocked.pretty} " +
          s"${(blocked / messageCount).pretty}/chunk ($percentage%)"
      })  // This is the time an unbuffered stdout/stderr pipe is blocked

private object OutErrStatistics:
  private val RelevantBlockedNanos = 1.s.toNanos
