package js7.agent.scheduler.order

import java.io.Writer
import java.lang.System.nanoTime
import js7.agent.scheduler.order.StatisticalWriter._
import js7.base.time.ScalaTime._
import js7.common.utils.ByteUnits.toKBGB
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
private[order] final class StatisticalWriter(writer: Writer) extends Writer
{
  private val runningSince = now
  private var blockedNanos = 0L
  private var messageCount = 0
  private var size = 0L

  def write(chars: Array[Char], offset: Int, length: Int) = {
    val time = nanoTime
    messageCount += 1
    size += length
    writer.write(chars, offset, length)
    blockedNanos += nanoTime - time
  }

  override def toString =
    s"$messageCount chunks" +
      (if (size == 0) "" else {
        val bocked = blockedNanos.nanoseconds
        val duration = runningSince.elapsed.toNanos
        val percentage = if (duration == 0) 1 else 100 * bocked.toNanos / duration
        s" (${toKBGB(size)}), blocked ${bocked.pretty} ($percentage%)"
      })  // This is the time an unbuffered stdout/stderr pipe is blocked

  def flush() = writer.flush()

  def close() = writer.close()

  def isRelevant = messageCount > 0 && blockedNanos >= RelevantBlockedNanos
}

object StatisticalWriter {
  private val RelevantBlockedNanos = 1.second.toNanos
}
