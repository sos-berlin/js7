package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import java.io.Writer
import java.lang.System.nanoTime
import java.time.Duration
import java.time.Instant.now

/**
  * @author Joacim Zschimmer
  */
private[order] final class StatisticalWriter(writer: Writer) extends Writer {

  private val startTime = now
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
        val bocked = Duration.ofNanos(blockedNanos)
        val duration = (now - startTime).toNanos
        val percentage = if (duration == 0) 1 else 100 * bocked.toNanos / duration
        s" (${toKBGB(size)}) blocked ${bocked.pretty} ($percentage%)"
      })  // This is the time an unbuffered stdout/stderr pipe is blocked

  def flush() = writer.flush()

  def close() = writer.close()

  def nonEmpty = !isEmpty
  def isEmpty = messageCount == 0
}
