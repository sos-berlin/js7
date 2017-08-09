package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import java.io.Writer
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

/**
  * Combines several `write` calls to one `onFlush`.
  * Tries to free the buffer (`StringBuilder`) when not needed.
  *
  * @author Joacim Zschimmer
  */
private[order] trait BufferedStringWriter extends Writer {

  require(passThroughSize <= size)

  protected def size: Int

  protected def passThroughSize: Int

  /** Do not block here! */
  protected def onFlush(string: String): Future[Completed]

  protected def onBufferingStarted(): Unit

  private var stringBuilder: StringBuilder = null

  final def write(chars: Array[Char], offset: Int, length: Int) =
    if (length > 0) {
      val completed = writeSynchronized(chars, offset, length)
      Await.ready(completed, Inf).successValue  // We block in stdout/stderr reader thread to avoid congestion
    }

  private def writeSynchronized(chars: Array[Char], offset: Int, length: Int): Future[Completed] =
    synchronized {
      if (bufferLength + length > size) {
        flushBuffer()
      }
      if (isEmpty && length >= passThroughSize) {
        val completed = onFlush(new String(chars, offset, length))
        stringBuilder = null
        completed
      } else {
        if (isEmpty) {
          onBufferingStarted()
          if (stringBuilder == null) {
            stringBuilder = new StringBuilder(size)  // Reserve now (StringBuilder itself would reserve up to twice as much, see StringBuilder#newCapacity)
          }
        }
        stringBuilder.appendAll(chars, offset, length)
        if (bufferLength == size) {
          flushBuffer()
        }
        Future.successful(Completed)
      }
    }

  final def flush(): Unit =
    synchronized {  // `flush` may be called by the actor thread, while the pipe thread is calling `write`
      flushBuffer()
      stringBuilder = null
    }

  private def flushBuffer(): Unit = {
    if (!isEmpty) {
      val string = stringBuilder.toString
      stringBuilder.clear()
      onFlush(string)
    }
  }

  private[order] def isEmpty = bufferLength == 0

  @TestOnly
  private[order] def reservedSize = if (stringBuilder == null) 0 else stringBuilder.capacity

  private def bufferLength: Int =
    if (stringBuilder == null) 0 else stringBuilder.length
}
