package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import java.io.Writer
import java.lang.Thread.currentThread
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future

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

  implicit protected def scheduler: Scheduler

  /** Do not block here! */
  protected def onFlush(string: String): Future[Completed]

  protected def onBufferingStarted(): Unit

  private var stringBuilder: StringBuilder = null
  @volatile
  private var blockingThread: Thread = null  // Only only thread is expected to write

  final def close() = {
    blockingThread match {
      case null ⇒
      case t ⇒ t.interrupt()  // In case of an emergency close, free blocking thread.
    }
    flush()
  }

  final def write(chars: Array[Char], offset: Int, length: Int) =
    if (length > 0) {
      val completed = writeSynchronized(chars, offset, length)
      blockingThread = currentThread
      try completed.awaitInfinite  // We block in our stdout/stderr reader thread to avoid congestion  // TODO Waits forever if OrderActor crashes
      finally blockingThread = null
    }

  private def writeSynchronized(chars: Array[Char], offset: Int, length: Int): Future[Completed] =
    synchronized {
      val startWritten =
        if (bufferLength + length > size)
          flushBuffer()
        else
          Future.successful(Completed)
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
        if (bufferLength >= size) {
          val flushed = flushBuffer()
          Future.sequence(flushed :: startWritten :: Nil) map (_ reduce ((_, _) ⇒ Completed))
        }
        else
          startWritten
      }
    }

  final def flush(): Unit =
    synchronized {  // `flush` may be called by the actor thread, while the pipe thread is calling `write`
      flushBuffer()
      stringBuilder = null
    } // Don't wait here for flushBuffer() to be completed

  private def flushBuffer(): Future[Completed] = {
    if (isEmpty)
      Future.successful(Completed)
    else {
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
