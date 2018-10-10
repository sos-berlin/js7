package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.common.scalautil.Closer._
import java.util.Objects.requireNonNull
import java.util.concurrent.ConcurrentLinkedDeque
import monix.execution.atomic.AtomicAny
import scala.annotation.tailrec
import scala.util.control.NonFatal

final class Closer extends AutoCloseable
{
  private val stack = new ConcurrentLinkedDeque[AutoCloseable]
  private val throwable = AtomicAny[Throwable](null)

  def onClose(closeable: ⇒ Unit): Unit =
    register(() ⇒ closeable)

  def register(closeable: AutoCloseable): Unit =
    stack.add(requireNonNull(closeable))

  @tailrec
  def close(): Unit =
    stack.pollLast() match {
      case null ⇒  // finish
        if (throwable.get != null) throw throwable.get

      case closeable ⇒
        try closeable.close()
        catch {
          case NonFatal(t) ⇒
            if (!throwable.compareAndSet(null, t)) {
              logger.debug(s"Throwable.addSuppressed($t)")
              throwable.get.addSuppressed(t)
            }
          case fatal ⇒
            throw fatal
        }
        close()
    }
}

object Closer {
  private val logger = Logger(getClass)
}
