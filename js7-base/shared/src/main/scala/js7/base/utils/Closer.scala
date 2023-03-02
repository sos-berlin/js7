package js7.base.utils

import java.util.Objects.requireNonNull
import java.util.concurrent.ConcurrentLinkedDeque
import js7.base.log.Logger
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.*
import monix.execution.atomic.AtomicAny
import scala.annotation.tailrec
import scala.util.control.NonFatal

final class Closer extends AutoCloseable
{
  private val stack = new ConcurrentLinkedDeque[AutoCloseable]
  private val throwable = AtomicAny[Throwable](null)

  def onCloseOrShutdown(body: => Unit): Unit = {
    onClose(body)
    whenNotClosedAtShutdown(body)
  }

  def whenNotClosedAtShutdown(body: => Unit): Unit = {
    val hook = new Thread(s"ShutdownHook for $toString") {
      override def run() = body
    }
    sys.runtime.addShutdownHook(hook)
    onClose {
      sys.runtime.removeShutdownHook(hook)
    }
  }

  /**
    * Closes the `Closer`, then "finally" (nonwithstanding any NonFatal exception) call `body`.
    */
  def closeThen(body: => Unit) = {
    val c = new Closer
    c onClose body
    c.register(this)
    c.close()
  }

  def onClose(closeable: => Unit): Unit =
    register(() => closeable)

  def register(closeable: AutoCloseable): Unit =
    stack.add(requireNonNull(closeable))

  @tailrec
  def close(): Unit =
    stack.pollLast() match {
      case null =>  // finish
        for (t <- Option(throwable.get())) throw t

      case closeable =>
        //Not in JavaScript: logger.traceCall[Unit](s"close $closeable") {
          try closeable.close()
          catch {
            case NonFatal(t) =>
              if (!throwable.compareAndSet(null, t)) {
                val tt = throwable.get()
                if (tt ne t) {
                  logger.debug(s"Throwable.addSuppressed($t)")
                  tt.addSuppressed(t)
                }
              }
            case fatal: Throwable =>
              throw fatal
          }
        //}
        close()
    }
}

object Closer
{
  private val logger = Logger[this.type]

  def withCloser[A](f: Closer => A): A =
    autoClosing(new Closer)(f)

  def closeOrdered(closeables: AutoCloseable*): Unit = {
    val closer = new Closer
    closeables.reverseIterator foreach closer.register
    closer.close()
  }

  object syntax {
    implicit final class RichClosersAutoCloseable[A <: AutoCloseable](private val underlying: A) extends AnyVal {
      def closeWithCloser(implicit closer: Closer): A = {
        closer.register(underlying)
        underlying
      }
    }

    implicit final class RichClosersAny[A <: AnyRef](private val underlying: A) extends AnyVal {
      def withCloser(onClose: A => Unit)(implicit closer: Closer): A = {
        closer.onClose { onClose(underlying) }
        underlying
      }
    }
  }
}
