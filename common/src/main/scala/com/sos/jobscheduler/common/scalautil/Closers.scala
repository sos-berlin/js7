package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import scala.language.reflectiveCalls

/**
 * @author Joacim Zschimmer
 */
object Closers
{
  object implicits {
    implicit final class RichClosersCloser(private val delegate: Closer) extends AnyVal {
      def onCloseOrShutdown(body: ⇒ Unit): Unit = {
        onClose(body)
        whenNotClosedAtShutdown(body)
      }

      def whenNotClosedAtShutdown(body: ⇒ Unit): Unit = {
        val hook = new Thread(s"ShutdownHook for $delegate") {
          override def run() = body
        }
        sys.runtime.addShutdownHook(hook)
        onClose {
          sys.runtime.removeShutdownHook(hook)
        }
      }

      def registerAutoCloseable(autoCloseable: AutoCloseable): Unit =
        delegate.register(autoCloseable)

      def onClose(body: ⇒ Unit): Unit =
        delegate.onClose(body)

      /**
        * Closes the `Closer`, then "finally" (nonwithstanding any NonFatal exception) call `body`.
        */
      def closeThen(body: ⇒ Unit) = {
        val c = new Closer
        c onClose body
        c.registerAutoCloseable(delegate)
        c.close()
      }
    }

    implicit final class RichClosersAutoCloseable[A <: AutoCloseable](private val delegate: A) extends AnyVal {
      def closeWithCloser(implicit closer: Closer): A = {
        closer.register(delegate)
        delegate
      }
    }

    implicit final class RichClosersAny[A <: AnyRef](private val delegate: A) extends AnyVal {
      def withCloser(onClose: A ⇒ Unit)(implicit closer: Closer): A = {
        closer.onClose(onClose(delegate))
        delegate
      }
    }
  }

  def withCloser[A](f: Closer ⇒ A): A = autoClosing(new Closer)(f)

  def closeOrdered(closeables: AutoCloseable*): Unit = {
    val closer = new Closer
    closeables.reverseIterator foreach closer.registerAutoCloseable
    closer.close()
  }
}
