package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import scala.language.reflectiveCalls

/**
 * @author Joacim Zschimmer
 */
object Closers {
  type GuavaCloseable = java.io.Closeable
  private type HasClose = { def close(): Unit }

  object implicits {
    implicit class RichClosersCloser(val delegate: Closer) extends AnyVal {
      final def onCloseOrShutdown(body: ⇒ Unit): Unit = {
        onClose(body)
        whenNotClosedAtShutdown(body)
      }

      final def whenNotClosedAtShutdown(body: ⇒ Unit): Unit = {
        val hook = new Thread(s"ShutdownHook for $delegate") {
          override def run() = body
        }
        sys.runtime.addShutdownHook(hook)
        onClose {
          sys.runtime.removeShutdownHook(hook)
        }
      }

      final def registerAutoCloseable(autoCloseable: AutoCloseable): Unit =
        delegate.register(toGuavaCloseable(autoCloseable))

      final def onClose(body: ⇒ Unit): Unit =
        delegate.register(guavaCloseable(body))
    }

    implicit class RichClosersAutoCloseable[A <: AutoCloseable](val delegate: A) extends AnyVal {
      final def closeWithCloser(implicit closer: Closer): A = {
        closer.register(toGuavaCloseable(delegate))
        delegate
      }
    }

    implicit class RichClosersHasClose[A <: HasClose](val delegate: A) extends AnyVal {
      final def closeWithCloser(implicit closer: Closer): A = {
        closer.register(toGuavaCloseable(delegate))
        delegate
      }
    }

    implicit class RichClosersAny[A <: AnyRef](val delegate: A) extends AnyVal {
      final def withCloser(onClose: A ⇒ Unit)(implicit closer: Closer): A = {
        closer.register(guavaCloseable { onClose(delegate) })
        delegate
      }
    }
  }

  def toGuavaCloseable(autoCloseable: AutoCloseable): GuavaCloseable =
    autoCloseable match {
      case o: GuavaCloseable ⇒ o
      case o ⇒ guavaCloseable { o.close() }
    }

  def toGuavaCloseable(closeable: HasClose): GuavaCloseable = guavaCloseable { closeable.close() }

  private def guavaCloseable(f: ⇒ Unit): GuavaCloseable =
    new GuavaCloseable {
      def close() = f
    }

  def withCloser[A](f: Closer ⇒ A): A = autoClosing(Closer.create())(f)
}
