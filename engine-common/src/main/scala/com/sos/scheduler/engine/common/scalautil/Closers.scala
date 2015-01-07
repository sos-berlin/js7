package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
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
        delegate.register(toGuavaCloseable(body))
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
  }

  def toGuavaCloseable(autoCloseable: AutoCloseable): GuavaCloseable =
    autoCloseable match {
      case o: GuavaCloseable ⇒ o
      case o ⇒ toGuavaCloseable { o.close() }
    }

  def toGuavaCloseable(closeable: HasClose): GuavaCloseable = toGuavaCloseable { closeable.close() }

  def toGuavaCloseable(f: ⇒ Unit): GuavaCloseable =
    new GuavaCloseable {
      def close() = f
    }
}
