package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.HasCloser._
import com.sos.scheduler.engine.common.scalautil.HasCloser.implicits._
import scala.language.reflectiveCalls

trait HasCloser extends AutoCloseable with CloseOnError {

  private val _closer: Closer = Closer.create()

  protected implicit final def closer: Closer = {
    if (_closer == null) throw new NullPointerException(s"$getClass should extend HasClose further in front?")
    _closer
  }

  protected final def onCloseOrShutdown(body: ⇒ Unit): Unit = {
    closer.onCloseOrShutdown(body)
  }

  protected final def whenNotClosedAtShutdown(body: ⇒ Unit): Unit = {
    closer.whenNotClosedAtShutdown(body)
  }

  /** Registers the function for execution in close(), in reverse order of registering. */
  protected final def onClose(f: ⇒ Unit): Unit = {
    closer.register(toGuavaCloseable(f))
  }

  protected final def registerAutoCloseable(autoCloseable: AutoCloseable): Unit = {
    closer.register(toGuavaCloseable(autoCloseable))
  }

  def close(): Unit = {
    closer.close()
  }
}


object HasCloser {
  private type GuavaCloseable = java.io.Closeable

  object implicits {
    implicit class RichCloser(val delegate: Closer) extends AnyVal {
      final def onCloseOrShutdown(body: ⇒ Unit): Unit = {
        apply(body)
        whenNotClosedAtShutdown(body)
      }
      
      final def apply(body: ⇒ Unit): Unit = {
        delegate.register(toGuavaCloseable(body))
      }

      final def whenNotClosedAtShutdown(body: ⇒ Unit): Unit = {
        val hook = new Thread(s"ShutdownHook for $toString") {
          override def run(): Unit = {
            body
          }
        }
        sys.runtime.addShutdownHook(hook)
        apply {
          sys.runtime.removeShutdownHook(hook)
        }
      }
    }

    implicit class RichClosable[A <: { def close(): Unit }](val delegate: A) extends AnyVal {
      final def registerCloseable(implicit closer: Closer): A = {
        closer.register(toGuavaCloseable { delegate.close() })
        delegate
      }
    }
  }

  private def toGuavaCloseable(autoCloseable: AutoCloseable): GuavaCloseable =
    autoCloseable match {
      case o: GuavaCloseable ⇒ o
      case o ⇒ toGuavaCloseable { o.close() }
    }

  private def toGuavaCloseable(f: ⇒ Unit): GuavaCloseable =
    new GuavaCloseable {
      def close(): Unit = {
        f
      }
    }
}
