package com.sos.jobscheduler.common.scalautil

import scala.language.higherKinds
import scala.util.control.{ControlThrowable, NonFatal}

/** Wie java try(AutoCloseable), aber für alle Klassen mit close().
  * @author Joacim Zschimmer */
object AutoClosing {

  private val logger = Logger(getClass)

  def multipleAutoClosing[M[X] <: Iterable[X], A <: AutoCloseable, B](resources: M[A])(body: resources.type ⇒ B): B = {
    autoClosing(new Closer) { closer ⇒
      for (o ← resources) closer.register(o)
      body(resources)
    }
  }

  /** Wie Java 7 try-with-resource */
  def autoClosing[A <: AutoCloseable, B](resource: A)(body: resource.type ⇒ B): B = {
    val result = closeOnError(resource) {
      body(resource)
    }
    resource.close()
    result
  }

  final def closeOnError[A <: AutoCloseable, B](closeable: A)(body: ⇒ B): B = {
    if (closeable == null) throw new NullPointerException
    try body
    catch {
      case t: Throwable ⇒
        closeAfterError(closeable, t)
        throw t
    }
  }

  private def closeAfterError[A <: AutoCloseable](resource: A, t: Throwable): Unit = {
    t match {
      case NonFatal(_) | _: ControlThrowable ⇒ // Normal exception
      case _ ⇒ logger.error(t.toString, t)
    }
    try resource.close()
    catch {
      case suppressed: Throwable ⇒
        t.addSuppressed(suppressed)
        val suppresseds = t.getSuppressed
        if (suppresseds.isEmpty || (suppresseds.last ne suppressed)) // Suppression disabled?
          logger.warn(s"While handling an exception, this second exception is ignored: $suppressed\n" + s"Original exception is: $t", suppressed)
    }
  }
}
