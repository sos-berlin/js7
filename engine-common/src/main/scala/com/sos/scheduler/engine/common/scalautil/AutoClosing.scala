package com.sos.scheduler.engine.common.scalautil

import scala.language.reflectiveCalls
import scala.util.control.NonFatal

/** Wie java try(AutoClosable), aber für alle Klassen mit close().
  * @author Joacim Zschimmer */
object AutoClosing {

  private type HasClose = ({ def close(): Unit }) with AnyRef

  private val logger = Logger(getClass)

  /** Wie Java 7 try-with-resource */
  def autoClosing[A <: HasClose, B](resource: A)(f: A ⇒ B): B = {
    if (resource eq null) throw new NullPointerException("closingFinally: object is null")
    val result =
      try f(resource)
      catch {
        case t: Throwable ⇒
          closeAfterError(resource, t)
          throw t
      }
    resource.close()
    result
  }

  private def closeAfterError[A <: HasClose](resource: A, t: Throwable): Unit = {
    if (!NonFatal(t)) logger.error(t.toString, t)
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
