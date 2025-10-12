package js7.base.utils

import java.util.Objects.requireNonNull
import js7.base.log.Logger
import scala.util.control.{ControlThrowable, NonFatal}

// TODO Use scala.util.Using instead of this
/** Wie java try(AutoCloseable), aber für alle Klassen mit close().
  * @author Joacim Zschimmer */
object AutoClosing:

  private val logger = Logger[this.type]

  def multipleAutoClosing[M[X] <: Iterable[X], A <: AutoCloseable, B](resources: M[A])(body: resources.type => B): B =
    autoClosing(new Closer) { closer =>
      for o <- resources do closer.register(o)
      body(resources)
    }

  /** Wie Java 7 try-with-resource */
  def autoClosing[A <: AutoCloseable, B](resource: A)(body: resource.type => B): B =
    val result = closeOnError(resource):
      body(resource)
    resource.close()
    result

  final def closeOnError[A <: AutoCloseable, B](closeable: A)(body: => B): B =
    requireNonNull(closeable)
    try body
    catch
      case t: Throwable =>
        closeAfterError(closeable, t)
        throw t

  private def closeAfterError[A <: AutoCloseable](resource: A, t: Throwable): Unit =
    t match
      case NonFatal(_) | _: ControlThrowable => // Normal exception
      case _ => logger.error(t.toString, t)
    try resource.close()
    catch
      case suppressed: Throwable =>
        if t ne suppressed then
          t.addSuppressed(suppressed)
          val suppresseds = t.getSuppressed
          if suppresseds.isEmpty || (suppresseds.last ne suppressed) then // Suppression disabled?
            logger.warn(
              s"While handling an exception, this second exception is ignored: $suppressed\n" +
                s"Original exception is: $t", suppressed)

  object syntax:
    implicit final class RichAutoClosable[A <: AutoCloseable](private val autoCloseable: A)
    extends AnyVal:
      def use[B](body: A => B): B =
        autoClosing(autoCloseable)(body)
