package js7.base.log

import com.typesafe.scalalogging.Logger
import org.slf4j.Marker

/**
  * Like com.typesafe.scalalogging.Logger, but the functions have lazy signatures.
  * The difference to macro-based with eager-looking function signatures should only be syntactically.
  * Needed when used as argument for higher-order functions.
  *
  * @author Joacim Zschimmer
  */
final class LazyScalaLogger(delegate: Logger) {

  def error(message: => String) =
    delegate.error(message)

  def error(message: => String, t: Throwable) =
    delegate.error(message, t)

  def error(message: => String, args: AnyRef*) =
    delegate.error(message, args*)

  def error(marker: Marker, message: => String) =
    delegate.error(marker, message)

  def error(marker: Marker, message: String, t: Throwable) =
    delegate.error(marker, message, t)

  def error(marker: Marker, message: String, args: AnyRef*) =
    delegate.error(marker, message, args*)


  def warn(message: => String) =
    delegate.warn(message)

  def warn(message: => String, t: Throwable) =
    delegate.warn(message, t)

  def warn(message: => String, args: AnyRef*) =
    delegate.warn(message, args*)

  def warn(marker: Marker, message: => String) =
    delegate.warn(marker, message)

  def warn(marker: Marker, message: String, t: Throwable) =
    delegate.warn(marker, message, t)

  def warn(marker: Marker, message: String, args: AnyRef*) =
    delegate.warn(marker, message, args*)


  def info(message: => String) =
    delegate.info(message)

  def info(message: => String, t: Throwable) =
    delegate.info(message, t)

  def info(message: => String, args: AnyRef*) =
    delegate.info(message, args*)

  def info(marker: Marker, message: => String) =
    delegate.info(marker, message)

  def info(marker: Marker, message: String, t: Throwable) =
    delegate.info(marker, message, t)

  def info(marker: Marker, message: String, args: AnyRef*) =
    delegate.info(marker, message, args*)


  def debug(message: => String) =
    delegate.debug(message)

  def debug(message: => String, t: Throwable) =
    delegate.debug(message, t)

  def debug(message: => String, args: AnyRef*) =
    delegate.debug(message, args*)

  def debug(marker: Marker, message: => String) =
    delegate.debug(marker, message)

  def debug(marker: Marker, message: String, t: Throwable) =
    delegate.debug(marker, message, t)

  def debug(marker: Marker, message: String, args: AnyRef*) =
    delegate.debug(marker, message, args*)


  def trace(message: => String) =
    delegate.trace(message)

  def trace(message: => String, t: Throwable) =
    delegate.trace(message, t)

  def trace(message: => String, args: AnyRef*) =
    delegate.trace(message, args*)

  def trace(marker: Marker, message: => String) =
    delegate.trace(marker, message)

  def trace(marker: Marker, message: String, t: Throwable) =
    delegate.trace(marker, message, t)

  def trace(marker: Marker, message: String, args: AnyRef*) =
    delegate.debug(marker, message, args*)
}

object LazyScalaLogger {

  implicit final class AsLazyScalaLogger(private val delegate: Logger) extends AnyVal {
    /**
      * Converts eager-looking (because macro-based) com.typesafe.scalalogging.Logger
      * to LazyScalaLogger with lazy function signatures.
      */
    def asLazy = new LazyScalaLogger(delegate)
  }
}
