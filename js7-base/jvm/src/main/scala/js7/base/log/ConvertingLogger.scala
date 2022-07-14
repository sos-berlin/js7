package js7.base.log

import org.slf4j.{Marker, Logger as Slf4jLogger}

/**
  * @author Joacim Zschimmer
  */
trait ConvertingLogger extends Slf4jLogger
{
  protected val delegate: Slf4jLogger

  def convertMessage(o: String): String

  def convertFormat(o: String): String

  final def warn(msg: String) =
    delegate.warn(convertMessage(msg))

  final def warn(format: String, arg: Any) =
    delegate.warn(convertFormat(format), arg)

  final def warn(format: String, args: AnyRef*) =
    delegate.warn(convertFormat(format), args: _*)

  final def warn(format: String, arg1: Any, arg2: Any) =
    delegate.warn(convertFormat(format), arg1, arg2)

  final def warn(msg: String, t: Throwable) =
    delegate.warn(convertMessage(msg), t)

  final def warn(marker: Marker, msg: String) =
    delegate.warn(marker, convertMessage(msg))

  final def warn(marker: Marker, format: String, arg: Any) =
    delegate.warn(marker, convertFormat(format), arg)

  final def warn(marker: Marker, format: String, arg1: Any, arg2: Any) =
    delegate.warn(marker, convertFormat(format), arg1, arg2)

  final def warn(marker: Marker, format: String, args: AnyRef*) =
    delegate.warn(marker, convertFormat(format), args: _*)

  final def warn(marker: Marker, msg: String, t: Throwable) =
    delegate.warn(marker, convertMessage(msg), t)

  final def isErrorEnabled =
    delegate.isErrorEnabled

  final def isErrorEnabled(marker: Marker) =
    delegate.isErrorEnabled(marker)

  final def getName =
    delegate.getName

  final def isInfoEnabled =
    delegate.isInfoEnabled

  final def isInfoEnabled(marker: Marker) =
    delegate.isInfoEnabled(marker)

  final def isDebugEnabled =
    delegate.isDebugEnabled

  final def isDebugEnabled(marker: Marker) =
    delegate.isDebugEnabled(marker)

  final def isTraceEnabled =
    delegate.isTraceEnabled

  final def isTraceEnabled(marker: Marker) =
    delegate.isTraceEnabled(marker)

  final def error(msg: String) =
    delegate.error(convertMessage(msg))

  final def error(format: String, arg: Any) =
    delegate.error(convertFormat(format), arg)

  final def error(format: String, arg1: Any, arg2: Any) =
    delegate.error(convertFormat(format), arg1, arg2)

  final def error(format: String, args: AnyRef*) =
    delegate.error(convertFormat(format), args: _*)

  final def error(msg: String, t: Throwable) =
    delegate.error(convertMessage(msg), t)

  final def error(marker: Marker, msg: String) =
    delegate.error(marker, convertMessage(msg))

  final def error(marker: Marker, format: String, arg: Any) =
    delegate.error(marker, convertFormat(format), arg)

  final def error(marker: Marker, format: String, arg1: Any, arg2: Any) =
    delegate.error(marker, convertFormat(format), arg1, arg2)

  final def error(marker: Marker, format: String, args: AnyRef*) =
    delegate.error(marker, convertFormat(format), args: _*)

  final def error(marker: Marker, msg: String, t: Throwable) =
    delegate.error(marker, convertMessage(msg), t)

  final def debug(msg: String) =
    delegate.debug(convertMessage(msg))

  final def debug(format: String, arg: Any) =
    delegate.debug(convertFormat(format), arg)

  final def debug(format: String, arg1: Any, arg2: Any) =
    delegate.debug(convertFormat(format), arg1, arg2)

  final def debug(format: String, args: AnyRef*) =
    delegate.debug(convertFormat(format), args: _*)

  final def debug(msg: String, t: Throwable) =
    delegate.debug(convertMessage(msg), t)

  final def debug(marker: Marker, msg: String) =
    delegate.debug(marker, convertMessage(msg))

  final def debug(marker: Marker, format: String, arg: Any) =
    delegate.debug(marker, convertFormat(format), arg: Any)

  final def debug(marker: Marker, format: String, arg1: Any, arg2: Any) =
    delegate.debug(marker, convertFormat(format), arg1: Any, arg2: Any)

  final def debug(marker: Marker, format: String, args: AnyRef*) =
    delegate.debug(marker, convertFormat(format), args: _*)

  final def debug(marker: Marker, msg: String, t: Throwable) =
    delegate.debug(marker, convertMessage(msg), t: Throwable)

  final def isWarnEnabled =
    delegate.isWarnEnabled

  final def isWarnEnabled(marker: Marker) =
    delegate.isWarnEnabled(marker)

  final def trace(msg: String) =
    delegate.trace(convertMessage(msg))

  final def trace(format: String, arg: Any) =
    delegate.trace(convertFormat(format), arg)

  final def trace(format: String, arg1: Any, arg2: Any) =
    delegate.trace(convertFormat(format), arg1, arg2)

  final def trace(format: String, args: AnyRef*) =
    delegate.trace(convertFormat(format), args: _*)

  final def trace(msg: String, t: Throwable) =
    delegate.trace(convertMessage(msg), t)

  final def trace(marker: Marker, msg: String) =
    delegate.trace(marker, convertMessage(msg))

  final def trace(marker: Marker, format: String, arg: Any) =
    delegate.trace(marker, convertFormat(format), arg: Any)

  final def trace(marker: Marker, format: String, arg1: Any, arg2: Any) =
    delegate.trace(marker, convertFormat(format), arg1: Any, arg2: Any)

  final def trace(marker: Marker, format: String, args: AnyRef*) =
    delegate.trace(marker, convertFormat(format), args: AnyRef)

  final def trace(marker: Marker, msg: String, t: Throwable) =
    delegate.trace(marker, convertMessage(msg), t: Throwable)

  final def info(msg: String) =
    delegate.info(convertMessage(msg))

  final def info(format: String, arg: Any) =
    delegate.info(convertFormat(format), arg)

  final def info(format: String, arg1: Any, arg2: Any) =
    delegate.info(convertFormat(format), arg1, arg2)

  final def info(format: String, args: AnyRef*) =
    delegate.info(convertFormat(format), args: _*)

  final def info(msg: String, t: Throwable) =
    delegate.info(convertMessage(msg), t)

  final def info(marker: Marker, msg: String) =
    delegate.info(marker, convertMessage(msg))

  final def info(marker: Marker, format: String, arg: Any) =
    delegate.info(marker, convertFormat(format), arg)

  final def info(marker: Marker, format: String, arg1: Any, arg2: Any) =
    delegate.info(marker, convertFormat(format), arg1, arg2)

  final def info(marker: Marker, format: String, args: AnyRef*) =
    delegate.info(marker, convertFormat(format), args: _*)

  final def info(marker: Marker, msg: String, t: Throwable) =
    delegate.info(marker, convertMessage(msg), t)
}

object ConvertingLogger
{
  final class Prefixed(prefix: String, protected val delegate: Slf4jLogger) extends ConvertingLogger {
    protected val fullPrefix = s"($prefix) "
    //protected val fullPrefix = s"“$prefix” "
    private val escapedPrefix = fullPrefix.replace("{}", "\\{}")

    def convertMessage(o: String) = fullPrefix + o

    def convertFormat(o: String) = escapedPrefix + o
  }
}
