package js7.base.log

import org.slf4j.{Marker, Logger as Slf4jLogger}

/**
  * @author Joacim Zschimmer
  */
trait ConvertingLogger extends Slf4jLogger:

  protected val delegate: Slf4jLogger

  def convertMessage(o: String): String

  def convertFormat(o: String): String

  final def warn(msg: String): Unit =
    delegate.warn(convertMessage(msg))

  final def warn(format: String, arg: Any): Unit =
    delegate.warn(convertFormat(format), arg)

  final def warn(format: String, args: AnyRef*): Unit =
    delegate.warn(convertFormat(format), args*)

  final def warn(format: String, arg1: Any, arg2: Any): Unit =
    delegate.warn(convertFormat(format), arg1, arg2)

  final def warn(msg: String, t: Throwable): Unit =
    delegate.warn(convertMessage(msg), t)

  final def warn(marker: Marker, msg: String): Unit =
    delegate.warn(marker, convertMessage(msg))

  final def warn(marker: Marker, format: String, arg: Any): Unit =
    delegate.warn(marker, convertFormat(format), arg)

  final def warn(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    delegate.warn(marker, convertFormat(format), arg1, arg2)

  final def warn(marker: Marker, format: String, args: AnyRef*): Unit =
    delegate.warn(marker, convertFormat(format), args*)

  final def warn(marker: Marker, msg: String, t: Throwable): Unit =
    delegate.warn(marker, convertMessage(msg), t)

  final def isErrorEnabled: Boolean =
    delegate.isErrorEnabled

  final def isErrorEnabled(marker: Marker): Boolean =
    delegate.isErrorEnabled(marker)

  final def getName: String =
    delegate.getName

  final def isInfoEnabled: Boolean =
    delegate.isInfoEnabled

  final def isInfoEnabled(marker: Marker): Boolean =
    delegate.isInfoEnabled(marker)

  final def isDebugEnabled: Boolean =
    delegate.isDebugEnabled

  final def isDebugEnabled(marker: Marker): Boolean =
    delegate.isDebugEnabled(marker)

  final def isTraceEnabled: Boolean =
    delegate.isTraceEnabled

  final def isTraceEnabled(marker: Marker): Boolean =
    delegate.isTraceEnabled(marker)

  final def error(msg: String): Unit =
    delegate.error(convertMessage(msg))

  final def error(format: String, arg: Any): Unit =
    delegate.error(convertFormat(format), arg)

  final def error(format: String, arg1: Any, arg2: Any): Unit =
    delegate.error(convertFormat(format), arg1, arg2)

  final def error(format: String, args: AnyRef*): Unit =
    delegate.error(convertFormat(format), args*)

  final def error(msg: String, t: Throwable): Unit =
    delegate.error(convertMessage(msg), t)

  final def error(marker: Marker, msg: String): Unit =
    delegate.error(marker, convertMessage(msg))

  final def error(marker: Marker, format: String, arg: Any): Unit =
    delegate.error(marker, convertFormat(format), arg)

  final def error(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    delegate.error(marker, convertFormat(format), arg1, arg2)

  final def error(marker: Marker, format: String, args: AnyRef*): Unit =
    delegate.error(marker, convertFormat(format), args*)

  final def error(marker: Marker, msg: String, t: Throwable): Unit =
    delegate.error(marker, convertMessage(msg), t)

  final def debug(msg: String): Unit =
    delegate.debug(convertMessage(msg))

  final def debug(format: String, arg: Any): Unit =
    delegate.debug(convertFormat(format), arg)

  final def debug(format: String, arg1: Any, arg2: Any): Unit =
    delegate.debug(convertFormat(format), arg1, arg2)

  final def debug(format: String, args: AnyRef*): Unit =
    delegate.debug(convertFormat(format), args*)

  final def debug(msg: String, t: Throwable): Unit =
    delegate.debug(convertMessage(msg), t)

  final def debug(marker: Marker, msg: String): Unit =
    delegate.debug(marker, convertMessage(msg))

  final def debug(marker: Marker, format: String, arg: Any): Unit =
    delegate.debug(marker, convertFormat(format), arg: Any)

  final def debug(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    delegate.debug(marker, convertFormat(format), arg1: Any, arg2: Any)

  final def debug(marker: Marker, format: String, args: AnyRef*): Unit =
    delegate.debug(marker, convertFormat(format), args*)

  final def debug(marker: Marker, msg: String, t: Throwable): Unit =
    delegate.debug(marker, convertMessage(msg), t: Throwable)

  final def isWarnEnabled: Boolean =
    delegate.isWarnEnabled

  final def isWarnEnabled(marker: Marker): Boolean =
    delegate.isWarnEnabled(marker)

  final def trace(msg: String): Unit =
    delegate.trace(convertMessage(msg))

  final def trace(format: String, arg: Any): Unit =
    delegate.trace(convertFormat(format), arg)

  final def trace(format: String, arg1: Any, arg2: Any): Unit =
    delegate.trace(convertFormat(format), arg1, arg2)

  final def trace(format: String, args: AnyRef*): Unit =
    delegate.trace(convertFormat(format), args*)

  final def trace(msg: String, t: Throwable): Unit =
    delegate.trace(convertMessage(msg), t)

  final def trace(marker: Marker, msg: String): Unit =
    delegate.trace(marker, convertMessage(msg))

  final def trace(marker: Marker, format: String, arg: Any): Unit =
    delegate.trace(marker, convertFormat(format), arg: Any)

  final def trace(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    delegate.trace(marker, convertFormat(format), arg1: Any, arg2: Any)

  final def trace(marker: Marker, format: String, args: AnyRef*): Unit =
    delegate.trace(marker, convertFormat(format), args: AnyRef)

  final def trace(marker: Marker, msg: String, t: Throwable): Unit =
    delegate.trace(marker, convertMessage(msg), t: Throwable)

  final def info(msg: String): Unit =
    delegate.info(convertMessage(msg))

  final def info(format: String, arg: Any): Unit =
    delegate.info(convertFormat(format), arg)

  final def info(format: String, arg1: Any, arg2: Any): Unit =
    delegate.info(convertFormat(format), arg1, arg2)

  final def info(format: String, args: AnyRef*): Unit =
    delegate.info(convertFormat(format), args*)

  final def info(msg: String, t: Throwable): Unit =
    delegate.info(convertMessage(msg), t)

  final def info(marker: Marker, msg: String): Unit =
    delegate.info(marker, convertMessage(msg))

  final def info(marker: Marker, format: String, arg: Any): Unit =
    delegate.info(marker, convertFormat(format), arg)

  final def info(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    delegate.info(marker, convertFormat(format), arg1, arg2)

  final def info(marker: Marker, format: String, args: AnyRef*): Unit =
    delegate.info(marker, convertFormat(format), args*)

  final def info(marker: Marker, msg: String, t: Throwable): Unit =
    delegate.info(marker, convertMessage(msg), t)


object ConvertingLogger:
  final class Prefixed(prefix: String, protected val delegate: Slf4jLogger) extends ConvertingLogger:
    protected val fullPrefix = s"($prefix) "
    //protected val fullPrefix = s"“$prefix” "
    private val escapedPrefix = fullPrefix.replace("{}", "\\{}")

    def convertMessage(o: String): String = 
      fullPrefix + o

    def convertFormat(o: String): String =
      escapedPrefix + o
