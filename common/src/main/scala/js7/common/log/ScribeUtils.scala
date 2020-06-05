package js7.common.log

import java.util.concurrent.ConcurrentHashMap
import scribe.format._
import scribe.output.LogOutput
import scribe.{Level, LogRecord}

object ScribeUtils
{
  private var initialized = false
  private val classToLoggerNameCache = new ConcurrentHashMap[String, String]

  def coupleScribeWithSlf4j(): Unit =
    synchronized {
      if (!initialized) {
        scribe.Logger.root
          .clearHandlers()
          .clearModifiers()
          .withHandler(formatter"$message$mdc", Log4jWriter, Some(Level.Trace))
          .replace()
        initialized = true
      }
    }

  private object Log4jWriter extends scribe.writer.Writer {
    def write[M](record: LogRecord[M], output: LogOutput): Unit = {
      val slf4jLogger = org.slf4j.LoggerFactory.getLogger(
        classToLoggerNameCache.computeIfAbsent(record.className, o => classToLoggerName(o)))
      lazy val msg = record.messageFunction().toString
      if (record.level >= Level.Error) {
        if (slf4jLogger.isErrorEnabled) {
          slf4jLogger.error(msg, record.throwable.orNull)
        }
      } else if (record.level >= Level.Warn) {
        if (slf4jLogger.isWarnEnabled) {
          slf4jLogger.warn(msg, record.throwable.orNull)
        }
      } else if (record.level >= Level.Info) {
        if (slf4jLogger.isInfoEnabled) {
          slf4jLogger.info(msg, record.throwable.orNull)
        }
      } else if (record.level >= Level.Debug) {
        if (slf4jLogger.isDebugEnabled) {
          slf4jLogger.debug(msg, record.throwable.orNull)
        }
      } else {
        if (slf4jLogger.isTraceEnabled) {
          slf4jLogger.trace(msg, record.throwable.orNull)
        }
      }
    }
  }

  private def classToLoggerName(className: String) =
    className.replaceFirst("""\.\$.*""", "")   // Cut off ".$anonfun"
}
