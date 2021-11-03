package js7.base.log

import java.util.concurrent.ConcurrentHashMap
import scribe.format.Formatter
import scribe.output.LogOutput
import scribe.output.format.OutputFormat
import scribe.{Level, LogRecord}

object ScribeForJava
{
  private var initialized = false
  private val classToLoggerCache = new ConcurrentHashMap[String, org.slf4j.Logger]

  def coupleScribeWithSlf4j(): Unit =
    synchronized {
      if (!initialized) {
        scribe.Logger.root
          .clearHandlers()
          .clearModifiers()
          .withHandler(Log4jFormatter, Log4jWriter, Some(Level.Trace))
          //.withHandler(Formatter.simple, Log4jWriter, Some(Level.Trace))
          .replace()
        initialized = true
      }
    }

  private object DummyLogOutput extends LogOutput {
    override def plainText = throw new NotImplementedError("DummyLogOutput.plainText")
    override def map(f: String => String) = this
  }

  private object Log4jFormatter extends Formatter {
    def format[M](record: LogRecord[M]) = DummyLogOutput
  }

  private object Log4jWriter extends scribe.writer.Writer {
    def write[M](record: LogRecord[M], output: LogOutput, outputFormat: OutputFormat): Unit = {
      val slf4jLogger = classToLoggerCache.computeIfAbsent(record.className,
        o => {
          Slf4jUtils.initialize()
          org.slf4j.LoggerFactory.getLogger(classToLoggerName(o))
        })
      if (record.level.value >= Level.Error.value) {
        if (slf4jLogger.isErrorEnabled) {
          slf4jLogger.error("{}", record.message.value, record.throwable.orNull)
        }
      } else if (record.level.value >= Level.Warn.value) {
        if (slf4jLogger.isWarnEnabled) {
          slf4jLogger.warn("{}", record.message.value, record.throwable.orNull)
        }
      } else if (record.level.value >= Level.Info.value) {
        if (slf4jLogger.isInfoEnabled) {
          slf4jLogger.info("{}", record.message.value, record.throwable.orNull)
        }
      } else if (record.level.value >= Level.Debug.value) {
        if (slf4jLogger.isDebugEnabled) {
          slf4jLogger.debug("{}", record.message.value, record.throwable.orNull)
        }
      } else {
        if (slf4jLogger.isTraceEnabled) {
          slf4jLogger.trace("{}", record.message.value, record.throwable.orNull)
        }
      }
    }
  }

  // ".$anon" or "$$anon$1"  or .<...> in Scala class name
  private val ClassNameGarbage = """([.$]\$|\.<).*""".r

  private def classToLoggerName(className: String) =
    ClassNameGarbage.replaceFirstIn(className, "")
}
