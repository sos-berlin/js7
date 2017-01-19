package com.sos.scheduler.engine.data.log

/**
 * @author Joacim Zschimmer
 */
trait SchedulerLogger {
  def info(message: String): Unit = log(SchedulerLogLevel.info, message)
  def warn(message: String): Unit = log(SchedulerLogLevel.warning, message)
  def error(message: String): Unit = log(SchedulerLogLevel.error, message)
  def debug(message: String): Unit = log(SchedulerLogLevel.debug3, message)
  def log(level: SchedulerLogLevel, message: String): Unit
}
