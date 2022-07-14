package js7.base.time

import monix.execution.schedulers.SchedulerService
import monix.execution.{Cancelable, ExecutionModel, UncaughtExceptionReporter}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

// NOT USED
private final class AlarmClockScheduler(underlying: SchedulerService, val alarmClock: AlarmClock)
extends SchedulerService
{
  def isShutdown =
    underlying.isShutdown

  def isTerminated =
    underlying.isTerminated

  def shutdown() = {
    alarmClock.stop()
    underlying.shutdown()
  }

  def awaitTermination(timeout: Long, unit: TimeUnit, awaitOn: ExecutionContext) =
    underlying.awaitTermination(timeout, unit, awaitOn)

  def withExecutionModel(em: ExecutionModel) =
    new AlarmClockScheduler(
      underlying.withExecutionModel(em),
      alarmClock)

  def withUncaughtExceptionReporter(r: UncaughtExceptionReporter) =
    new AlarmClockScheduler(
      underlying.withUncaughtExceptionReporter(r),
      alarmClock)

  def execute(runnable: Runnable) =
    underlying.execute(runnable)

  def scheduleOnce(initialDelay: Long, unit: TimeUnit, r: Runnable) =
    underlying.scheduleOnce(initialDelay, unit, r)

  def scheduleWithFixedDelay(initialDelay: Long, delay: Long, unit: TimeUnit, r: Runnable) =
    underlying.scheduleWithFixedDelay(initialDelay, delay, unit, r)

  def scheduleAtFixedRate(initialDelay: Long, period: Long, unit: TimeUnit, r: Runnable) =
    underlying.scheduleAtFixedRate(initialDelay, period, unit, r)

  def clockRealTime(unit: TimeUnit) = {
    val ms = alarmClock.epochMilli()
    unit.convert(ms, MILLISECONDS)
  }

  def clockMonotonic(unit: TimeUnit) =
    underlying.clockMonotonic(unit)

  def reportFailure(t: Throwable) =
    underlying.reportFailure(t)

  def executionModel =
    underlying.executionModel


  def scheduleAt(timestamp: Timestamp)(callback: => Unit): Cancelable =
    alarmClock.scheduleAt(timestamp)(callback)
}
