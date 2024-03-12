package js7.base.time

import cats.effect.unsafe.Scheduler
import cats.effect.{IO, Resource, Sync}
import js7.base.monixlike.SyncCancelable
import js7.base.time.ScalaTime.*
import js7.base.utils.LabeledRunnable
import scala.concurrent.duration.*
import scala.util.NotGiven

trait AlarmClock extends WallClock:

  def stop(): Unit

  final def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
    (using fullName: sourcecode.FullName)
  : SyncCancelable =
    scheduleOnce(delay, fullName.value)(callback)

  def scheduleOnce(delay: FiniteDuration, label: => String)(callback: => Unit)
  : SyncCancelable

  final def scheduleAt(timestamp: Timestamp)(callback: => Unit)
    (using fullName: sourcecode.FullName)
  : SyncCancelable =
    scheduleAt(timestamp, fullName.value)(callback)

  def scheduleAt(timestamp: Timestamp, label: => String = "")(callback: => Unit)
  : SyncCancelable

  final def sleep(duration: FiniteDuration, label: => String = ""): IO[Unit] =
    sleepX(scheduleOnce(duration, label))

  final def sleepUntil(timestamp: Timestamp, label: => String = ""): IO[Unit] =
    sleepX(scheduleAt(timestamp, label))

  private def sleepX(schedule: (=> Unit) => SyncCancelable): IO[Unit] =
    IO.async: onComplete =>
      IO:
        val cancelable = schedule(onComplete(Right(())))
        Some(IO(cancelable.cancel()))

  /** TestAlarmClock synchronizes time query and time change. */
  def lock[A](body: => A)(using NotGiven[A <:< IO[?]]): A =
    body


object AlarmClock:

  def resource[F[_]](clockCheckInterval: Option[FiniteDuration] = None)
    (using F: Sync[F], scheduler: Scheduler)
  : Resource[F, AlarmClock] =
    Resource(F.delay:
      val clock = AlarmClock(clockCheckInterval)
      clock -> F.delay(clock.stop()))

  def apply(clockCheckInterval: Option[FiniteDuration])(using Scheduler): AlarmClock =
    clockCheckInterval match
      case None => SystemAlarmClock()
      case Some(interval) => ClockCheckingAlarmClock(interval)

  def apply()(using Scheduler): AlarmClock =
    SystemAlarmClock()


  private final class SystemAlarmClock()
    (using protected val scheduler: Scheduler)
  extends SystemWallClock, Standard


  private[time] transparent trait Standard extends AlarmClock:

    protected def scheduler: Scheduler

    def stop() = {}

    def scheduleOnce(delay: FiniteDuration, label: => String)(callback: => Unit) =
      SyncCancelable:
        scheduler.sleep(delay max ZeroDuration, LabeledRunnable(label)(callback))

    def scheduleAt(timestamp: Timestamp, label: => String)(callback: => Unit) =
      val delay = (timestamp - now()) max ZeroDuration
      SyncCancelable:
        scheduler.sleep(delay max ZeroDuration, LabeledRunnable(label)(callback))

  private final class ClockCheckingAlarmClock(val clockCheckInterval: FiniteDuration)
    (using protected val scheduler: Scheduler)
  extends AlarmClock, ClockChecking:
    def epochMilli() = scheduler.nowMillis()
