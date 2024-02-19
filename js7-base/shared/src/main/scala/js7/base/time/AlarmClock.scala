package js7.base.time

import cats.effect.unsafe.Scheduler
import cats.effect.{IO, Resource, Sync}
import js7.base.monixlike.SyncCancelable
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.*

trait AlarmClock extends WallClock:

  protected def scheduler: Scheduler

  def stop(): Unit

  def scheduleOnce(delay: FiniteDuration)(callback: => Unit)(using sourcecode.FullName)
  : SyncCancelable

  def scheduleAt(timestamp: Timestamp)(callback: => Unit)(using sourcecode.FullName)
  : SyncCancelable

  final def sleep(duration: FiniteDuration): IO[Unit] =
    sleepX(scheduleOnce(duration))

  final def sleepUntil(timestamp: Timestamp): IO[Unit] =
    sleepX(scheduleAt(timestamp))

  private def sleepX(schedule: (=> Unit) => SyncCancelable): IO[Unit] =
    IO.async: onComplete =>
      IO:
        val cancelable = schedule(onComplete(Right(())))
        Some(IO(cancelable.cancel()))

  /** TestAlarmClock synchronizes time query and time change. */
  final def lock[A](body: IO[A]): IO[A] =
    body

  final def lock[A](body: A): A =
    body


object AlarmClock:

  def apply(clockCheckInterval: Option[FiniteDuration] = None)(using Scheduler)
  : AlarmClock =
    clockCheckInterval match
      case None =>
        new StandardAlarmClock
      case Some(clockCheckInterval) =>
        new ClockCheckingAlarmClock(clockCheckInterval)

  def resource[F[_]](clockCheckInterval: Option[FiniteDuration] = None)
    (using F: Sync[F], scheduler: Scheduler)
  : Resource[F, AlarmClock] =
    Resource.make(
      acquire = F.delay(AlarmClock(clockCheckInterval)))(
      release = clock => F.delay(clock.stop()))


  private[time] trait Standard:
    self: AlarmClock =>

    def stop() = {}

    def scheduleOnce(delay: FiniteDuration)(callback: => Unit)(using sourcecode.FullName) =
      SyncCancelable:
        scheduler.sleep(delay max ZeroDuration, () => callback)

    def scheduleAt(timestamp: Timestamp)(callback: => Unit)(using sourcecode.FullName) =
      val delay = (timestamp - now()) max ZeroDuration
      SyncCancelable:
        scheduler.sleep(delay max ZeroDuration, () => callback)


  private final class StandardAlarmClock()
    (using protected val scheduler: Scheduler)
  extends AlarmClock, Standard:
    def epochMilli() = System.currentTimeMillis()

  private final class ClockCheckingAlarmClock(val clockCheckInterval: FiniteDuration)
    (using protected val scheduler: Scheduler)
  extends AlarmClock, ClockChecking:
    def epochMilli() = scheduler.nowMillis()
