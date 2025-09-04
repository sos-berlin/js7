package js7.base.time

import cats.effect.std.Dispatcher
import cats.effect.unsafe.Scheduler
import cats.effect.{IO, Outcome, Resource, Sync}
import js7.base.log.Logger
import js7.base.monixlike.SyncCancelable
import js7.base.time.AlarmClock.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.{AsyncLock, LabeledRunnable}
import scala.annotation.unused
import scala.concurrent.duration.*
import scala.util.NotGiven

trait AlarmClock extends WallClock:

  private[time] def stop(): Unit

  final def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
    (using fullName: sourcecode.FullName)
  : SyncCancelable =
    scheduleOnce(delay, fullName.value)(callback)

  def scheduleOnce(delay: FiniteDuration, label: => String)(callback: => Unit): SyncCancelable

  final def scheduleAt(timestamp: Timestamp)(callback: => Unit)
    (using fullName: sourcecode.FullName)
  : SyncCancelable =
    scheduleAt(timestamp, fullName.value)(callback)

  def scheduleAt(timestamp: Timestamp, label: => String = "")(callback: => Unit): SyncCancelable

  /** @return An IO in an IO, to allow cancellation of the schedule (but not the scheduled IO). */
  final def scheduleIOOnce(delay: FiniteDuration, label: => String = "")(io: IO[Unit])
    (using dispatcher: Dispatcher[IO])
  : IO[IO[Unit]] =
    scheduleIO(
      scheduleOnce(delay, label), s"scheduleIOOnce($delay, $label)",
      io)

  /** @return An IO in an IO, to allow cancellation of the schedule (but not the scheduled IO). */
  final def scheduleIOAt(timestamp: Timestamp, label: => String = "")(io: IO[Unit])
    (using dispatcher: Dispatcher[IO])
  : IO[IO[Unit]] =
    scheduleIO(
      scheduleAt(timestamp, label), s"scheduleIOAt($timestamp, $label)",
      io)

  private final def scheduleIO(
    schedule: (=> Unit) => SyncCancelable, label: => String, io: IO[Unit])
    (using dispatcher: Dispatcher[IO])
  : IO[IO[Unit]] =
    IO:
      val scheduled = schedule:
        dispatcher.unsafeRunAndForget:
          io.guaranteeCase:
            case Outcome.Succeeded(_) => IO.unit
            case Outcome.Canceled() => IO(logger.debug(s"◼️ $label: Canceled"))
            case Outcome.Errored(t) => IO(logger.error(s"$label: ${t.toStringWithCauses}", t))
      IO(scheduled.cancel())

  // TODO Are sleep and sleepUntil cancelable?
  final def sleep(duration: FiniteDuration, label: => String = ""): IO[Unit] =
    toIO:
      scheduleOnce(duration, label)

  final def sleepUntil(timestamp: Timestamp, label: => String = ""): IO[Unit] =
    toIO:
      scheduleAt(timestamp, label)

  private def toIO(schedule: (=> Unit) => SyncCancelable): IO[Unit] =
    IO.async: onComplete =>
      IO:
        val cancelable = schedule(onComplete(Right(())))
        Some(IO(cancelable.cancel()))

  def lock[A](body: => A)(using @unused u: NotGiven[A <:< IO[?]]): A

  /** TestAlarmClock synchronizes time query and time change. */
  def lockIO[A](body: Timestamp => IO[A])(using sourcecode.Enclosing): IO[A]


object AlarmClock:

  private val logger = Logger[this.type]

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


  /** Use this or use TestAlarmClock. */
  private[time] transparent trait NonTestLocking:
    this: AlarmClock =>

    private val clockLock = AsyncLock()

    def lock[A](body: => A)(using @unused u: NotGiven[A <:< IO[?]]): A =
      body

    /** TestAlarmClock synchronizes time query and time change. */
    def lockIO[A](body: Timestamp => IO[A])(using sourcecode.Enclosing): IO[A] =
      // We lock as TestScheduler does, otherwise the behaviour would differ between testing and
      // production code.
      // If we use a lock when using TestScheduler, we should also use a lock in production.
      clockLock.lock:
        IO.defer:
          body(now())


  private final class SystemAlarmClock()
    (using protected val scheduler: Scheduler)
  extends SystemWallClock, NonTestLocking, Standard


  private[time] transparent trait Standard extends AlarmClock:

    protected def scheduler: Scheduler

    def stop(): Unit = {}

    def scheduleOnce(delay: FiniteDuration, label: => String)(callback: => Unit) =
      SyncCancelable:
        scheduler.sleep(delay max ZeroDuration, LabeledRunnable(label)(callback))

    def scheduleAt(timestamp: Timestamp, label: => String)(callback: => Unit) =
      val delay = (timestamp - now()) max ZeroDuration
      SyncCancelable:
        scheduler.sleep(delay max ZeroDuration, LabeledRunnable(label)(callback))


  private final class ClockCheckingAlarmClock(val clockCheckInterval: FiniteDuration)
    (using protected val scheduler: Scheduler)
  extends AlarmClock, NonTestLocking, ClockChecking:
    def epochMilli() = scheduler.nowMillis()
