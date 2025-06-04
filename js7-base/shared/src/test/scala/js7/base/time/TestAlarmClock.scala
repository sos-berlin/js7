package js7.base.time

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.time.ScalaTime.*
import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration
import scala.util.NotGiven

trait TestAlarmClock(start: Timestamp)(using ioRuntime: IORuntime) extends AlarmClock:

  protected final val scheduler = TestScheduler(start, ioRuntime)

  final def epochMilli() = scheduler.nowMillis()

  override def lock[A](body: => A)(using @unused u: NotGiven[A <:< IO[?]]): A =
    scheduler.lock(body)

  /** TestAlarmClock synchronizes time query and time change. */
  override def lockIO[A](body: Timestamp => IO[A]): IO[A] =
    scheduler.lockIO(body)

  /** Reset the wall clock but not the monotonic clock (move the clock hands).
   * <p>
   * `timestamp` may be before `now()`.
   */
  final def resetTo(timestamp: Timestamp): Unit =
    scheduler.resetNow(timestamp)

  /** Proceed the clock. */
  final def +=(duration: FiniteDuration): Unit =
    tick(duration)

  /** Proceed the clock.
   * <p>
   * `timestamp` must not be before `now()` (?).
   */
  final def :=(timestamp: Timestamp): Unit =
    tick1(timestamp - now())

  final def tick(duration: FiniteDuration = ZeroDuration): Unit =
    tick1(duration)

  private final def tick1(duration: FiniteDuration): Unit =
    scheduler.tick(duration)


object TestAlarmClock:

  def apply(start: Timestamp)(using IORuntime): TestAlarmClock =
    SimpleTestAlarmClock(start)

  def apply(start: Timestamp, clockCheckInterval: Option[FiniteDuration])
    (using IORuntime)
  : TestAlarmClock =
    clockCheckInterval match
      case None => SimpleTestAlarmClock(start)
      case Some(interval) => ClockCheckingTestAlarmClock(start, interval)

  //def resource(start: Timestamp, clockCheckInterval: FiniteDuration)
  //: ResourceIO[TestAlarmClock] =
  //  Resource.make(
  //    acquire = IO:
  //      ClockCheckingTestAlarmClock((start, clockCheckInterval))(
  //    release = clock => IO:
  //      clock.stop())


  private final class SimpleTestAlarmClock(start: Timestamp)(using IORuntime)
  extends TestAlarmClock(start), AlarmClock.Standard:
    override def productPrefix = "TestAlarmClock"


  /** Only to test the ClockChecking feature. */
  private[time] def forTest(start: Timestamp, clockCheckInterval: FiniteDuration)
    (using IORuntime)
  : TestAlarmClock =
    ClockCheckingTestAlarmClock(start, clockCheckInterval)


  private final class ClockCheckingTestAlarmClock(
    start: Timestamp,
    protected val clockCheckInterval: FiniteDuration)
    (using IORuntime)
  extends TestAlarmClock(start), ClockChecking
