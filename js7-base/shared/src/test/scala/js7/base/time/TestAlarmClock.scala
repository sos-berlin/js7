package js7.base.time

import cats.effect.{IO, Resource, ResourceIO}
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.FiniteDuration
import scala.util.NotGiven

trait TestAlarmClock(start: Timestamp) extends AlarmClock:

  protected final val scheduler = TestScheduler(start)

  final def epochMilli() = scheduler.nowMillis()

  override def lock[A](body: => A)(using NotGiven[A <:< IO[?]]): A =
    scheduler.lock(body)

  /** Reset the wall clock but not the monotonic clock (move the clock hands).
   * <p>
   * `timestamp` may be before `now()`.
   */
  final def resetTo(timestamp: Timestamp): Unit =
    scheduler.resetNow(timestamp)

  /** Proceed the clock. */
  final def +=(duration: FiniteDuration): Unit =
    this := now() + duration

  /** Proceed the clock.
   * <p>
   * `timestamp` must not be before `now()` (?).
   */
  final def :=(timestamp: Timestamp): Unit =
    tick1(timestamp - now())

  final def tick(duration: FiniteDuration = ZeroDuration) =
    tick1(duration)

  private final def tick1(duration: FiniteDuration) =
    scheduler.tick(duration)


object TestAlarmClock:

  def apply(start: Timestamp, clockCheckInterval: Option[FiniteDuration]): TestAlarmClock =
    clockCheckInterval match
      case None => SimpleTestAlarmClock(start)
      case Some(interval) => ClockCheckingTestAlarmClock(start, interval)

  def apply(start: Timestamp): TestAlarmClock =
    SimpleTestAlarmClock(start)

  def resource(start: Timestamp, clockCheckInterval: Option[FiniteDuration] = None)
  : ResourceIO[TestAlarmClock] =
    Resource.make(
      acquire =
        IO.executionContext.map(implicit ec =>
          TestAlarmClock(start, clockCheckInterval)))(
      release =
        clock => IO(clock.stop()))

  private final class SimpleTestAlarmClock(start: Timestamp)
  extends TestAlarmClock(start), AlarmClock.Standard:
    override def productPrefix = "TestAlarmClock"


  /** Only to test the ClockChecking feature. */
  private[time] def forTest(start: Timestamp, clockCheckInterval: FiniteDuration)
  : TestAlarmClock =
    ClockCheckingTestAlarmClock(start, clockCheckInterval)

  private final class ClockCheckingTestAlarmClock(
    start: Timestamp,
    protected val clockCheckInterval: FiniteDuration)
  extends TestAlarmClock(start), ClockChecking
