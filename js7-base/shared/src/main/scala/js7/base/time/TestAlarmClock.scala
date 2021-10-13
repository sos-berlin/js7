package js7.base.time

import js7.base.time.AlarmClock.{ClockChecking, Simple}
import js7.base.time.ScalaTime.{ZeroDuration, _}
import js7.base.utils.Assertions.assertThat
import monix.execution.atomic.AtomicLong
import monix.execution.schedulers.TestScheduler
import scala.concurrent.duration.FiniteDuration

trait TestAlarmClock extends AlarmClock
{
  /** Manipulate the clock. */
  def resetTo(timestamp: Timestamp): Unit

  /** Proceed the clock.
   * `timestamp` must not be less than `now()`. */
  def :=(timestamp: Timestamp): Unit

  def +=(duration: FiniteDuration): Unit

  def tick(duration: FiniteDuration = ZeroDuration): Unit
}

object TestAlarmClock
{
  private val logger = scribe.Logger[this.type]

  def apply(start: Timestamp): TestAlarmClock =
    new SimpleTestAlarmClock(start)

  private final class SimpleTestAlarmClock(protected val start: Timestamp)
  extends Simple with Impl {
    def prefix = "TestAlarmClock"
  }

  private trait Impl
  extends TestAlarmClock
  {
    protected def start: Timestamp
    protected final val scheduler = TestScheduler()

    private val clock = AtomicLong(start.toEpochMilli)

    final def epochMilli() = clock()

    override def lock[A](body: => A): A =
      synchronized(body)

    final def resetTo(timestamp: Timestamp): Unit =
      synchronized {
        logger.info(s"TestAlarmClock resetTo $timestamp")
        clock := timestamp.toEpochMilli
      }

    final def +=(duration: FiniteDuration): Unit =
      synchronized {
        this := now() + duration
      }

    final def :=(timestamp: Timestamp): Unit =
      synchronized {
        logger.info(s"TestAlarmClock := $timestamp")
        tick1(timestamp - now())
      }

    final def tick(duration: FiniteDuration = ZeroDuration) =
      synchronized {
        tick1(duration)
      }

    private final def tick1(duration: FiniteDuration = ZeroDuration) = {
      assertThat(!duration.isNegative)
      clock += duration.toMillis
      scheduler.tick(duration)
    }
  }

  /** Only to test the ClockChecking feature. */
  private[time] def forTest(start: Timestamp, clockCheckInterval: FiniteDuration)
  : TestAlarmClock =
    new ClockCheckingTestAlarmClock(start, clockCheckInterval)

  private final class ClockCheckingTestAlarmClock(
    protected val start: Timestamp,
    protected val clockCheckInterval: FiniteDuration)
  extends ClockChecking with Impl {
    def prefix = "ClockCheckingTestAlarmClock"
  }
}
