package js7.base.time

import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import monix.execution.atomic.{Atomic, AtomicLong}
import monix.execution.cancelables.SerialCancelable
import monix.execution.schedulers.TestScheduler
import monix.execution.{Cancelable, Scheduler}
import scala.collection.mutable
import scala.concurrent.duration._

trait AlarmClock extends WallClock
{
  def stop(): Unit

  def scheduleAt(at: Timestamp)(callback: => Unit): Cancelable
}

trait TestAlarmClock extends AlarmClock
{
  def :=(timestamp: Timestamp): Unit

  final def +=(duration: FiniteDuration): Unit =
    this := now() + duration

  final def -=(duration: FiniteDuration): Unit =
    this += -duration

  def tick(duration: FiniteDuration = ZeroDuration): Unit

  final def tickUntil(timestamp: Timestamp): Unit =
    tick(timestamp - now())
}

object AlarmClock
{
  private val logger = scribe.Logger[this.type]

  def apply(clockCheckInterval: FiniteDuration)(implicit s: Scheduler): AlarmClock =
    new Standard(clockCheckInterval)

  def forTest(start: Timestamp, clockCheckInterval: FiniteDuration)
    (implicit scheduler: Scheduler)
  : TestAlarmClock =
    new TestAlarmClockImpl(start, clockCheckInterval)

  private final class Standard(val clockCheckInterval: FiniteDuration)
    (implicit val scheduler: Scheduler)
  extends AlarmClock with Impl {
    def epochMilli() = scheduler.clockRealTime(MILLISECONDS)
  }

  private trait Impl extends Runnable {
    this: AlarmClock =>

    protected def clockCheckInterval: FiniteDuration
    protected def scheduler: Scheduler

    private val epochMilliToAlarms = mutable.SortedMap.empty[Long, Vector[Alarm]]
    // Same as epochMilliToAlarms.headOption.fold(Long.MaxValue)(_._1).
    // Can be read unsynchronized.
    @volatile private var nextMilli = Long.MaxValue
    private val timer = SerialCancelable()

    // We tick to check if clock time has been changed, as long as we have an alarm
    private lazy val tickInterval = clockCheckInterval.toMillis
    private val ticker = SerialCancelable()
    @volatile private var ticking = false

    @volatile private var stopped = false

    def stop(): Unit = {
      stopped = true
      synchronized {
        epochMilliToAlarms.values.view.flatten.foreach(_.cancel())
        epochMilliToAlarms.clear()
      }
    }

    final def scheduleAt(at: Timestamp)(callback: => Unit): Cancelable = {
      val milli: Long = at.toEpochMilli
      val alarm = new Alarm(milli, callback)
      synchronized {
        epochMilliToAlarms += milli -> (epochMilliToAlarms.getOrElse(milli, Vector.empty) :+ alarm)
        scheduleNext()
      }
      alarm
    }

    private def scheduleNext(): Unit =
      epochMilliToAlarms.headOption match {
        case None =>
          stopTicking()
          nextMilli = Long.MaxValue

        case Some((firstMilli, _)) =>
          // Reschedule at each tick, in case the clock has been adjusted
          val now = epochMilli()
          val delay = (firstMilli - now) max 0
          if (delay > tickInterval) {
            startTicking()
          } else {
            stopTicking()
          }
          logger.trace(s"scheduleOnce ${delay.ms.pretty} (${Timestamp.ofEpochMilli(now)})")
          timer := scheduler.scheduleOnce(delay, MILLISECONDS, this)
          nextMilli = firstMilli
      }

    private def startTicking() =
      if (!ticking) {
        ticking = true
        ticker := scheduler.scheduleAtFixedRate(tickInterval, tickInterval, MILLISECONDS, this)
        logger.trace(s"Ticking ${tickInterval.ms.pretty}")
      }

    private def stopTicking() =
      if (ticking) {
        ticking = false
        timer := Cancelable.empty
        logger.trace(s"Ticking stopped")
      }

    protected final def checkSchedulesNow() =
      scheduler.scheduleOnce(0, MILLISECONDS, this)

    final def run(): Unit =
      if (!stopped) {
        if (nextMilli <= epochMilli()) {
          var alarms = Vector.empty[Alarm]
          synchronized {
            while (epochMilliToAlarms.nonEmpty && epochMilliToAlarms.firstKey <= epochMilli()) {
              alarms ++= epochMilliToAlarms.remove(epochMilliToAlarms.firstKey).get
            }
          }
          logger.trace(s"${alarms.size} alarms")
          for (a <- alarms) a.call()
        } else
          logger.trace("Tick")

        synchronized {
          scheduleNext()
        }
      }

    override def toString =
      s"AlarmClock(" +
        epochMilliToAlarms.headOption.fold("no alarm")(o =>
          Timestamp.ofEpochMilli(o._1).toString +
            ((epochMilliToAlarms.size > 1) ?? s", ${epochMilliToAlarms.size} alarms")) +
        (ticking ?? ", ticking") +
        ")"

    private final class Alarm(epochMilli: Long, callback: => Unit)
    extends Cancelable {
      private val called = Atomic(false)

      def call(): Unit =
        if (!called.getAndSet(true)) {
          callback
        }

      def cancel() =
        Impl.this.synchronized {
          for (alarms <- epochMilliToAlarms.get(epochMilli)) {
            val remaining = alarms.filterNot(_ eq this)
            if (remaining.nonEmpty) {
              epochMilliToAlarms += epochMilli -> remaining
            } else {
              epochMilliToAlarms -= epochMilli
              scheduleNext()
            }
          }
        }
    }
  }

  private final class TestAlarmClockImpl(
    start: Timestamp,
    protected val clockCheckInterval: FiniteDuration)
    (implicit val scheduler: Scheduler)
  extends TestAlarmClock with Impl
  {
    private val clock = AtomicLong(start.toEpochMilli)

    def epochMilli() = clock()

    def :=(timestamp: Timestamp): Unit = {
      logger.debug(s"TestAlarmClock := $timestamp")
      clock := timestamp.toEpochMilli
    }

    /** With TestScheduler: tick it, other Scheduler: checkSchedulesNow. */
    def tick(duration: FiniteDuration = ZeroDuration) = {
      assertThat(!duration.isNegative)
      synchronized {
        clock += duration.toMillis

        scheduler match {
          case scheduler: TestScheduler => scheduler.tick(duration)
          case _ => checkSchedulesNow()
        }
      }
    }
  }
}
