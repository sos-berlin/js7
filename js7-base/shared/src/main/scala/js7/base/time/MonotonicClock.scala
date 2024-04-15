package js7.base.time

import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.concurrent.duration.FiniteDuration

// TODO Similar to AlarmClock, SyncDeadline, CatsDeadline ?
//  Only used for WaitForCondition?
private trait MonotonicClock:
  def now: Deadline

  def deadline(sinceZero: FiniteDuration): Deadline =
    Deadline(sinceZero)

  final case class Deadline(sinceZero: FiniteDuration)
    extends Ordered[Deadline]:

    private implicit def clock: MonotonicClock = MonotonicClock.this

    /**
     * Return a deadline advanced (i.e., moved into the future) by the given duration.
     */
    def +(duration: FiniteDuration): Deadline =
      copy(sinceZero = sinceZero + duration)

    /**
     * Return a deadline moved backwards (i.e., towards the past) by the given duration.
     */
    def -(duration: FiniteDuration): Deadline =
      copy(sinceZero = sinceZero - duration)

    /**
     * Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative).
     */
    def -(other: Deadline): FiniteDuration =
      sinceZero - other.sinceZero

    /**
     * Determine whether the deadline lies in the past at the point where this method is called.
     *
     * '''''Note that on some systems this operation is costly because it entails a system call.'''''
     * Check `System.nanoTime` for your platform.
     */
    def isOverdue: Boolean =
      hasElapsed

    /**
     * Determine whether the deadline still lies in the future at the point where this method is called.
     *
     * '''''Note that on some systems this operation is costly because it entails a system call.'''''
     * Check `System.nanoTime` for your platform.
     */
    def hasTimeLeft: Boolean =
      timeLeft >= ZeroDuration

    /** Returns true even if timeLeft == 0, this is different to `timeLeft` */
    def hasElapsed: Boolean =
      !hasTimeLeft

    def elapsedOrZero: FiniteDuration =
      elapsed max ZeroDuration

    def elapsed: FiniteDuration =
      -timeLeft

    def timeLeftOrZero: FiniteDuration =
      timeLeft max ZeroDuration

    /**
     * Calculate time difference between this duration and now; the result is negative if the deadline has passed.
     *
     * '''''Note that on some systems this operation is costly because it entails a system call.'''''
     * Check `System.nanoTime` for your platform.
     */
    def timeLeft: FiniteDuration =
      sinceZero - clock.now.sinceZero

    /**
     * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
     */
    def compare(other: Deadline): Int =
      sinceZero compare other.sinceZero

    /** Not immutable, may return a different string for each call. */
    override def toString: String =
      val elapsed = this.elapsed
      "Deadline(" + (elapsed.isPositive ?? "+") + elapsed.pretty + ")"


private trait BlockingSleeper extends MonotonicClock:
  def sleepUntil(until: Deadline): Unit

  def sleep(duration: FiniteDuration): Unit


private trait RealtimeClock extends MonotonicClock:
  private implicit val implicitClock: MonotonicClock = this

  def now = Deadline(System.currentTimeMillis().ms)

private object RealtimeClock extends RealtimeClock


private final class RealtimeBlockingSleeper extends RealtimeClock, BlockingSleeper:
  def sleepUntil(until: Deadline): Unit =
    ScalaTime.sleep(until - now)

  def sleep(duration: FiniteDuration): Unit =
    ScalaTime.sleep(duration)


private class TestMonotonicClock(sinceZero: FiniteDuration = ZeroDuration) extends MonotonicClock:
  import TestMonotonicClock.logger

  @volatile private var time = deadline(sinceZero)

  def now = time

  //def +(duration: FiniteDuration): TestMonotonicClock =
  //  TestMonotonicClock(time.sinceZero + duration)

  def tickUntil(until: Deadline): Unit =
    tick(until - now)

  def tick(duration: FiniteDuration): Unit =
    logger.trace(s"Tick $duration => ${time + duration}")
    time += duration
    logger.trace(s"Tick $duration => ${time} #2")


private object TestMonotonicClock:
  private val logger = Logger[this.type]


private final class TestBlockingSleeper(sinceZero: FiniteDuration = ZeroDuration)
extends TestMonotonicClock(sinceZero), BlockingSleeper:

  def sleepUntil(until: Deadline): Unit =
    tickUntil(until)

  def sleep(duration: FiniteDuration): Unit =
    tick(duration)
