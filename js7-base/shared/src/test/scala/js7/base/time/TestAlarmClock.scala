package js7.base.time

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.unsafe.Scheduler
import js7.base.catsutils.CatsEffectExtensions.unsafeScheduler
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import scala.concurrent.duration.FiniteDuration

trait TestAlarmClock extends AlarmClock:

  /** Manipulate the clock. */
  def resetTo(timestamp: Timestamp): IO[Unit]

  /** Proceed the clock.
   * `timestamp` must not be less than `now()`. */
  def set(timestamp: Timestamp): IO[Unit]

  def tickEpsilon: IO[Unit] =
    tick(1.ns)

  def tick(duration: FiniteDuration = ZeroDuration): IO[Unit]


object TestAlarmClock:
  private val logger = Logger[this.type]

  def apply(start: Timestamp, clockCheckInterval: Option[FiniteDuration] = None)
    (using Scheduler)
  : TestAlarmClock =
    clockCheckInterval match
      case None => new SimpleTestAlarmClock(start)
      case Some(interval) => new ClockCheckingTestAlarmClock(start, interval)

  def resource(start: Timestamp, clockCheckInterval: Option[FiniteDuration] = None)
  : Resource[IO, TestAlarmClock] =
    Resource.make(
      acquire =
        IO.unsafeScheduler.map(implicit scheduler =>
          TestAlarmClock(start, clockCheckInterval)))(
      release =
        clock => IO(clock.stop()))

  private final class SimpleTestAlarmClock(start: Timestamp)
    (using protected val scheduler: Scheduler)
  extends Impl(start), AlarmClock.Standard:

    override def productPrefix = "TestAlarmClock"

    override def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
      (using fullName: sourcecode.FullName)
    : Runnable =
      logger.trace(s"scheduleOnce ${delay.pretty} for ${fullName.value}")
      super.scheduleOnce(delay):
        logger.trace(s"🔔 scheduled ${delay.pretty}")
        callback

    override def scheduleAt(at: Timestamp)(callback: => Unit)
      (using fullName: sourcecode.FullName)
    : Runnable =
      logger.trace(s"scheduleAt $at for ${fullName.value}")
      super.scheduleAt(at):
        logger.trace(s"🔔 scheduled $at for ${fullName.value}")
        callback


  //private trait OwnScheduler:
  //  this: TestAlarmClock =>
  //
  //  private val timeToQueue = mutable.TreeMap.empty[Long, mutable.ArrayBuffer[Runnable]]
  //
  //  override def scheduleAt(at: Timestamp)(callback: => Unit)
  //    (using fullName: sourcecode.FullName)
  //  : Runnable =
  //    logger.trace(s"scheduleAt $at for ${fullName.value}")
  //    scheduleOnce(at - now())(callback)
  //
  //  override def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
  //    (using fullName: sourcecode.FullName)
  //  : Runnable =
  //    logger.trace(s"scheduleOnce ${delay.pretty} for ${fullName.value}")
  //    val t = scheduler.monotonicNanos() + delay.toNanos
  //    val runnable: Runnable = () =>
  //      //?logger.trace(s"🔔 scheduled ${delay.pretty}")
  //      callback
  //
  //    addTimer(t, runnable)
  //    runScheduler()
  //    () => removeTimer(t, runnable)
  //
  //  private def addTimer(t: Long, runnable: Runnable): Unit =
  //    timeToQueue.synchronized:
  //      timeToQueue.updateWith(t): q =>
  //        Some(q.fold(ArrayBuffer(runnable))(_ :+ runnable))
  //
  //  private def removeTimer(t: Long, runnable: Runnable): Unit =
  //    timeToQueue.synchronized:
  //      timeToQueue.updateWith(t):
  //        _.flatMap: q =>
  //          q -= runnable
  //          q.??
  //
  //  private def runScheduler() =
  //    timeToQueue.synchronized:
  //      boundary:
  //        while true do
  //          if timeToQueue.isEmpty then break()
  //          val t = timeToQueue.keys.head
  //          if scheduler.monotonicNanos() < t then break()
  //          for runnable <- timeToQueue.remove(t).get do
  //            try runnable.run()
  //            catch case NonFatal(t) => logger.error(s"Timer failed: ${t.toStringWithCauses}", t)


  private transparent trait Impl(start: Timestamp) extends TestAlarmClock:

    private var _baseTimestamp = start
    private val initialMonotonicNano = scheduler.monotonicNanos()

    final def epochMilli() =
      //_now
      (_baseTimestamp + (scheduler.monotonicNanos() - initialMonotonicNano).ns).toEpochMilli

    final def resetTo(timestamp: Timestamp): IO[Unit] =
      IO:
        logger.info(s"⏰ resetTo $timestamp")
        _baseTimestamp = timestamp
      //mutex.lock(IO:
      //  logger.info(s"⏰ resetTo $timestamp")
      //  _now = timestamp.toEpochMilli)

    final def set(timestamp: Timestamp): IO[Unit] =
      logger.infoIO:
        tick1(timestamp - now())

    final def tick(duration: FiniteDuration = ZeroDuration) =
      tick1(duration)

    private final def tick1(duration: FiniteDuration): IO[Unit] =
      IO.defer:
        assertThat(!duration.isNegative)
        logger.info(s"⏰ <-- ${now() + duration} (+${duration.pretty})")
        IO.sleep(duration) *>
          IO.cede /*try to run timers now*/

  end Impl


  /** Only to test the ClockChecking feature. */
  private[time] def forTest(start: Timestamp, clockCheckInterval: FiniteDuration)
  : IO[TestAlarmClock] =
    for scheduler <- IO.unsafeScheduler yield
      given Scheduler = scheduler
      new ClockCheckingTestAlarmClock(start, clockCheckInterval)

  private final class ClockCheckingTestAlarmClock(
    start: Timestamp,
    protected val clockCheckInterval: FiniteDuration)
    (using protected val scheduler: Scheduler)
  extends Impl(start), ClockChecking
