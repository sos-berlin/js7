//package js7.base.time
//
//import cats.effect.IO
//import cats.effect.testkit.TestControl
//import cats.effect.unsafe.Scheduler
//import js7.base.log.Logger
//import js7.base.time.AlarmClock.{ClockChecking, Simple}
//import js7.base.time.ScalaTime.*
//import js7.base.utils.Assertions.assertThat
//import js7.base.utils.Atomic
//import js7.base.utils.Atomic.extensions.*
//import scala.concurrent.duration.FiniteDuration
//
//trait TestAlarmClock extends AlarmClock:
//  /** Manipulate the clock. */
//  def resetTo(timestamp: Timestamp): Unit
//
//  /** Proceed the clock.
//   * `timestamp` must not be less than `now()`. */
//  def :=(timestamp: Timestamp): Unit
//
//  def +=(duration: FiniteDuration): Unit
//
//  def tick(duration: FiniteDuration = ZeroDuration): Unit
//
//
//object TestAlarmClock:
//  private val logger = Logger[this.type]
//
//  def apply(start: Timestamp): TestAlarmClock =
//    new SimpleTestAlarmClock(start)
//
//  private final class SimpleTestAlarmClock(protected val start: Timestamp)
//  extends Simple, Impl:
//    override def productPrefix = "TestAlarmClock"
//
//    override def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
//      (implicit fullName: sourcecode.FullName)
//    : Runnable =
//      logger.trace(s"scheduleOnce ${delay.pretty} for ${fullName.value}")
//      super.scheduleOnce(delay):
//        logger.trace("🔔 scheduled " + delay.pretty)
//        callback
//
//    override def scheduleAt(at: Timestamp)(callback: => Unit)
//      (implicit fullName: sourcecode.FullName)
//    : Runnable =
//      logger.trace(s"scheduleAt $at for ${fullName.value}")
//      super.scheduleAt(at):
//        logger.trace(s"🔔 scheduled $at for ${fullName.value}")
//        callback
//
//    protected def scheduler: Scheduler = ??? // FIXME
//
//    protected def testControl: TestControl[_] = ??? // FIXME
//
//  private trait Impl extends TestAlarmClock:
//    protected def testControl: TestControl[?]
//    protected def start: Timestamp
//
//    private val clock = Atomic(start.toEpochMilli)
//
//    final def epochMilli() = clock.get
//
//    override def lock[A](body: => A): A =
//      synchronized(body)
//
//    final def resetTo(timestamp: Timestamp): Unit =
//      synchronized:
//        logger.info(s"⏰ resetTo $timestamp")
//        clock := timestamp.toEpochMilli
//
//    final def +=(duration: FiniteDuration): Unit =
//      synchronized:
//        this := now() + duration
//
//    final def :=(timestamp: Timestamp): Unit =
//      synchronized:
//        logger.info(s"⏰ := $timestamp")
//        tick1(timestamp - now())
//
//    final def tick(duration: FiniteDuration = ZeroDuration) =
//      synchronized:
//        tick1(duration)
//
//    private final def tick1(duration: FiniteDuration): IO[Unit] =
//      IO.defer:
//        assertThat(!duration.isNegative)
//        clock += duration.toMillis
//        testControl.tickFor(duration)
//
//  /** Only to test the ClockChecking feature. */
//  private[time] def forTest(start: Timestamp, clockCheckInterval: FiniteDuration)
//  : TestAlarmClock =
//    new ClockCheckingTestAlarmClock(start, clockCheckInterval)
//
//  private final class ClockCheckingTestAlarmClock(
//    protected val start: Timestamp,
//    protected val clockCheckInterval: FiniteDuration)
//  extends Impl, ClockChecking:
//    protected def scheduler: Scheduler = ??? // FIXME
//
//    protected def testControl: TestControl[_] = ??? // FIXME
