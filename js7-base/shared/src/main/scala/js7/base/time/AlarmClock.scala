package js7.base.time

import cats.effect.kernel.{Resource, Sync}
import cats.effect.unsafe.IORuntime
import scala.concurrent.duration.FiniteDuration

//
//import cats.effect.unsafe.{IORuntime, Scheduler}
//import cats.effect.{IO, Resource, Sync}
//import js7.base.monixlike.SerialCancelable
//import js7.base.time.ScalaTime.*
//import js7.base.utils.CatsUtils.syntax.scheduleAtFixedRate
//import js7.base.utils.ScalaUtils.syntax.RichBoolean
//import js7.base.utils.{Atomic, EmptyRunnable}
//import scala.annotation.unused
//import scala.collection.mutable
//import scala.concurrent.duration.*
//
@deprecated("NOT IMPLEMENTED❗️")
trait AlarmClock extends WallClock:
  ???

//  def stop(): Unit
//
//  def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
//    (using sourcecode.FullName)
//  : Runnable
//
//  def scheduleAt(at: Timestamp)(callback: => Unit)
//    (using sourcecode.FullName)
//  : Runnable
//
//  def sleep(delay: FiniteDuration): IO[Unit] =
//    IO.async_[Unit](callback =>
//      scheduleOnce(delay)(callback(Right(()))))
//
//  def sleepUntil(ts: Timestamp): IO[Unit] =
//    IO.async_[Unit](callback =>
//      scheduleAt(ts)(callback(Right(()))))
//
//  /** TestAlarmClock synchronizes time query and time change. */
//  def lock[A](body: => A): A =
//    body


object AlarmClock:
  def apply(clockCheckInterval: Option[FiniteDuration] = None)(implicit ior: IORuntime)
  : AlarmClock =
    null // FIXME
//    given Scheduler = ior.scheduler
//    clockCheckInterval match
//      case None =>
//        new SimpleAlarmClock
//      case Some(clockCheckInterval) =>
//        new ClockCheckingAlarmClock(clockCheckInterval)

  def resource[F[_]](clockCheckInterval: Option[FiniteDuration] = None)
    (using F: Sync[F], ior: IORuntime)
  : Resource[F, AlarmClock] =
    Resource.pure(null: AlarmClock)
//    Resource.make(
//      acquire = F.delay(AlarmClock(clockCheckInterval)))(
//      release = clock => F.delay(clock.stop()))
//
//  private[time] trait Simple:
//    self: AlarmClock =>
//
//    protected def scheduler: Scheduler
//
//    def stop() = {}
//
//    def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
//      (implicit @unused fullName: sourcecode.FullName)
//    : Runnable =
//      scheduler.sleep(delay, () => callback)
//
//    def scheduleAt(at: Timestamp)(callback: => Unit)
//      (implicit @unused fullName: sourcecode.FullName)
//    : Runnable =
//      scheduler.sleep((at - now()) max Duration.Zero, () =>callback)
//
//  private final class SimpleAlarmClock()(using s: Scheduler)
//  extends AlarmClock with Simple:
//    def epochMilli() = scheduler.nowMillis()
//    protected def scheduler = s
//
//  private final class ClockCheckingAlarmClock(val clockCheckInterval: FiniteDuration)
//    (using val s: Scheduler)
//  extends AlarmClock with ClockChecking:
//    def epochMilli() = scheduler.nowMillis()
//    protected def scheduler = s
//
//  /** Checks the clock for change and corrects the schedule. */
//  private[time] trait ClockChecking extends Runnable:
//    self: AlarmClock =>
//
//    protected def clockCheckInterval: FiniteDuration
//    protected def scheduler: Scheduler
//
//    private val epochMilliToAlarms = mutable.SortedMap.empty[Long, Vector[Alarm]]
//    // Same as epochMilliToAlarms.headOption.fold(Long.MaxValue)(_._1).
//    // Can be read unsynchronized.
//    @volatile private var nextMilli = Long.MaxValue
//    private val timer = SerialCancelable()
//
//    // We tick to check if clock time has been changed, as long as we have an alarm
//    private lazy val tickInterval = clockCheckInterval.toMillis
//    private val ticker = SerialCancelable()
//    @volatile private var ticking = false
//    @volatile private var stopped = false
//
//    def stop() =
//      stopped = true
//      self.synchronized:
//        epochMilliToAlarms.values.view.flatten.foreach(_.cancel())
//        epochMilliToAlarms.clear()
//
//    final def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
//      (implicit fullName: sourcecode.FullName)
//    : Runnable =
//      scheduleAt(now() + delay)(callback)
//
//    final def scheduleAt(at: Timestamp)(callback: => Unit)
//      (implicit @unused fullName: sourcecode.FullName)
//    : Runnable =
//      val milli = at.toEpochMilli
//      val alarm = new Alarm(milli, callback)
//      self.synchronized:
//        epochMilliToAlarms.update(milli, epochMilliToAlarms.getOrElse(milli, Vector.empty) :+ alarm)
//        scheduleNext()
//      alarm
//
//    private def scheduleNext(): Unit =
//      epochMilliToAlarms.headOption match
//        case None =>
//          stopTicking()
//          nextMilli = Long.MaxValue
//
//        case Some((firstMilli, _)) =>
//          // Reschedule at each tick, in case the clock has been adjusted
//          val now = epochMilli()
//          val delay = (firstMilli - now) max 0
//          if delay > tickInterval then
//            startTicking(tickInterval.ms)
//          else
//            stopTicking()
//          //logger.trace(s"scheduleOnce ${delay.ms.pretty} (${Timestamp.ofEpochMilli(now)})")
//          timer := scheduler.sleep(delay.ms, this)
//          nextMilli = firstMilli
//
//    private def startTicking(tickInterval: FiniteDuration) =
//      if !ticking then
//        ticking = true
//        ticker := scheduler.scheduleAtFixedRate(tickInterval, tickInterval, this)
//        //logger.trace(s"Ticking ${tickInterval.ms.pretty}")
//
//    private def stopTicking() =
//      if ticking then
//        ticking = false
//        ticker := EmptyRunnable
//        //logger.trace(s"Ticking stopped")
//
//    final def run(): Unit =
//      if !stopped then
//        if nextMilli <= epochMilli() then
//          var alarms = Vector.empty[Alarm]
//          self.synchronized:
//            while epochMilliToAlarms.nonEmpty && epochMilliToAlarms.firstKey <= epochMilli() do
//              alarms ++= epochMilliToAlarms.remove(epochMilliToAlarms.firstKey).get
//          //logger.trace(s"Tick: ${alarms.size} alarms!")
//          for a <- alarms do a.call()
//        else {
//          //logger.trace("Tick")
//        }
//
//        self.synchronized:
//          scheduleNext()
//
//    override def toString =
//      productPrefix + "(" + now() + ", " +
//        epochMilliToAlarms.headOption.fold("no alarm")(o =>
//          "alarms=" +
//            Timestamp.ofEpochMilli(o._1).toString +
//            ((epochMilliToAlarms.size > 1) ?? s", ${epochMilliToAlarms.size} alarms")) +
//        (ticking ?? s", ticking ${clockCheckInterval.pretty}") +
//        ")"
//
//    private final class Alarm(epochMilli: Long, callback: => Unit)
//    extends Runnable:
//      private val called = Atomic(false)
//
//      def call(): Unit =
//        if !called.getAndSet(true) then
//          callback
//
//      def run() = cancel()
//
//      def cancel() =
//        self.synchronized:
//          for alarms <- epochMilliToAlarms.get(epochMilli) do
//            val remaining = alarms.filterNot(_ eq this)
//            if remaining.nonEmpty then
//              epochMilliToAlarms.update(epochMilli, remaining)
//            else
//              epochMilliToAlarms -= epochMilli
//              scheduleNext()
