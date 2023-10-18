package js7.base.time

import cats.effect.{Resource, Sync}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import monix.eval.Task
import monix.execution.atomic.Atomic
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, Scheduler}
import scala.annotation.unused
import scala.collection.mutable
import scala.concurrent.duration.*

trait AlarmClock extends WallClock:

  def stop(): Unit

  def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
    (implicit fullName: sourcecode.FullName): Cancelable

  def scheduleAt(at: Timestamp)(callback: => Unit)
    (implicit fullName: sourcecode.FullName): Cancelable

  def sleep(delay: FiniteDuration): Task[Unit] =
    Task.create((_, callback) =>
      scheduleOnce(delay)(callback(Right(()))))

  def sleepUntil(ts: Timestamp): Task[Unit] =
    Task.create((_, callback) =>
      scheduleAt(ts)(callback(Right(()))))

  /** TestAlarmClock synchronizes time query and time change. */
  def lock[A](body: => A): A =
    body


object AlarmClock:
  def apply(clockCheckInterval: Option[FiniteDuration] = None)(implicit s: Scheduler): AlarmClock =
    clockCheckInterval match
      case None =>
        new SimpleAlarmClock

      case Some(clockCheckInterval) =>
        new ClockCheckingAlarmClock(clockCheckInterval)

  def resource[F[_]](clockCheckInterval: Option[FiniteDuration] = None)
    (implicit F: Sync[F], s: Scheduler)
  : Resource[F, AlarmClock] =
    Resource.make(
      acquire = F.delay(AlarmClock(clockCheckInterval)))(
      release = clock => F.delay(clock.stop()))

  private final class SimpleAlarmClock
    (implicit val s: Scheduler)
  extends AlarmClock, Simple:
    def epochMilli() = scheduler.clockRealTime(MILLISECONDS)
    protected def scheduler = s

  private final class ClockCheckingAlarmClock(val clockCheckInterval: FiniteDuration)
    (implicit val s: Scheduler)
  extends AlarmClock, ClockChecking:
    def epochMilli() = scheduler.clockRealTime(MILLISECONDS)
    protected def scheduler = s

  private[time] trait Simple:
    self: AlarmClock =>

    protected def scheduler: Scheduler

    def stop() = {}

    def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
      (implicit @unused fullName: sourcecode.FullName): Cancelable =
      scheduler.scheduleOnce(delay)(callback)

    def scheduleAt(at: Timestamp)(callback: => Unit)
      (implicit @unused fullName: sourcecode.FullName): Cancelable =
      scheduler.scheduleOnce((at - now()) max Duration.Zero)(callback)
    //
    //override def toString =
    //  productPrefix + "(" + now() + ")"

  /** Checks the clock for change and corrects the schedule. */
  private[time] trait ClockChecking extends Runnable:
    self: AlarmClock =>

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

    def stop() =
      stopped = true
      self.synchronized:
        epochMilliToAlarms.values.view.flatten.foreach(_.cancel())
        epochMilliToAlarms.clear()

    final def scheduleOnce(delay: FiniteDuration)(callback: => Unit)
      (implicit fullName: sourcecode.FullName)
    : Cancelable =
      scheduleAt(now() + delay)(callback)

    final def scheduleAt(at: Timestamp)(callback: => Unit)
      (implicit @unused fullName: sourcecode.FullName)
    : Cancelable =
      val milli = at.toEpochMilli
      val alarm = new Alarm(milli, callback)
      self.synchronized:
        epochMilliToAlarms.update(milli, epochMilliToAlarms.getOrElse(milli, Vector.empty) :+ alarm)
        scheduleNext()
      alarm

    private def scheduleNext(): Unit =
      epochMilliToAlarms.headOption match
        case None =>
          stopTicking()
          nextMilli = Long.MaxValue

        case Some((firstMilli, _)) =>
          // Reschedule at each tick, in case the clock has been adjusted
          val now = epochMilli()
          val delay = (firstMilli - now) max 0
          if delay > tickInterval then
            startTicking(tickInterval)
          else
            stopTicking()
          //logger.trace(s"scheduleOnce ${delay.ms.pretty} (${Timestamp.ofEpochMilli(now)})")
          timer := scheduler.scheduleOnce(delay, MILLISECONDS, this)
          nextMilli = firstMilli

    private def startTicking(tickInterval: Long) =
      if !ticking then
        ticking = true
        ticker := scheduler.scheduleAtFixedRate(tickInterval, tickInterval, MILLISECONDS, this)
        //logger.trace(s"Ticking ${tickInterval.ms.pretty}")

    private def stopTicking() =
      if ticking then
        ticking = false
        ticker := Cancelable.empty
        //logger.trace(s"Ticking stopped")

    final def run(): Unit =
      if !stopped then
        if nextMilli <= epochMilli() then
          var alarms = Vector.empty[Alarm]
          self.synchronized:
            while epochMilliToAlarms.nonEmpty && epochMilliToAlarms.firstKey <= epochMilli() do
              alarms ++= epochMilliToAlarms.remove(epochMilliToAlarms.firstKey).get
          //logger.trace(s"Tick: ${alarms.size} alarms!")
          for a <- alarms do a.call()
        else {
          //logger.trace("Tick")
        }

        self.synchronized:
          scheduleNext()

    override def toString =
      productPrefix + "(" + now() + ", " +
        epochMilliToAlarms.headOption.fold("no alarm")(o =>
          "alarms=" +
            Timestamp.ofEpochMilli(o._1).toString +
            ((epochMilliToAlarms.size > 1) ?? s", ${epochMilliToAlarms.size} alarms")) +
        (ticking ?? s", ticking ${clockCheckInterval.pretty}") +
        ")"

    private final class Alarm(epochMilli: Long, callback: => Unit)
    extends Cancelable:
      private val called = Atomic(false)

      def call(): Unit =
        if !called.getAndSet(true) then
          callback

      def cancel() =
        self.synchronized:
          for alarms <- epochMilliToAlarms.get(epochMilli) do
            val remaining = alarms.filterNot(_ eq this)
            if remaining.nonEmpty then
              epochMilliToAlarms.update(epochMilli, remaining)
            else
              epochMilliToAlarms -= epochMilli
              scheduleNext()
