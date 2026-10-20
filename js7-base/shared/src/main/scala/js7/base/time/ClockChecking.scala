package js7.base.time

import cats.effect.unsafe.Scheduler
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.scheduleAtFixedRate
import js7.base.monixlike.{SerialSyncCancelable, SyncCancelable}
import js7.base.time.ClockChecking.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Atomic, emptyRunnable}
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Checks the clock for change and corrects the schedule. */
private[time] trait ClockChecking extends Runnable:
  clockChecking: AlarmClock =>

  protected def clockCheckInterval: FiniteDuration
  protected def scheduler: Scheduler

  // TODO Use SortedSet like in TestScheduler. Is this code duplicate?
  private val epochMilliToAlarms = mutable.SortedMap.empty[Long, Vector[Alarm]]

  // Guarded by clockChecking.synchronized. Detects recursive calls of scheduleNext.
  private var scheduleGeneration = 0L

  private val nextAlarmId = Atomic(1L)

  // Same as epochMilliToAlarms.headOption.fold(Long.MaxValue)(_._1).
  // Can be read unsynchronized.
  @volatile private var nextMilli = Long.MaxValue

  private val timer = SerialSyncCancelable()

  // We tick to check if clock time has been changed, as long as we have an alarm
  private lazy val tickInterval = clockCheckInterval.toMillis
  private val ticker = SerialSyncCancelable()
  @volatile private var ticking = false
  @volatile private var stopped = false

  def stop(): Unit =
    logger.trace(s"⏰ stop $this")
    stopped = true
    stopTicking()
    ticker.cancel()
    clockChecking.synchronized:
      epochMilliToAlarms.values.view.flatten.foreach(_.cancel())
      epochMilliToAlarms.clear()

  final def scheduleOnce(delay: FiniteDuration, label: => String)(callback: => Unit)
  : SyncCancelable =
    scheduleAt(now() + delay, label)(callback)

  final def scheduleAt(at: Timestamp, label: => String)(callback: => Unit): SyncCancelable =
    val milli = at.toEpochMilli
    if toMs(milli - epochMilli()).isFailure then
      logger.warn(s"scheduleAt($at, $label) ignored because it is out of range")
      SyncCancelable.empty
    else
      val alarm = Alarm(nextAlarmId.getAndIncrement(), milli, label, callback)
      logger.trace(s"⏰ scheduleAt $alarm")
      clockChecking.synchronized:
        epochMilliToAlarms.update(milli, epochMilliToAlarms.getOrElse(milli, Vector.empty) :+ alarm)
        scheduleNext()
      SyncCancelable(alarm)

  private def scheduleNext(): Unit =
    if stopped then
      logger.trace("⏰ scheduleNext: stopped")
    else epochMilliToAlarms.headOption match
      case None =>
        logger.trace(s"⏰ scheduleNext: no alarm (nextMilli was ${milliToString(nextMilli)})")
        stopTicking()
        nextMilli = Long.MaxValue

      case Some((firstMilli, alarms)) =>
        // Reschedule at each tick, in case the clock has been adjusted
        val now = epochMilli()
        val delay = (firstMilli - now) max 0
        if delay > tickInterval then
          startTicking(tickInterval.ms)
        else
          stopTicking()

        logger.trace(s"⏰ scheduleNext: sleep ${delay}ms until ${milliToString(firstMilli)
          } for ${alarms.mkString(", ")}${
          (epochMilliToAlarms.size > 1) ?? s" (${epochMilliToAlarms.size} alarm times)"}")
        nextMilli = firstMilli
        scheduleGeneration += 1
        val generation = scheduleGeneration
        val cancelTimer =
          toMs(delay).match
            case Failure(t) =>
              logger.error(s"scheduleNext: delay=${delay}ms is out of range for FiniteDuration: ${
                alarms.mkString(", ")}")
              emptyRunnable
            case Success(delay) =>
              // May synchronously call run() when the alarm is due,
              // which calls scheduleNext() recursively and sets a newer timer
              sleepUntil(firstMilli, delay, clockChecking /*=self.run()*/)

        if generation == scheduleGeneration then
          timer := cancelTimer
          logger.trace(s"⏰ scheduleNext: timer set until ${milliToString(firstMilli)}")
        else
          // A recursive scheduleNext() has set a newer timer. Don't replace it!
          cancelTimer.run()
          logger.trace(s"⏰ scheduleNext: timer until ${milliToString(firstMilli)
            } is outdated, keeping the newer timer until ${milliToString(nextMilli)}")

  /** Sleep until `epochMilli`, which is `delay` from now.
   * <p>
   * TestAlarmClock overrides this to be atomic with respect to its `tick`. */
  protected def sleepUntil(epochMilli: Long, delay: FiniteDuration, runnable: Runnable)
  : Runnable =
    scheduler.sleep(delay, runnable)

  private def toMs(millis: Long): Try[FiniteDuration] =
    Try((millis max 0).ms)

  private def startTicking(tickInterval: FiniteDuration) =
    if !ticking && !stopped then
      logger.trace(s"⏰ Start ticking every ${tickInterval.pretty}")
      ticking = true
      ticker := scheduler.scheduleAtFixedRate(tickInterval, tickInterval):
        logger.trace("⏰ Tick")
        run()

  private def stopTicking() =
    if ticking then
      logger.trace("⏰ Stop ticking")
      ticking = false
      ticker := SyncCancelable.empty

  /** Runnable run to be used for Cats Effect's Scheduler#sleep. */
  final def run(): Unit =
    // holdsLock=true means re-entrant call, for example by Scheduler#sleep in scheduleNext,
    // which may synchronously run an expired timer
    logger.trace(s"⏰ run nextMilli=${milliToString(nextMilli)} stopped=$stopped holdsLock=${
      Thread.holdsLock(clockChecking)}")
    if !stopped then
      if nextMilli <= epochMilli() then
        var alarms = Vector.empty[Alarm]
        clockChecking.synchronized:
          while epochMilliToAlarms.nonEmpty && epochMilliToAlarms.firstKey <= epochMilli() do
            alarms ++= epochMilliToAlarms.remove(epochMilliToAlarms.firstKey).get
        logger.trace(s"⏰ run: ${alarms.size} alarms due")
        for a <- alarms do a.call()

      clockChecking.synchronized:
        scheduleNext()

  override def toString =
    productPrefix + "(" + now() + ", " +
      epochMilliToAlarms.headOption.fold("no alarm")(o =>
        "alarms=" +
          Timestamp.ofEpochMilli(o._1).toString +
          ((epochMilliToAlarms.size > 1) ?? s", ${epochMilliToAlarms.size} alarms")) +
      (ticking ?? s", ticking ${clockCheckInterval.pretty}") +
      ")"


  private final class Alarm(id: Long, epochMilli: Long, label: => String, callback: => Unit)
  extends Runnable:
    private val called = Atomic(false)

    def call(): Unit =
      if !called.getAndSet(true) then
        logger.trace(s"🔔 $toString")
        try
          callback
        catch
          // When not caught, the Cats Effect working thread will die and not be restarted.
          // After some exceptions, all working threads have died and the program no longer reacts.
          case t: IllegalStateException if t.getMessage == "Dispatcher already closed" =>
            logger.warn(s"$toString: ${t.toStringWithCauses}")
          case NonFatal(t) =>
            logger.error(s"$toString: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
      else
        logger.trace(s"🔔 $toString: already called")

    def run(): Unit = cancel()

    def cancel(): Unit =
      clockChecking.synchronized:
        epochMilliToAlarms.get(epochMilli) match
          case None =>
            logger.trace(s"⏰ cancel $toString: not scheduled (called=${called.get()})")
          case Some(alarms) =>
            logger.trace(s"⏰ cancel $toString")
            val remaining = alarms.filterNot(_ eq this)
            if remaining.nonEmpty then
              epochMilliToAlarms.update(epochMilli, remaining)
            else
              epochMilliToAlarms -= epochMilli
              scheduleNext()

    override def toString =
      s"Alarm#$id(${Timestamp.ofEpochMilli(epochMilli)} $label)"


object ClockChecking:
  private val logger = Logger[this.type]

  private def milliToString(milli: Long): String =
    if milli == Long.MaxValue then "never" else Timestamp.ofEpochMilli(milli).toString
