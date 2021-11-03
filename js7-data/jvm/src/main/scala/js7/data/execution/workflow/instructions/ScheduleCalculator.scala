package js7.data.execution.workflow.instructions

import java.time.DayOfWeek.MONDAY
import java.time.ZoneOffset.UTC
import java.time.{LocalDateTime, ZoneId}
import js7.base.problem.Checked
import js7.base.time.AdmissionTimeSchemeForJavaTime._
import js7.base.time.JavaTimestamp.specific._
import js7.base.time.{JavaTimestamp, TimeInterval, Timestamp}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.order.CycleState
import js7.data.workflow.instructions.Schedule
import js7.data.workflow.instructions.Schedule.{Continuous, Periodic, Ticking}
import scala.concurrent.duration._

final class ScheduleCalculator(schedule: Schedule, zone: ZoneId, dateOffset: FiniteDuration)
extends ScheduleSimulator
{
  def nextCycleState(now: Timestamp, cycleState: CycleState): Option[CycleState] =
    for ((schemeIndex, next) <- nextCycle(now, cycleState)) yield
      cycleState.copy(
        schemeIndex = schemeIndex,
        index =
          if (schemeIndex != cycleState.schemeIndex)
            1
          else
            cycleState.index + 1,
        next = next)

  /**
   * If it is to late for next in cycleState, then calculate a new CycleState.
   * @return Right(None) iff `cycleState` is still valid
   *         Right(Some(None)) iff `Cycle` has been finished
   */
  def maybeRecalcCycleState(now: Timestamp, cycleState: CycleState)
  : Checked[Option[Option[CycleState]]] =
    for (scheme <- schedule.schemes.checked(cycleState.schemeIndex)) yield
      !scheme.admissionTimeScheme.isPermitted(now max cycleState.next, zone, dateOffset) ?
        nextCycleState(now, cycleState)

  /** Returns schemeIndex and Timestamp. */
  private def nextCycle(now: Timestamp, cycleState: CycleState) =
    schedule.schemes.view.zipWithIndex
      // For each Scheme
      .flatMap { case (scheme, schemeIndex) =>
        for (interval <- scheme.admissionTimeScheme.findTimeInterval(now, zone, dateOffset))
          yield (interval, scheme.repeat, schemeIndex)
      }
      // For each current or next TimeInterval in Schemes
      .flatMap { case (interval, repeat, schemeIndex) =>
        val lastScheduledCycleStart = cycleState.next max interval.start
        val end = cycleState.end min interval.end
        val first = schemeIndex != cycleState.schemeIndex
        repeat
          .match_ {
            case periodic: Periodic =>
              nextPeriod(periodic, lastScheduledCycleStart, now, first = first, end)

            case Ticking(interval) =>
              Some(
                if (first)
                  lastScheduledCycleStart
                else if (now > lastScheduledCycleStart + interval) {
                  // Late, return the last scheduled time before now
                  val n = (now.toEpochMilli - lastScheduledCycleStart.toEpochMilli) / interval.toMillis
                  lastScheduledCycleStart + n * interval
                } else
                  lastScheduledCycleStart + interval)

            case Continuous(pause, limit) =>
              val index = if (first) 0 else cycleState.index
              limit.forall(index < _) ? {
                val next = now.max(interval.start) + pause * (!first).toInt
                if (next <= now) Timestamp.Epoch else next
              }
          }
          .filter(_ < end)
          .map(schemeIndex -> _)
      }
      // Select earliest TimeInterval
      .minByOption { case (_, next) => next }

  private def nextPeriod(periodic: Periodic,
    last: Timestamp, now: Timestamp, first: Boolean, end: Timestamp)
  : Option[Timestamp] = {
    import periodic.{offsets, period}
    val scheduleMillis = offsets.view.map(_.toMillis)
    val p = period.toMillis
    val localMilli = last.toZonedDateTime(zone).toLocalDateTime.toInstant(UTC).toEpochMilli
    val localPeriodStart = localMilli / p * p
    val localMilliOfPeriod = localMilli % p

    // Slow??
    val nextTimestamps = Iterator.from(0)
      .flatMap(i => scheduleMillis.map(_ + i * p))
      .filter(t =>
        if (first)
          t >= localMilliOfPeriod
        else
          t > localMilliOfPeriod)
      .map { nextMilli =>
        val localEpochMilli = localPeriodStart + nextMilli
        val localDateTime = LocalDateTime.ofEpochSecond(
          localEpochMilli / 1000,
          (localEpochMilli % 1000).toInt,
          UTC)
        JavaTimestamp.ofInstant(localDateTime.atZone(zone).toInstant)
      }
      .takeWhile(_ < end)
      .toVector

    // If late, select the latest time before now (and don't catch up the earlier lost times),
    // other select the earliest time (after now)
    nextTimestamps.view.filter(_ <= now).maxOption
      .orElse(nextTimestamps.minOption)
  }

  def findTimeInterval(schemeIndex: Int, now: Timestamp): Checked[Option[TimeInterval]] =
    for (scheme <- schedule.schemes.checked(schemeIndex)) yield
      scheme.admissionTimeScheme
        .findTimeInterval(now, zone, dateOffset)
}

object ScheduleCalculator
{
  private val monday1 = LocalDateTime.parse("2021-11-01T00:00")
  assert(monday1.getDayOfWeek == MONDAY)

  def apply(schedule: Schedule, zone: ZoneId, dateOffset: FiniteDuration) =
    checked(schedule, zone, dateOffset).orThrow

  private[instructions] def checked(
    schedule: Schedule,
    zone: ZoneId,
    dateOffset: FiniteDuration)
  : Checked[ScheduleCalculator] =
    Right(new ScheduleCalculator(schedule, zone, dateOffset))
}
