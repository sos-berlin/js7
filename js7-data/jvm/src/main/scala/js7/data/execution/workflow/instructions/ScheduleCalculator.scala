package js7.data.execution.workflow.instructions

import java.time.ZoneOffset.UTC
import java.time.{LocalDateTime, ZoneId}
import js7.base.problem.Checked
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.{JavaTimestamp, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.CycleState
import js7.data.workflow.instructions.Schedule
import js7.data.workflow.instructions.Schedule.{Continuous, Periodic, Ticking}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

final class ScheduleCalculator private(
  schedule: Schedule, zone: ZoneId, dateOffset: FiniteDuration, onlyOnePeriod: Boolean)
extends ScheduleSimulator:

  /** Call this when cycling starts and after a cycle has finished.
    * @return The next CycleState or None when cycling ends
    */
  def nextCycleState(cycleState: CycleState, now: Timestamp): Option[CycleState] =
    nextCycle(now, cycleState).flatMap: (schemeIndex, periodIndex, next) =>
      val periodChanges = !cycleState.isInitial
        && (schemeIndex != cycleState.schemeIndex || periodIndex != cycleState.periodIndex)
      !(onlyOnePeriod && periodChanges) ?
        cycleState.copy(
          schemeIndex = schemeIndex,
          periodIndex = periodIndex,
          index = if periodChanges then 1 else cycleState.index + 1,
          next = next)

  /**
   * If it is to late for next in cycleState, then calculate a new CycleState.
   * @return Right(None) iff `cycleState` is still valid
   *         Right(Some(None)) iff `Cycle` has been finished
   */
  def maybeRecalcCycleState(now: Timestamp, cycleState: CycleState)
  : Checked[Option[Option[CycleState]]] =
    for scheme <- schedule.schemes.checked(cycleState.schemeIndex) yield
      !scheme.admissionTimeScheme.isPermitted(now.max(cycleState.next), zone, dateOffset) ?
        nextCycleState(cycleState, now)

  /** Returns schemeIndex and Timestamp. */
  private def nextCycle(now: Timestamp, cycleState: CycleState): Option[(Int, Int, Timestamp)] =
    schedule.schemes.view.zipWithIndex
      .flatMap: (scheme, schemeIndex) =>
        scheme.admissionTimeScheme
          .findTimeIntervals(now, until = cycleState.end, zone, dateOffset)
          // For each current or next (periodIndex, TimeInterval) in Schemes
          .flatMap: (periodIndex, interval) =>
            val lastScheduledCycleStart = cycleState.next max interval.start
            val end = cycleState.end min interval.end
            val isFirst = schemeIndex != cycleState.schemeIndex
              || periodIndex != cycleState.periodIndex
            scheme.repeat.match
              case periodic: Periodic =>
                nextPeriod(periodic, lastScheduledCycleStart, now, isFirst = isFirst, end)

              case Ticking(tickDuration) =>
                Some:
                  val ticks = (now - lastScheduledCycleStart).toMillis / tickDuration.toMillis
                  if ticks > 0 then // Late?
                    lastScheduledCycleStart + ticks * tickDuration
                  else
                    lastScheduledCycleStart + tickDuration * (!isFirst).toInt

              case Continuous(pause, limit) =>
                val index = if isFirst then 0 else cycleState.index
                limit.forall(index < _).thenSome:
                  val next = (now max interval.start) + pause * (!isFirst).toInt
                  if next <= now then Timestamp.Epoch else next
            .filter(_ < end)
            .map: next =>
              (schemeIndex, periodIndex, next)
      // Select earliest TimeInterval
      .minByOption((_, _, next) => next)

  private def nextPeriod(periodic: Periodic,
    last: Timestamp, now: Timestamp, isFirst: Boolean, end: Timestamp)
  : Option[Timestamp] =
    import periodic.{offsets, period}
    val scheduleMillis = offsets.view.map(_.toMillis)
    val p = period.toMillis
    val localMilli = last.toZonedDateTime(zone).toLocalDateTime.toInstant(UTC).toEpochMilli
    val localPeriodStart = localMilli / p * p
    val localMilliOfPeriod = localMilli % p

    // Slow???
    val nextTimestamps = Iterator.from(0)
      .flatMap(i => scheduleMillis.map(_ + i * p))
      .filter: t =>
        if isFirst then
          t >= localMilliOfPeriod
        else
          t > localMilliOfPeriod
      .map: nextMilli =>
        val localEpochMilli = localPeriodStart + nextMilli
        val localDateTime = LocalDateTime.ofEpochSecond(
          localEpochMilli / 1000,
          (localEpochMilli % 1000).toInt,
          UTC)
        JavaTimestamp.ofInstant(localDateTime.atZone(zone).toInstant)
      .takeWhile(_ < end)
      .toVector

    // If late, select the latest time before now (and don't catch up the earlier lost times),
    // other select the earliest time (after now)
    nextTimestamps.view.filter(_ <= now).maxOption.orElse(nextTimestamps.minOption)


object ScheduleCalculator:

  @TestOnly
  def apply(schedule: Schedule, zone: ZoneId, dateOffset: FiniteDuration,
    onlyOnePeriod: Boolean = false)
  : ScheduleCalculator =
    checked(schedule, zone, dateOffset, onlyOnePeriod).orThrow

  def checked(
    schedule: Schedule,
    zone: ZoneId,
    dateOffset: FiniteDuration,
    onlyOnePeriod: Boolean = false)
  : Checked[ScheduleCalculator] =
    Right(new ScheduleCalculator(schedule, zone, dateOffset, onlyOnePeriod))
