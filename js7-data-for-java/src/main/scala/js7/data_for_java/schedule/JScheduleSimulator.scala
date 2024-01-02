package js7.data_for_java.schedule

import java.time.{Duration, Instant}
import java.util.stream.Stream
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.data.execution.workflow.instructions.ScheduleSimulator
import js7.data_for_java.schedule.JScheduleSimulator.*
import scala.jdk.StreamConverters.IterableHasSeqStream

trait JScheduleSimulator:
  this: JScheduleCalculator =>

  def simulate(timeInterval: JTimeInterval, actionDuration: Duration): Stream[Scheduled] =
    asScala
      .simulate(timeInterval.asScala, actionDuration.toFiniteDuration)
      .map(Scheduled(_))
      .asJavaSeqStream


object JScheduleSimulator:
  final case class Scheduled(asScala: ScheduleSimulator.Scheduled):
    /** Instant when Order arrives and starts waiting for the scheduled time. */
    def arrival: Instant =
      asScala.arrival.toInstant

    /** The scheduled time. */
    def start: Instant =
      asScala.cycleState.next.toInstant
