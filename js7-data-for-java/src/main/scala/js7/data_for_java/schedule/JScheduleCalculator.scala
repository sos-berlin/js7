package js7.data_for_java.schedule

import java.time.{Duration, ZoneId}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.data.execution.workflow.instructions.ScheduleCalculator
import js7.data_for_java.vavr.Standards.VEither
import js7.data_for_java.vavr.VavrConverters.RichVavrOption

final case class JScheduleCalculator(asScala: ScheduleCalculator)
extends JScheduleSimulator


object JScheduleCalculator:

  @Nonnull
  def checked(
    @Nonnull schedule: JSchedule,
    @Nonnull zone: ZoneId,
    @Nonnull dateOffset: Duration)
  : VEither[Problem, JScheduleCalculator] =
    ScheduleCalculator.checked(schedule.asScala, zone, dateOffset.toFiniteDuration)
      .map(JScheduleCalculator(_))
      .toVavr
