package js7.base.time

import java.time.LocalTime
import js7.base.problem.Problem
import scala.collection.immutable.Map.Map2
import scala.concurrent.duration.FiniteDuration

object Problems:

  final case class PeriodCrossesProductionDayBoundaryProblem(
    period: AdmissionPeriod,
    dateOffset: FiniteDuration)
  extends Problem.Coded:
    def arguments = Map2(
      "period", period.toString,
      "dateOffset", LocalTime.ofNanoOfDay(dateOffset.toNanos).toString)
