package js7.data_for_java.schedule

import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.workflow.instructions.Schedule
import js7.data_for_java.common.JJsonable
import js7.data_for_java.vavr.Standards.VEither

final case class JSchedule(asScala: Schedule) extends JJsonable[JSchedule]
{
  protected type AsScala = Schedule
  protected val companion = JSchedule
}

object JSchedule extends JJsonable.Companion[JSchedule]
{
  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JSchedule] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Schedule.jsonCodec
  protected def jsonDecoder = Schedule.jsonCodec
}
