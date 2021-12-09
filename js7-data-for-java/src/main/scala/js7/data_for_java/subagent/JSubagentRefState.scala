package js7.data_for_java.subagent

import io.vavr.control.{Either => VEither}
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.subagent.{SubagentId, SubagentRefState, SubagentRunId}
import js7.data_for_java.common.JJsonable
import scala.jdk.OptionConverters._

final case class JSubagentRefState(asScala: SubagentRefState)
extends JJsonable[JSubagentRefState]
{
  protected type AsScala = SubagentRefState
  protected def companion = JSubagentRefState

  @Nonnull
  def subagentId: SubagentId =
    asScala.subagentId

  @Nonnull
  def subagentRef: JSubagentRef =
    JSubagentRef(asScala.subagentRef)

  @Nonnull
  def subagentRunId: Optional[SubagentRunId] =
    asScala.subagentRunId.toJava

  @Nonnull
  def problem: Optional[Problem] =
    asScala.problem.toJava
}

object JSubagentRefState extends JJsonable.Companion[JSubagentRefState]
{
  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JSubagentRefState] =
    super.fromJson(jsonString)

  protected def jsonEncoder = SubagentRefState.jsonCodec
  protected def jsonDecoder = SubagentRefState.jsonCodec
}
