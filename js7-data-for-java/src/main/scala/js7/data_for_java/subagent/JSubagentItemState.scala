package js7.data_for_java.subagent

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.subagent.{SubagentId, SubagentItemState, SubagentRunId}
import js7.data_for_java.common.JJsonable
import scala.jdk.OptionConverters.*

final case class JSubagentItemState(asScala: SubagentItemState)
extends JJsonable[JSubagentItemState]:

  type AsScala = SubagentItemState
  protected def companion = JSubagentItemState

  @Nonnull
  def subagentId: SubagentId =
    asScala.subagentId

  @Nonnull
  def subagentItem: JSubagentItem =
    JSubagentItem(asScala.subagentItem)

  @Nonnull
  def subagentRunId: Optional[SubagentRunId] =
    asScala.subagentRunId.toJava

  @Nonnull
  def problem: Optional[Problem] =
    asScala.problem.toJava

object JSubagentItemState extends JJsonable.Companion[JSubagentItemState]:
  type AsScala = SubagentItemState

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JSubagentItemState] =
    super.fromJson(jsonString)

  protected def jsonEncoder = SubagentItemState.jsonEncoder
  protected def jsonDecoder = SubagentItemState.jsonDecoder
