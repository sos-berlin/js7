package js7.data_for_java.subagent

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.subagent.{SubagentId, SubagentSelection, SubagentSelectionId}
import js7.data_for_java.common.JJsonable
import scala.jdk.CollectionConverters._

final case class JSubagentSelection(asScala: SubagentSelection)
extends JJsonable[JSubagentSelection]
{
  protected type AsScala = SubagentSelection
  protected def companion = JSubagentSelection

  @Nonnull
  def id: SubagentSelectionId =
    asScala.id

  @Nonnull
  def subagentToPriority: java.util.Map[SubagentId, Int] =
    asScala.subagentToPriority.asJava
}

object JSubagentSelection extends JJsonable.Companion[JSubagentSelection]
{
  def of(id: SubagentSelectionId, subagentToPriority: java.util.Map[SubagentId, Int]) =
    JSubagentSelection(
      SubagentSelection(id, subagentToPriority.asScala.toMap))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JSubagentSelection] =
    super.fromJson(jsonString)

  protected def jsonEncoder = SubagentSelection.jsonCodec
  protected def jsonDecoder = SubagentSelection.jsonCodec
}
