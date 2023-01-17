package js7.data_for_java.subagent

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.item.ItemRevision
import js7.data.subagent.{SubagentId, SubagentSelection, SubagentSelectionId}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

final case class JSubagentSelection(asScala: SubagentSelection)
extends JJsonable[JSubagentSelection] with JUnsignedSimpleItem
{
  type AsScala = SubagentSelection
  protected def companion = JSubagentSelection

  @Nonnull
  def path: SubagentSelectionId =
    id

  @Nonnull
  def id: SubagentSelectionId =
    asScala.id

  @Nonnull
  def subagentToPriority: java.util.Map[SubagentId, Int] =
    asScala.subagentToPriority.asJava

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JSubagentSelection =
    copy(asScala.withRevision(revision.toScala))
}

object JSubagentSelection extends JJsonable.Companion[JSubagentSelection]
{
  def of(
    id: SubagentSelectionId,
    subagentToPriority: java.util.Map[SubagentId, java.lang.Integer])
  : JSubagentSelection =
    JSubagentSelection(
      SubagentSelection(
        id,
        subagentToPriority.asScala.view.mapValues(_.toInt).toMap))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JSubagentSelection] =
    super.fromJson(jsonString)

  protected def jsonEncoder = SubagentSelection.jsonCodec
  protected def jsonDecoder = SubagentSelection.jsonCodec
}
