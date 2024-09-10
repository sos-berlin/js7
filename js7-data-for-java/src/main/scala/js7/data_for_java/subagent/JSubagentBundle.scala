package js7.data_for_java.subagent

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.item.ItemRevision
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import js7.data_for_java.value.JExpression
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

final case class JSubagentBundle(asScala: SubagentBundle)
extends JJsonable[JSubagentBundle], JUnsignedSimpleItem:

  type AsScala = SubagentBundle
  protected def companion = JSubagentBundle

  @Nonnull
  def path: SubagentBundleId =
    id

  @Nonnull
  def id: SubagentBundleId =
    asScala.id

  @Nonnull
  def subagentToPriority: java.util.Map[SubagentId, JExpression] =
    asScala.subagentToPriority
      .view.mapValues(JExpression(_)).toMap
      .asJava

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JSubagentBundle =
    copy(asScala.withRevision(revision.toScala))


object JSubagentBundle extends JJsonable.Companion[JSubagentBundle]:
  type AsScala = SubagentBundle

  def of(
    id: SubagentBundleId,
    subagentToPriority: java.util.Map[SubagentId, JExpression])
  : JSubagentBundle =
    JSubagentBundle(
      SubagentBundle(
        id,
        subagentToPriority.asScala.view.mapValues(_.asScala).toMap))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JSubagentBundle] =
    super.fromJson(jsonString)

  protected def jsonEncoder = SubagentBundle.jsonCodec
  protected def jsonDecoder = SubagentBundle.jsonCodec
