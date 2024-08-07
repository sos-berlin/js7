package js7.data_for_java.subagent

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.item.ItemRevision
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import scala.jdk.OptionConverters.*

final case class JSubagentItem(asScala: SubagentItem)
extends JJsonable[JSubagentItem], JUnsignedSimpleItem:

  type AsScala = SubagentItem
  protected def companion = JSubagentItem

  @Nonnull
  def path: SubagentId =
    id

  @Nonnull
  def id: SubagentId =
    asScala.id

  @Nonnull
  def agentPath: AgentPath =
    asScala.agentPath

  @Nonnull
  def uri: Uri =
    asScala.uri

  def disabled: Boolean =
    asScala.disabled

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JUnsignedSimpleItem =
    copy(asScala.withRevision(revision.toScala))


object JSubagentItem extends JJsonable.Companion[JSubagentItem]:
  type AsScala = SubagentItem

  @Nonnull
  def of(
    @Nonnull id: SubagentId,
    @Nonnull agentPath: AgentPath,
    @Nonnull uri: Uri,
    disabled: Boolean)
  : JSubagentItem =
    JSubagentItem(SubagentItem(id, agentPath, uri, disabled = disabled))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JSubagentItem] =
    super.fromJson(jsonString)

  protected def jsonEncoder = SubagentItem.jsonCodec
  protected def jsonDecoder = SubagentItem.jsonCodec
