package js7.data.subagent

import io.circe.Codec
import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import js7.base.circeutils.typed.Subtype
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.item.{ItemRevision, UnsignedSimpleItem}
import scala.collection.View

final case class SubagentItem(
  id: SubagentId,
  agentPath: AgentPath,
  uri: Uri,
  disabled: Boolean = false,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = SubagentItem

  val companion = SubagentItem

  def rename(id: SubagentId) =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def path = id

  override def dedicatedAgentPath = Some(agentPath)

  // Circular dependency! AgentRef references subagentId: Seq[SubagentId]
  override def referencedItemPaths = new View.Single(agentPath)

  // COMPATIBLE with v2.1
  def updateUri(uri: Uri): SubagentItem =
    copy(
      uri = uri,
      itemRevision = Some(
        itemRevision.fold(ItemRevision(1))(_.next)))
}

object SubagentItem
extends UnsignedSimpleItem.Companion[SubagentItem]
{
  type Key = SubagentId
  val Key = SubagentId

  override type Path = SubagentId
  override val Path = SubagentId

  val cls = classOf[SubagentItem]

  implicit val jsonCodec: Codec.AsObject[SubagentItem] = {
    implicit val x = withDefaults
    deriveConfiguredCodec[SubagentItem]
  }

  override val subtype: Subtype[SubagentItem] =
    Subtype[SubagentItem](aliases = Seq("SubagentRef"))
}
