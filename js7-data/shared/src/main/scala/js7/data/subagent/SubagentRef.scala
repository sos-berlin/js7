package js7.data.subagent

import io.circe.Codec
import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.item.{ItemRevision, UnsignedSimpleItem}
import scala.collection.View

final case class SubagentRef(
  id: SubagentId,
  agentPath: AgentPath,
  uri: Uri,
  // Higher number means higher priority
  priority: Option[Int] = None,
  disabled: Boolean = false,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = SubagentRef

  val companion = SubagentRef

  def rename(id: SubagentId) =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def path = id

  override def dedicatedAgentPath = Some(agentPath)

  // Circular dependency! AgentRef references subagentId: Seq[SubagentId]
  override def referencedItemPaths = new View.Single(agentPath)
}

object SubagentRef
extends UnsignedSimpleItem.Companion[SubagentRef]
{
  type Key = SubagentId
  val Key = SubagentId

  type Path = SubagentId
  val Path = SubagentId

  val cls = classOf[SubagentRef]

  implicit val jsonCodec: Codec.AsObject[SubagentRef] = {
    implicit val x = withDefaults
    deriveConfiguredCodec[SubagentRef]
  }
}
