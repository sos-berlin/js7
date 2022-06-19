package js7.data.subagent

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.item.{ItemRevision, UnsignedSimpleItem}

final case class SubagentSelection(
  id: SubagentSelectionId,
  subagentToPriority: Map[SubagentId, Int],
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = SubagentSelection
  val companion = SubagentSelection

  def path = id

  def rename(id: SubagentSelectionId) =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def toInitialItemState = SubagentSelectionState(this)

  //override def dedicatedAgentPath = AgentPath cannot be determined here ???

  override def referencedItemPaths =
    subagentToPriority.keySet.view
}

object SubagentSelection extends UnsignedSimpleItem.Companion[SubagentSelection]
{
  override type Path = SubagentSelectionId
  override val Path = SubagentSelectionId

  type Key = Path
  val Key = Path

  type ItemState = SubagentSelectionState

  val cls = classOf[SubagentSelection]

  implicit val jsonCodec: Codec.AsObject[SubagentSelection] =
    deriveCodec[SubagentSelection]
}
