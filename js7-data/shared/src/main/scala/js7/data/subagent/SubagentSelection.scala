package js7.data.subagent

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.item.{InventoryItemPath, ItemRevision, UnsignedItemPath, UnsignedSimpleItem}
import scala.collection.View

final case class SubagentSelection(
  id: SubagentSelectionId,
  subagentToPriority: Map[SubagentId, Int],
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:
  protected type Self = SubagentSelection
  val companion: SubagentSelection.type = SubagentSelection

  def path: SubagentSelectionId = id

  def rename(id: SubagentSelectionId): SubagentSelection =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]): SubagentSelection =
    copy(itemRevision = revision)

  def toInitialItemState: SubagentSelectionState =
    SubagentSelectionState(this)

  //override def dedicatedAgentPath = AgentPath cannot be determined here ???

  override def referencedItemPaths: View[InventoryItemPath] =
    subagentIds

  def subagentIds: View[SubagentId] =
    subagentToPriority.keySet.view


object SubagentSelection extends UnsignedSimpleItem.Companion[SubagentSelection]:
  type Key = SubagentSelectionId
  def Key: SubagentSelectionId.type = SubagentSelectionId

  override type Path = Key
  val Path: UnsignedItemPath.Companion[SubagentSelectionId] = Key

  type ItemState = SubagentSelectionState

  val cls: Class[SubagentSelection] = classOf[SubagentSelection]

  implicit val jsonCodec: Codec.AsObject[SubagentSelection] =
    deriveCodec[SubagentSelection]
