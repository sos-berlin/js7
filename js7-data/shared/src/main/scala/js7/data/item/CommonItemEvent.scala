package js7.data.item

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentId
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached}

sealed trait CommonItemEvent extends InventoryItemEvent

object CommonItemEvent
{
  final case class ItemDeletionMarked(id: InventoryItemId)
  extends CommonItemEvent {
    def attachedState = None
  }

  final case class ItemDestroyed(id: InventoryItemId)
  extends CommonItemEvent

  sealed trait ItemAttachedStateChanged
  extends CommonItemEvent {
    def agentId: AgentId
    def attachedState: ItemAttachedState
  }
  object ItemAttachedStateChanged {
    def unapply(event: ItemAttachedStateChanged) =
      Some((event.id, event.agentId, event.attachedState))
  }

  final case class ItemAttachable(id: InventoryItemId, agentId: AgentId)
  extends ItemAttachedStateChanged {
    def attachedState = Attachable
  }

  final case class ItemAttached(id: InventoryItemId, itemRevision: Option[ItemRevision], agentId: AgentId)
  extends ItemAttachedStateChanged {
    def attachedState = Attached(itemRevision)
  }

  /** Agent only. */
  final case class ItemAttachedToAgent(item: InventoryItem)
  extends CommonItemEvent {
    def id = item.id
  }

  final case class ItemDetachable(id: InventoryItemId, agentId: AgentId)
  extends ItemAttachedStateChanged {
    def attachedState = Detachable
  }

  final case class ItemDetached(id: InventoryItemId, agentId: AgentId)
  extends ItemAttachedStateChanged {
    def attachedState = Detached
  }

  def jsonCodec[A <: SimpleItem](companions: Seq[InventoryItem.Companion])
  : TypedJsonCodec[CommonItemEvent] = {
    implicit val itemJsonCodec = InventoryItem.jsonCodec(companions)
    implicit val idJsonCodec = InventoryItemId.jsonCodec(companions.map(_.Id))

    TypedJsonCodec(
      Subtype(deriveCodec[ItemDeletionMarked]),
      Subtype(deriveCodec[ItemDestroyed]),
      Subtype(deriveCodec[ItemAttachable]),
      Subtype(deriveCodec[ItemAttached]),
      Subtype(deriveCodec[ItemAttachedToAgent]),
      Subtype(deriveCodec[ItemDetachable]),
      Subtype(deriveCodec[ItemDetached]))
  }
}
