package js7.data.item

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentPath
import js7.data.event.JournaledState
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached}

sealed trait BasicItemEvent extends InventoryItemEvent

object BasicItemEvent
{
  sealed trait ForController extends BasicItemEvent
  sealed trait ForAgent extends BasicItemEvent

  final case class ItemDeletionMarked(id: InventoryItemKey)
  extends ForController {
    def attachedState = None
  }

  final case class ItemDestroyed(id: InventoryItemKey)
  extends ForController

  sealed trait ItemAttachedStateChanged
  extends ForController {
    def agentId: AgentPath
    def attachedState: ItemAttachedState
  }
  object ItemAttachedStateChanged {
    def apply(id: InventoryItemKey, agentId: AgentPath, attachedState: ItemAttachedState)
    : ItemAttachedStateChanged =
      attachedState match {
        case Attachable => ItemAttachable(id, agentId)
        case Attached(itemRevision) => ItemAttached(id, itemRevision, agentId)
        case Detachable => ItemDetachable(id, agentId)
        case Detached => ItemDetached(id, agentId)
      }
    def unapply(event: ItemAttachedStateChanged) =
      Some((event.id, event.agentId, event.attachedState))
  }

  final case class ItemAttachable(id: InventoryItemKey, agentId: AgentPath)
  extends ItemAttachedStateChanged {
    def attachedState = Attachable
  }

  final case class ItemAttached(id: InventoryItemKey, itemRevision: Option[ItemRevision], agentId: AgentPath)
  extends ItemAttachedStateChanged {
    def attachedState = Attached(itemRevision)
  }

  /** Agent only. */
  final case class ItemAttachedToAgent(item: InventoryItem)
  extends ForAgent {
    def id = item.id
  }

  final case class ItemDetachable(id: InventoryItemKey, agentId: AgentPath)
  extends ItemAttachedStateChanged {
    def attachedState = Detachable
  }

  final case class ItemDetached(id: InventoryItemKey, agentId: AgentPath)
  extends ItemAttachedStateChanged with ForAgent {
    def attachedState = Detached
  }

  def jsonCodec[S <: JournaledState[S]](implicit S: JournaledState.Companion[S])
  : TypedJsonCodec[BasicItemEvent] = {
    implicit val x = S.inventoryItemJsonCodec
    implicit val y = S.inventoryItemIdJsonCodec

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
