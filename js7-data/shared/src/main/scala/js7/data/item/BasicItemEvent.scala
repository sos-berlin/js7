package js7.data.item

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentPath
import js7.data.event.ItemContainer
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached}

sealed trait BasicItemEvent extends InventoryItemEvent

object BasicItemEvent
{
  sealed trait ForController extends BasicItemEvent
  sealed trait ForAgent extends BasicItemEvent

  /** Used for OrderWatch to allow to attach it from Agent. */
  final case class ItemDeletionMarked(key: InventoryItemKey)
  extends ForController {
    def attachedState = None
  }

  final case class ItemDeleted(key: InventoryItemKey)
  extends ForController

  sealed trait ItemAttachedStateChanged
  extends ForController {
    def agentPath: AgentPath
    def attachedState: ItemAttachedState
  }
  object ItemAttachedStateChanged {
    def apply(key: InventoryItemKey, agentPath: AgentPath, attachedState: ItemAttachedState)
    : ItemAttachedStateChanged =
      attachedState match {
        case Attachable => ItemAttachable(key, agentPath)
        case Attached(itemRevision) => ItemAttached(key, itemRevision, agentPath)
        case Detachable => ItemDetachable(key, agentPath)
        case Detached => ItemDetached(key, agentPath)
      }
    def unapply(event: ItemAttachedStateChanged) =
      Some((event.key, event.agentPath, event.attachedState))
  }

  final case class ItemAttachable(key: InventoryItemKey, agentPath: AgentPath)
  extends ItemAttachedStateChanged {
    def attachedState = Attachable
  }

  final case class ItemAttached(key: InventoryItemKey, itemRevision: Option[ItemRevision], agentPath: AgentPath)
  extends ItemAttachedStateChanged {
    def attachedState = Attached(itemRevision)
  }

  /** Agent only. */
  final case class ItemAttachedToAgent(item: InventoryItem)
  extends ForAgent {
    def key = item.key
  }

  final case class ItemDetachable(key: InventoryItemKey, agentPath: AgentPath)
  extends ItemAttachedStateChanged {
    def attachedState = Detachable
  }

  final case class ItemDetached(key: InventoryItemKey, agentPath: AgentPath)
  extends ItemAttachedStateChanged with ForAgent {
    def attachedState = Detached
  }

  def jsonCodec[S](implicit S: ItemContainer.Companion[S])
  : TypedJsonCodec[BasicItemEvent] = {
    implicit val x = S.inventoryItemJsonCodec
    implicit val y = S.inventoryItemKeyJsonCodec

    TypedJsonCodec(
      Subtype(deriveCodec[ItemDeletionMarked]),
      Subtype(deriveCodec[ItemDeleted]),
      Subtype(deriveCodec[ItemAttachable]),
      Subtype(deriveCodec[ItemAttached]),
      Subtype(deriveCodec[ItemAttachedToAgent]),
      Subtype(deriveCodec[ItemDetachable]),
      Subtype(deriveCodec[ItemDetached]))
  }
}
