package js7.data.item

import io.circe.Decoder
import io.circe.generic.semiauto.deriveEncoder
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.DelegateId
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

  sealed trait ItemAttachedStateEvent
  extends ForController {
    def delegateId: DelegateId
    def attachedState: ItemAttachedState
  }
  object ItemAttachedStateEvent {
    def apply(key: InventoryItemKey, delegateId: DelegateId, attachedState: ItemAttachedState)
    : ItemAttachedStateEvent =
      attachedState match {
        case Attachable => ItemAttachable(key, delegateId)
        case Attached(itemRevision) => ItemAttached(key, itemRevision, delegateId)
        case Detachable => ItemDetachable(key, delegateId)
        case Detached => ItemDetached(key, delegateId)
      }
    def unapply(event: ItemAttachedStateEvent) =
      Some((event.key, event.delegateId, event.attachedState))
  }

  final case class ItemAttachable(key: InventoryItemKey, delegateId: DelegateId)
  extends ItemAttachedStateEvent {
    def attachedState = Attachable
  }
  object ItemAttachable {
    // COMPATIBLE with version 2.1
    implicit def jsonDecoder[S](implicit S: ItemContainer.Companion[S]): Decoder[ItemAttachable] = {
      import S.inventoryItemKeyJsonCodec
      c => for {
        key <- c.get[InventoryItemKey]("key")
        delegateId <- S.decodeDelegateIdOrAgentPath(c)
      } yield ItemAttachable(key, delegateId)
    }
  }

  final case class ItemAttached(
    key: InventoryItemKey,
    itemRevision: Option[ItemRevision],
    delegateId: DelegateId)
  extends ItemAttachedStateEvent {
    def attachedState = Attached(itemRevision)
  }
  object ItemAttached {
    // COMPATIBLE with version 2.1
    implicit def jsonDecoder[S](implicit S: ItemContainer.Companion[S]): Decoder[ItemAttached] = {
      import S.inventoryItemKeyJsonCodec
      c => for {
        key <- c.get[InventoryItemKey]("key")
        rev <- c.get[Option[ItemRevision]]("itemRevision")
        delegateId <- S.decodeDelegateIdOrAgentPath(c)
      } yield ItemAttached(key, rev, delegateId)
    }
  }

  /** Agent only. */
  final case class ItemAttachedToAgent(item: InventoryItem)
  extends ForAgent {
    def key = item.key
  }

  final case class ItemDetachable(key: InventoryItemKey, delegateId: DelegateId)
  extends ItemAttachedStateEvent {
    def attachedState = Detachable
  }
  object ItemDetachable {
    // COMPATIBLE with version 2.1
    implicit def jsonDecoder[S](implicit S: ItemContainer.Companion[S]): Decoder[ItemDetachable] = {
      import S.inventoryItemKeyJsonCodec
      c => for {
        key <- c.get[InventoryItemKey]("key")
        delegateId <- S.decodeDelegateIdOrAgentPath(c)
      } yield ItemDetachable(key, delegateId)
    }
  }

  final case class ItemDetached(key: InventoryItemKey, delegateId: DelegateId)
  extends ItemAttachedStateEvent with ForAgent {
    def attachedState = Detached
  }
  object ItemDetached {
    // COMPATIBLE with version 2.1
    implicit def jsonDecoder[S](implicit S: ItemContainer.Companion[S]): Decoder[ItemDetached] = {
      import S.inventoryItemKeyJsonCodec
      c => for {
        key <- c.get[InventoryItemKey]("key")
        delegateId <- S.decodeDelegateIdOrAgentPath(c)
      } yield ItemDetached(key, delegateId)
    }
  }

  def jsonCodec[S](implicit S: ItemContainer.Companion[S]): TypedJsonCodec[BasicItemEvent] = {
    implicit val x = S.inventoryItemJsonCodec
    implicit val y = S.inventoryItemKeyJsonCodec
    implicit val z = S.delegateIdJsonCodec

    TypedJsonCodec(
      Subtype(deriveCodec[ItemDeletionMarked]),
      Subtype(deriveCodec[ItemDeleted]),
      Subtype(deriveEncoder[ItemAttachable], ItemAttachable.jsonDecoder),
      Subtype(deriveEncoder[ItemAttached], ItemAttached.jsonDecoder),
      Subtype(deriveCodec[ItemAttachedToAgent]),
      Subtype(deriveEncoder[ItemDetachable], ItemDetachable.jsonDecoder),
      Subtype(deriveEncoder[ItemDetached], ItemDetached.jsonDecoder))
  }
}
