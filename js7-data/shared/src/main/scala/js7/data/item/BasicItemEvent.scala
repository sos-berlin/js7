package js7.data.item

import io.circe.generic.semiauto.{deriveCodec, deriveEncoder}
import io.circe.{Codec, Decoder, HCursor}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.delegate.DelegateId
import js7.data.event.ItemContainer
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached}

sealed trait BasicItemEvent extends InventoryItemEvent

object BasicItemEvent:
  sealed trait ForClient extends BasicItemEvent
  sealed trait ForDelegate extends BasicItemEvent

  /** Used for OrderWatch to allow to attach it from Agent. */
  final case class ItemDeletionMarked(key: InventoryItemKey)
  extends ForClient:
    def attachedState: None.type = None


  final case class ItemDeleted(key: InventoryItemKey)
  extends ForClient


  sealed trait ItemAttachedStateEvent
  extends ForClient:
    def delegateId: DelegateId
    def attachedState: ItemAttachedState

  object ItemAttachedStateEvent:
    def apply(key: InventoryItemKey, delegateId: DelegateId, attachedState: ItemAttachedState)
    : ItemAttachedStateEvent =
      attachedState match
        case Attachable => ItemAttachable(key, delegateId)
        case Attached(itemRevision) => ItemAttached(key, itemRevision, delegateId)
        case Detachable => ItemDetachable(key, delegateId)
        case Detached => ItemDetached(key, delegateId)
    def unapply(event: ItemAttachedStateEvent)
    : Some[(InventoryItemKey, DelegateId, ItemAttachedState)] =
      Some((event.key, event.delegateId, event.attachedState))


  final case class ItemAttachable(key: InventoryItemKey, delegateId: DelegateId)
  extends ItemAttachedStateEvent:
    def attachedState: ItemAttachedState = Attachable

  object ItemAttachable:
    // COMPATIBLE with version 2.1
    implicit def jsonDecoder[S](implicit S: ItemContainer.Companion[S]): Decoder[ItemAttachable] =
      import S.inventoryItemKeyJsonCodec
      c => for
        key <- c.get[InventoryItemKey]("key")
        delegateId <- S.decodeDelegateIdOrAgentPath(c)
      yield ItemAttachable(key, delegateId)


  final case class ItemAttached(
    key: InventoryItemKey,
    override val itemRevision: Option[ItemRevision],
    delegateId: DelegateId)
  extends ItemAttachedStateEvent:
    def attachedState: ItemAttachedState = Attached(itemRevision)

  object ItemAttached:
    // COMPATIBLE with version 2.1
    implicit def jsonDecoder[S](implicit S: ItemContainer.Companion[S]): Decoder[ItemAttached] =
      import S.inventoryItemKeyJsonCodec
      c => for
        key <- c.get[InventoryItemKey]("key")
        rev <- c.get[Option[ItemRevision]]("itemRevision")
        delegateId <- S.decodeDelegateIdOrAgentPath(c)
      yield ItemAttached(key, rev, delegateId)


  /** Agent only. */
  final case class ItemAttachedToMe(item: InventoryItem)
  extends ForDelegate:
    override final def itemRevision =
      item.itemRevision

    def key: InventoryItemKey = item.key

  /** Agent and Subagent only. */
  final case class SignedItemAttachedToMe(signed: Signed[SignableItem])
  extends ForDelegate:
    def item: SignableItem = signed.value
    def key: SignableItemKey = item.key

  object SignedItemAttachedToMe:
    def jsonCodec[S: ItemContainer.Companion]: Codec.AsObject[SignedItemAttachedToMe] =
      new Codec.AsObject[SignedItemAttachedToMe]:
        def encodeObject(o: SignedItemAttachedToMe) =
          SignableItem.signedEncodeJson(o.signed.signedString, o.signed.value.itemRevision)

        def apply(c: HCursor) =
         SignableItem.signedJsonDecoder.decodeJson(c.value).map(SignedItemAttachedToMe(_))

  final case class ItemDetachable(key: InventoryItemKey, delegateId: DelegateId)
  extends ItemAttachedStateEvent:
    def attachedState: ItemAttachedState = Detachable

  object ItemDetachable:
    // COMPATIBLE with version 2.1
    implicit def jsonDecoder[S](implicit S: ItemContainer.Companion[S]): Decoder[ItemDetachable] =
      import S.inventoryItemKeyJsonCodec
      c => for
        key <- c.get[InventoryItemKey]("key")
        delegateId <- S.decodeDelegateIdOrAgentPath(c)
      yield ItemDetachable(key, delegateId)


  final case class ItemDetachingFromMe(key: InventoryItemKey)
  extends ForDelegate:
    def attachedState: ItemAttachedState.Detached.type = Detached

  object ItemDetachingFromMe:
    def jsonCodec[S](implicit S: ItemContainer.Companion[S])
    : Codec.AsObject[ItemDetachingFromMe] =
      import S.{delegateIdJsonCodec, inventoryItemKeyJsonCodec}
      intelliJuseImport((inventoryItemKeyJsonCodec, delegateIdJsonCodec))
      deriveCodec[ItemDetachingFromMe]


  final case class ItemDetached(key: InventoryItemKey, delegateId: DelegateId)
  extends ItemAttachedStateEvent, ForDelegate:
    def attachedState: ItemAttachedState = Detached

  object ItemDetached:
    // COMPATIBLE with version 2.1
    implicit def jsonDecoder[S](implicit S: ItemContainer.Companion[S]): Decoder[ItemDetached] =
      import S.inventoryItemKeyJsonCodec
      c => for
        key <- c.get[InventoryItemKey]("key")
        delegateId <- S.decodeDelegateIdOrAgentPath(c)
      yield ItemDetached(key, delegateId)

  def jsonCodec[S](implicit S: ItemContainer.Companion[S]): TypedJsonCodec[BasicItemEvent] =
    import S.inventoryItemJsonCodec
    import S.inventoryItemKeyJsonCodec
    import S.delegateIdJsonCodec
    import S.signableItemJsonCodec
    intelliJuseImport((
      inventoryItemJsonCodec,
      inventoryItemKeyJsonCodec,
      delegateIdJsonCodec,
      signableItemJsonCodec))

    TypedJsonCodec(
      Subtype(deriveCodec[ItemDeletionMarked]),
      Subtype(deriveCodec[ItemDeleted]),
      Subtype(deriveEncoder[ItemAttachable], ItemAttachable.jsonDecoder),
      Subtype(deriveEncoder[ItemAttached], ItemAttached.jsonDecoder),
      Subtype(deriveCodec[ItemAttachedToMe], aliases = "ItemAttachedToAgent" :: Nil),
      Subtype(SignedItemAttachedToMe.jsonCodec),
      Subtype(deriveEncoder[ItemDetachable], ItemDetachable.jsonDecoder),
      Subtype(ItemDetachingFromMe.jsonCodec),
      Subtype(deriveEncoder[ItemDetached], ItemDetached.jsonDecoder))
