package js7.data.event

import io.circe.{Codec, Decoder}
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.agent.AgentPath
import js7.data.delegate.DelegateId
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemKey, InventoryItemPath, SignableItem, SignableItemKey, SignableSimpleItem, SimpleItem, SimpleItemPath, UnsignedItem, UnsignedSimpleItem, UnsignedSimpleItemPath, VersionedControl, VersionedItem, VersionedItemPath}
import scala.collection.MapView

trait ItemContainer:
  def keyToItem: MapView[InventoryItemKey, InventoryItem]

  final lazy val pathToSimpleItem: MapView[SimpleItemPath, SimpleItem] =
    keyToItem
      .filter(_._2.isInstanceOf[SimpleItem])
      .asInstanceOf[MapView[SimpleItemPath, SimpleItem]]

  lazy val pathToUnsignedSimpleItem: MapView[UnsignedSimpleItemPath, UnsignedSimpleItem] =
    keyToItem
      .filter(_._2.isInstanceOf[UnsignedSimpleItem])
      .asInstanceOf[MapView[UnsignedSimpleItemPath, UnsignedSimpleItem]]

  final def keyToItem[I <: InventoryItem](I: InventoryItem.Companion[I]): MapView[I.Key, I] =
    keyToItem
      .filter(_._2.companion eq I)
      .asInstanceOf[MapView[I.Key, I]]


object ItemContainer:
  trait Companion[A]:
    implicit final val implicitItemContainer: Companion[A] = this

    protected def inventoryItems: Seq[InventoryItem.Companion_]

    protected def itemPaths: Seq[InventoryItemPath.AnyCompanion] =
      inventoryItems.map(_.Path)

    final lazy val delegateIds: Seq[DelegateId.Companion_] =
      itemPaths.collect { case o: DelegateId.Companion[?] => o }

    final lazy val versionedItemPaths: Seq[VersionedItemPath.AnyCompanion] =
      inventoryItems.collect { case o: VersionedItem.Companion[?] => o.Path }

    final lazy val versionedControls: Seq[VersionedControl.Companion_] =
      inventoryItems.collect { case o: VersionedControl.Companion[?] => o }

    final lazy val simpleItems: Seq[SimpleItem.Companion_] =
      inventoryItems.collect { case o: SimpleItem.Companion[?] => o }

    final lazy val unsignedSimpleItems: Seq[UnsignedSimpleItem.Companion_] =
      inventoryItems.collect { case o: UnsignedSimpleItem.Companion[?] => o }

    final lazy val unsignedItems: Seq[UnsignedItem.Companion_] =
      inventoryItems.collect { case o: UnsignedItem.Companion[?] => o }

    final lazy val signableItems: Seq[SignableItem.Companion_] =
      inventoryItems.collect { case o: SignableItem.Companion[?] => o }

    final lazy val signableSimpleItems: Seq[SignableSimpleItem.Companion_] =
      inventoryItems.collect { case o: SignableSimpleItem.Companion[?] => o }

    final lazy val versionedItems: Seq[VersionedItem.Companion_] =
      inventoryItems.collect { case o: VersionedItem.Companion[?] => o }

    implicit final lazy val inventoryItemJsonCodec: TypedJsonCodec[InventoryItem] =
      TypedJsonCodec[InventoryItem](inventoryItems.map(_.subtype)*)

    implicit final lazy val inventoryItemEventJsonCodec: TypedJsonCodec[InventoryItemEvent] =
      InventoryItemEvent.jsonCodec(using this)

    implicit final lazy val inventoryItemKeyJsonCodec: Codec[InventoryItemKey] =
      InventoryItemKey.jsonCodec(inventoryItems.map(_.Key))

    implicit final lazy val simpleItemIdJsonCodec: Codec[SimpleItemPath] =
      SimpleItemPath.jsonCodec(simpleItems.map(_.Key))

    implicit final lazy val unsignedSimpleItemJsonCodec: TypedJsonCodec[UnsignedSimpleItem] =
      TypedJsonCodec[UnsignedSimpleItem](unsignedSimpleItems.map(_.subtype)*)

    implicit final lazy val unsignedItemJsonCodec: TypedJsonCodec[UnsignedItem] =
      TypedJsonCodec[UnsignedItem](unsignedItems.map(_.subtype)*)

    implicit final lazy val signableSimpleItemJsonCodec: TypedJsonCodec[SignableSimpleItem] =
      TypedJsonCodec[SignableSimpleItem](signableSimpleItems.map(_.subtype)*)

    implicit final lazy val signableItemJsonCodec: TypedJsonCodec[SignableItem] =
      TypedJsonCodec(signableItems.map(_.subtype)*)

    implicit final lazy val signableItemKeyJsonCodec: Codec[SignableItemKey] =
      SignableItemKey.jsonCodec(signableItems.map(_.Key))

    implicit final lazy val versionedItemJsonCodec: TypedJsonCodec[VersionedItem] =
      TypedJsonCodec(versionedItems.map(_.subtype)*)

    implicit final lazy val versionedItemPathJsonCodec: Codec[VersionedItemPath] =
      InventoryItemPath.jsonCodec(versionedItemPaths)

    implicit final lazy val versionedControlJsonCodec: TypedJsonCodec[VersionedControl] =
    TypedJsonCodec[VersionedControl](versionedControls.map(_.subtype)*)

    implicit final lazy val basicItemEventJsonCodec: TypedJsonCodec[BasicItemEvent] =
      BasicItemEvent.jsonCodec(this)

    implicit final lazy val delegateIdJsonCodec: Codec[DelegateId] =
      InventoryItemPath.jsonCodec(delegateIds)


    def decodeDelegateIdOrAgentPath: Decoder[DelegateId] =
      c => c.get[DelegateId]("delegateId") match
        case Left(_) if !c.keys.exists(_.exists(_ == "delegateId")) =>
          c.get[AgentPath]("agentPath")
        case o => o
