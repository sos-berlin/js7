package js7.data.event

import io.circe.Codec
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemKey, SignableItem, SignableItemKey, SignableSimpleItem, SimpleItem, SimpleItemPath, UnsignedSimpleItem, UnsignedSimpleItemPath, VersionedItem, VersionedItemPath}
import scala.collection.MapView

trait ItemContainer
{
  def keyToItem: MapView[InventoryItemKey, InventoryItem]

  final lazy val pathToSimpleItem: MapView[SimpleItemPath, SimpleItem] =
    keyToItem.asInstanceOf[MapView[SimpleItemPath, SimpleItem]]
      .filter(_._2.isInstanceOf[SimpleItem])

  lazy val pathToUnsignedSimpleItem: MapView[UnsignedSimpleItemPath, UnsignedSimpleItem] =
    keyToItem
      .filter(_._2.isInstanceOf[UnsignedSimpleItem])
      .asInstanceOf[MapView[UnsignedSimpleItemPath, UnsignedSimpleItem]]

  final def keyTo[I <: InventoryItem](I: InventoryItem.Companion[I]): MapView[I.Key, I] =
    keyToItem
      .filter { case (_, v) => I.cls.isAssignableFrom(v.getClass) }
      .asInstanceOf[MapView[I.Key, I]]
}

object ItemContainer
{
  trait Companion[A]
  {
    implicit final val implicitItemContainer: Companion[A] = this

    protected def inventoryItems: Seq[InventoryItem.Companion_]

    final lazy val itemPaths: Seq[VersionedItemPath.AnyCompanion] =
      inventoryItems.collect { case o: VersionedItem.Companion_ => o.Path }

    final lazy val simpleItems: Seq[SimpleItem.Companion_] =
      inventoryItems collect { case o: SimpleItem.Companion_ => o }

    final lazy val unsignedSimpleItems: Seq[UnsignedSimpleItem.Companion_] =
      inventoryItems collect { case o: UnsignedSimpleItem.Companion_ => o }

    final lazy val signableItems: Seq[SignableItem.Companion_] =
      inventoryItems collect { case o: SignableItem.Companion_ => o }

    final lazy val signableSimpleItems: Seq[SignableSimpleItem.Companion_] =
      inventoryItems collect { case o: SignableSimpleItem.Companion_ => o }

    final lazy val versionedItems: Seq[VersionedItem.Companion_] =
      inventoryItems collect { case o: VersionedItem.Companion_ => o }


    implicit final lazy val inventoryItemJsonCodec: TypedJsonCodec[InventoryItem] =
      TypedJsonCodec[InventoryItem](inventoryItems.map(_.subtype): _*)

    implicit final lazy val inventoryItemEventJsonCodec: TypedJsonCodec[InventoryItemEvent] =
      InventoryItemEvent.jsonCodec(this)

    implicit final lazy val inventoryItemKeyJsonCodec: Codec[InventoryItemKey] =
      InventoryItemKey.jsonCodec(inventoryItems.map(_.Key))

    implicit final lazy val simpleItemIdJsonCodec: Codec[SimpleItemPath] =
      SimpleItemPath.jsonCodec(simpleItems.map(_.Key))

    implicit final lazy val unsignedSimpleItemJsonCodec: TypedJsonCodec[UnsignedSimpleItem] =
    TypedJsonCodec[UnsignedSimpleItem](unsignedSimpleItems.map(_.subtype): _*)

    implicit final lazy val signableSimpleItemJsonCodec: TypedJsonCodec[SignableSimpleItem] =
      TypedJsonCodec[SignableSimpleItem](signableSimpleItems.map(_.subtype): _*)

    implicit final lazy val signableItemJsonCodec: TypedJsonCodec[SignableItem] =
      TypedJsonCodec(signableItems.map(_.subtype): _*)

    implicit final lazy val signableItemKeyJsonCodec: Codec[SignableItemKey] =
      SignableItemKey.jsonCodec(signableItems.map(_.Key))

    implicit final lazy val versionedItemJsonCodec: TypedJsonCodec[VersionedItem] =
      TypedJsonCodec(versionedItems.map(_.subtype): _*)

    implicit final lazy val versionedItemPathJsonCodec: Codec[VersionedItemPath] =
      VersionedItemPath.jsonCodec(itemPaths)

    implicit final lazy val basicItemEventJsonCodec: TypedJsonCodec[BasicItemEvent] =
      BasicItemEvent.jsonCodec(this)
  }
}
