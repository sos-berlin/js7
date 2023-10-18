package js7.data.item

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.data.event.ItemContainer

sealed trait UnsignedItemEvent extends InventoryItemEvent:
  def key: VersionedControlId_


object UnsignedItemEvent:
  // Restricted to VersionedControl Items
  // but may replace UnsignedSimpleItemEvent in future !!!

  sealed trait UnsignedItemAddedOrChanged
  extends UnsignedItemEvent
  with ItemAddedOrChanged:
    def item: VersionedControl
    assertThat(item.itemRevision.isDefined)
  object UnsignedItemAddedOrChanged:
    def unapply(event: UnsignedItemAddedOrChanged) = Some(event.item)

  final case class UnsignedItemAdded(item: VersionedControl)
  extends UnsignedItemAddedOrChanged:
    def key = item.key

  final case class UnsignedItemChanged(item: VersionedControl)
  extends UnsignedItemAddedOrChanged:
    def key = item.key

  implicit def jsonCodec[S](implicit S: ItemContainer.Companion[S])
  : TypedJsonCodec[UnsignedItemEvent] =
    implicit val x = S.versionedControlJsonCodec
    TypedJsonCodec(
      Subtype(deriveCodec[UnsignedItemAdded]),
      Subtype(deriveCodec[UnsignedItemChanged]))
