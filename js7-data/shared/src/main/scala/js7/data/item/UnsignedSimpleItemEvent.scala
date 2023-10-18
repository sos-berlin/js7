package js7.data.item

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.data.event.ItemContainer

// May be replaced by UnsignedItemEvent !!!
sealed trait UnsignedSimpleItemEvent extends InventoryItemEvent:
  def key: UnsignedSimpleItemPath


object UnsignedSimpleItemEvent:
  sealed trait UnsignedSimpleItemAddedOrChanged extends UnsignedSimpleItemEvent with ItemAddedOrChanged:
    def item: UnsignedSimpleItem
    assertThat(item.itemRevision.isDefined)
  object UnsignedSimpleItemAddedOrChanged:
    def unapply(event: UnsignedSimpleItemAddedOrChanged) = Some(event.item)

  final case class UnsignedSimpleItemAdded(item: UnsignedSimpleItem)
  extends UnsignedSimpleItemAddedOrChanged:
    def key = item.key

  final case class UnsignedSimpleItemChanged(item: UnsignedSimpleItem)
  extends UnsignedSimpleItemAddedOrChanged:
    def key = item.key

  def jsonCodec[S](implicit S: ItemContainer.Companion[S])
  : TypedJsonCodec[UnsignedSimpleItemEvent] =
    implicit val x = S.unsignedSimpleItemJsonCodec
    TypedJsonCodec(
      Subtype(deriveCodec[UnsignedSimpleItemAdded]),
      Subtype(deriveCodec[UnsignedSimpleItemChanged]))
