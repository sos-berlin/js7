package js7.data.item

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.data.event.ItemContainer

sealed trait VersionedControlEvent extends InventoryItemEvent
{
  def key: VersionedControlId_
}

object VersionedControlEvent
{
  sealed trait VersionedControlAddedOrChanged
  extends VersionedControlEvent
  with ItemAddedOrChanged
  {
    def item: VersionedControl
    assertThat(item.itemRevision.isDefined)
  }
  object VersionedControlAddedOrChanged {
    def unapply(event: VersionedControlAddedOrChanged) = Some(event.item)
  }

  final case class VersionedControlAdded(item: VersionedControl)
  extends VersionedControlAddedOrChanged
  {
    def key = item.key
  }

  final case class VersionedControlChanged(item: VersionedControl)
  extends VersionedControlAddedOrChanged
  {
    def key = item.key
  }

  implicit def jsonCodec[S](implicit S: ItemContainer.Companion[S])
  : TypedJsonCodec[VersionedControlEvent] = {
    implicit val x = S.versionedControlJsonCodec
    TypedJsonCodec(
      Subtype(deriveCodec[VersionedControlAdded]),
      Subtype(deriveCodec[VersionedControlChanged]))
  }
}
