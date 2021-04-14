package js7.data.item

import io.circe.Codec
import js7.data.item.SimpleItem._

trait SimpleItem extends InventoryItem
{
  protected type Self <: SimpleItem

  def withRevision(revision: ItemRevision): Self

  val companion: Companion

  def id: companion.Id

  def itemRevision: Option[ItemRevision]
}

object SimpleItem
{
  trait Companion extends InventoryItem.Companion
  {
    type Item <: SimpleItem
    type Id <: SimpleItemId

    val Id: SimpleItemId.Companion[Id]
  }

  def jsonCodec(companions: Seq[Companion]): Codec.AsObject[SimpleItem] =
    InventoryItem.jsonCodec(companions)
      .asInstanceOf[Codec.AsObject[SimpleItem]]
}
