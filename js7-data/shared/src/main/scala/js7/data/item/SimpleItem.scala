package js7.data.item

import io.circe.Codec
import js7.data.item.SimpleItem._

trait SimpleItem extends InventoryItem
{
  protected type Self <: SimpleItem

  def withRevision(revision: Option[ItemRevision]): Self

  val companion: Companion[Self]

  def id: companion.Id

  def itemRevision: Option[ItemRevision]
}

object SimpleItem
{
  type Companion_ = Companion[_ <: SimpleItem]

  trait Companion[A <: SimpleItem] extends InventoryItem.Companion[A]
  {
    type Item  = A
    type Id <: SimpleItemId

    val Id: SimpleItemId.Companion[Id]
  }

  def jsonCodec(companions: Seq[Companion_]): Codec.AsObject[SimpleItem] =
    InventoryItem.jsonCodec(companions)
      .asInstanceOf[Codec.AsObject[SimpleItem]]
}
