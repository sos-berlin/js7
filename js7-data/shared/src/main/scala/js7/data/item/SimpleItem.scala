package js7.data.item

import io.circe.Codec
import js7.data.item.SimpleItem._

trait SimpleItem extends InventoryItem
{
  protected type Self <: SimpleItem

  def withRevision(revision: Option[ItemRevision]): Self

  val companion: Companion[Self]

  final def key = path

  def path: companion.Key

  def itemRevision: Option[ItemRevision]
}

object SimpleItem
{
  type Companion_ = Companion[_ <: SimpleItem]

  trait Companion[A <: SimpleItem] extends InventoryItem.Companion[A]
  {
    type Item  = A

    type Key <: SimpleItemPath
    val Key: SimpleItemPath.Companion[Key]
  }

  def jsonCodec(companions: Seq[Companion_]): Codec.AsObject[SimpleItem] =
    InventoryItem.jsonCodec(companions)
      .asInstanceOf[Codec.AsObject[SimpleItem]]
}
