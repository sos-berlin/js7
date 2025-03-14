package js7.data.item

import io.circe.Codec
import js7.data.item.SimpleItem.*

/** Unversioned item. */
trait SimpleItem extends InventoryItem:
  protected type Self <: SimpleItem

  val companion: Companion[Self]

  def rename(key: companion.Key): Self

  def withRevision(revision: Option[ItemRevision]): Self

  def incrementRevision: Self =
    withRevision:
      Some(nextRevision)

  def nextRevision: ItemRevision =
    itemRevision.fold(ItemRevision.Initial)(_.next)

  final def key: companion.Key = path

  def path: companion.Path

  def pathRev: PathRev[companion.Key] =
    PathRev(path, itemRevision)


object SimpleItem:
  type Companion_ = Companion[? <: SimpleItem]

  trait Companion[A <: SimpleItem] extends InventoryItem.Companion[A]:
    type Item = A

    type Key <: SimpleItemPath
    def Key: SimpleItemPath.Companion[Key]

    type Path = Key

  def jsonCodec(companions: Seq[Companion_]): Codec.AsObject[SimpleItem] =
    InventoryItem.jsonCodec(companions)
      .asInstanceOf[Codec.AsObject[SimpleItem]]
