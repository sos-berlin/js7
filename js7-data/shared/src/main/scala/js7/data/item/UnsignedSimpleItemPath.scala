package js7.data.item

import io.circe.Codec
import js7.data.item.UnsignedSimpleItemPath.*

trait UnsignedSimpleItemPath
extends UnsignedItemPath, UnsignedItemKey, SimpleItemPath:

  protected type Self <: UnsignedSimpleItemPath

  def companion: Companion[? <: UnsignedSimpleItemPath]


object UnsignedSimpleItemPath:
  type Companion_ = Companion[? <: UnsignedSimpleItemPath]

  trait Companion[A <: UnsignedSimpleItemPath]
  extends UnsignedItemPath.Companion[A], UnsignedItemKey.Companion[A], SimpleItemPath.Companion[A]:

    type Item <: UnsignedSimpleItem
    override implicit def implicitCompanion: Companion[A] = this

  def jsonCodec(companions: Iterable[UnsignedSimpleItemPath.Companion_]): Codec[UnsignedSimpleItemPath] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[UnsignedSimpleItemPath]]
