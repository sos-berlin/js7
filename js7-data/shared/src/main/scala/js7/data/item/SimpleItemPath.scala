package js7.data.item

import io.circe.Codec
import js7.data.item.SimpleItemPath.*

trait SimpleItemPath extends InventoryItemKey, InventoryItemPath:

  protected type Self <: SimpleItemPath

  def companion: Companion[? <: SimpleItemPath]

  final def path = this


object SimpleItemPath:
  trait Companion[A <: SimpleItemPath]
  extends InventoryItemPath.Companion[A], InventoryItemKey.Companion[A]:

    implicit def implicitCompanion: Companion[A] =
      this

  type AnyCompanion = Companion[? <: SimpleItemPath]

  def jsonCodec(companions: Iterable[AnyCompanion]): Codec[SimpleItemPath] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[SimpleItemPath]]
