package js7.data.item

import io.circe.Codec
import js7.data.item.SignableSimpleItemPath.*

trait SignableSimpleItemPath extends SimpleItemPath with SignableItemKey:
  def companion: Companion[? <: SignableSimpleItemPath]


object SignableSimpleItemPath:
  type Companion_ = Companion[? <: SignableSimpleItemPath]

  trait Companion[A <: SignableSimpleItemPath]
  extends SimpleItemPath.Companion[A]
  with SignableItemKey.Companion[A]

  def jsonCodec(companions: Iterable[SignableSimpleItemPath.Companion_]): Codec[SignableSimpleItemPath] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[SignableSimpleItemPath]]
