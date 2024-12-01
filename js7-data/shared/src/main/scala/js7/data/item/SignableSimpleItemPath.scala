package js7.data.item

import io.circe.Codec
import js7.base.generic.GenericString
import js7.data.item.SignableSimpleItemPath.*

trait SignableSimpleItemPath extends SimpleItemPath, SignableItemKey:
  def companion: Companion[? <: SignableSimpleItemPath]


object SignableSimpleItemPath:
  type Companion_ = Companion[? <: SignableSimpleItemPath]

  given Ordering[SignableSimpleItemPath] = GenericString.ordering

  trait Companion[A <: SignableSimpleItemPath]
  extends SimpleItemPath.Companion[A], SignableItemKey.Companion[A]

  def jsonCodec(companions: Iterable[SignableSimpleItemPath.Companion_]): Codec[SignableSimpleItemPath] =
    InventoryItemKey.jsonCodec(companions)
      .asInstanceOf[Codec[SignableSimpleItemPath]]
