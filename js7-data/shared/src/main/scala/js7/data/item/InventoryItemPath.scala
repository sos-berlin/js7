package js7.data.item

import js7.base.generic.GenericString
import js7.base.standards.Js7PathValidating

trait InventoryItemPath extends GenericString

object InventoryItemPath
{
  trait Companion[A <: InventoryItemPath] extends Js7PathValidating[A]

  type AnyCompanion = Companion[_ <: InventoryItemPath]
}
