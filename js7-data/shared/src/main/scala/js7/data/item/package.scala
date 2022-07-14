package js7.data

/**
  * @author Joacim Zschimmer
  */
package object item
{
  type VersionedItemId_ = VersionedItemId[? <: VersionedItemPath]

  type InventoryItemDiff_ = InventoryItemDiff[InventoryItemPath, InventoryItem]

  type VersionedControlId_ = UnsignedVersionedItemId[? <: VersionedControlPath]
}
