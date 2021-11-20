package js7.data

/**
  * @author Joacim Zschimmer
  */
package object item
{
  type VersionedItemId_ = VersionedItemId[_ <: VersionedItemPath]

  type InventoryItemDiff_ = InventoryItemDiff[InventoryItemPath, InventoryItem]
}
