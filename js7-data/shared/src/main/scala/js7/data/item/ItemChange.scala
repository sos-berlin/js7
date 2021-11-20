package js7.data.item

sealed trait ItemChange
{
  def path: InventoryItemPath
}

object ItemChange
{
  sealed trait Change extends ItemChange

  final case class AddedOrChanged(item: InventoryItem) extends ItemChange {
    def path = item.path
    def toShortString = s"AddedOrChanged(${item.key})"
  }

  final case class Removed(path: InventoryItemPath) extends ItemChange
}
