package  js7.data_for_java.item

import js7.data.item.InventoryItem
import js7.data_for_java.common.JavaWrapper

trait JInventoryItem extends JavaWrapper
{
  protected type AsScala <: InventoryItem
}
