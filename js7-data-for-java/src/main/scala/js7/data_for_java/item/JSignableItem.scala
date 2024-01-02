package js7.data_for_java.item

import js7.data.item.SignableItem

trait JSignableItem extends JInventoryItem:
  type AsScala <: SignableItem
