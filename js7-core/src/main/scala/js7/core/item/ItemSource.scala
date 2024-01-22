package js7.core.item

import js7.base.data.ByteArray
import js7.data.item.{InventoryItemPath, SourceType}

/**
  * @author Joacim Zschimmer
  */
final case class ItemSource(byteArray: ByteArray, path: InventoryItemPath, sourceType: SourceType)
