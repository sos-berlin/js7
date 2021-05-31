package js7.core.item

import js7.base.data.ByteArray
import js7.data.item.{SourceType, VersionedItemPath}

/**
  * @author Joacim Zschimmer
  */
final case class ItemSource(byteArray: ByteArray, path: VersionedItemPath, sourceType: SourceType)
