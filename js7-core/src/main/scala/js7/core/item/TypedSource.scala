package js7.core.item

import js7.base.data.ByteArray
import js7.data.item.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class TypedSource(byteArray: ByteArray, path: TypedPath, sourceType: SourceType)
