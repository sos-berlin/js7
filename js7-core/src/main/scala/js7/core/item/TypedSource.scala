package js7.core.item

import akka.util.ByteString
import js7.data.item.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class TypedSource(byteString: ByteString, path: TypedPath, sourceType: SourceType)
