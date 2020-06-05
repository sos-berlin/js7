package js7.core.filebased

import akka.util.ByteString
import js7.data.filebased.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class TypedSource(byteString: ByteString, path: TypedPath, sourceType: SourceType)
