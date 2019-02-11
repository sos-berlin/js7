package com.sos.jobscheduler.core.filebased

import akka.util.ByteString
import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class TypedSource(byteString: ByteString, path: TypedPath, sourceType: SourceType)
