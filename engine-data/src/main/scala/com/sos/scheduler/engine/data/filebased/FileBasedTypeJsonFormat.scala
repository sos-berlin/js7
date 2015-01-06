package com.sos.scheduler.engine.data.filebased

/**
 * @author Joacim Zschimmer
 */
object FileBasedTypeJsonFormat {
  implicit val MyJsonFormat = FileBasedType.MyJsonFormat
}
