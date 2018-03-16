package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class FileBasedsOverview(count: Int)

object FileBasedsOverview {
  implicit val jsonCodec = deriveCodec[FileBasedsOverview]
}
