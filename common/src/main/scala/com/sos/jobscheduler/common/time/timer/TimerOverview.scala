package com.sos.jobscheduler.common.time.timer

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.instant.StringInstantJsonCodec
import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
final case class TimerOverview(at: Instant, name: String)

object TimerOverview {
  implicit val jsonCodec = deriveCodec[TimerOverview]
}
