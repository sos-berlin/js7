package com.sos.jobscheduler.common.time.timer

import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.InstantJsonCodec
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import io.circe.generic.JsonCodec
import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class TimerOverview(at: Instant, name: String)

object TimerOverview {
  intelliJuseImport(InstantJsonCodec)
}
