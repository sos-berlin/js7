package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.instant.StringInstantJsonCodec
import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
final case class OrderScheduleEndedAt(instant: Instant)

object OrderScheduleEndedAt {
  implicit val jsonCodec = deriveCirceCodec[OrderScheduleEndedAt]
}
