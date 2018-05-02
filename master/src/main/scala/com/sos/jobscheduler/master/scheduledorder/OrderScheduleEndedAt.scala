package com.sos.jobscheduler.master.scheduledorder

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.instant.StringInstantJsonCodec
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport

/**
  * @author Joacim Zschimmer
  */
final case class OrderScheduleEndedAt(timestamp: Timestamp)

object OrderScheduleEndedAt {
  intelliJuseImport(StringInstantJsonCodec)
  implicit val jsonCodec = deriveCodec[OrderScheduleEndedAt]
}
