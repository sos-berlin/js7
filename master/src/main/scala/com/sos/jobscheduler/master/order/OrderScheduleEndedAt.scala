package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.InstantJsonCodec
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import io.circe.generic.JsonCodec
import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class OrderScheduleEndedAt(instant: Instant)

object OrderScheduleEndedAt {
  intelliJuseImport(InstantJsonCodec)
}
