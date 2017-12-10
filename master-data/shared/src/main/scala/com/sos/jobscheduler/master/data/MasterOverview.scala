package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.system.JavaInformation

/**
  * @author Joacim Zschimmer
  */
final case class MasterOverview(
  version: String,
  buildId: String,
  startedAt: Timestamp,
  orderCount: Int,
  system: SystemInformation,
  java: JavaInformation)

object MasterOverview {
  implicit val JsonCodec = deriveCirceCodec[MasterOverview]
}
