package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.system.JavaInformation
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class MasterOverview(
  version: String,
  startedAt: Timestamp,
  orderCount: Int,
  system: SystemInformation,
  java: JavaInformation)
