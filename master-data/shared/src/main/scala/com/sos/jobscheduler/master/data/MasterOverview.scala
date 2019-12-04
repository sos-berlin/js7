package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.system.JavaInformation
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class MasterOverview(
  id: MasterId,
  version: String,
  buildId: String,
  startedAt: Option[Timestamp],
  totalRunningTime: FiniteDuration,
  orderCount: Option[Int],
  system: SystemInformation,
  java: JavaInformation)

object MasterOverview
{
  implicit val jsonCodec = deriveCodec[MasterOverview]
  intelliJuseImport(FiniteDurationJsonEncoder)
}
