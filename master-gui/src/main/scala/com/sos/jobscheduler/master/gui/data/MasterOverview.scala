package com.sos.jobscheduler.master.gui.data

import com.sos.jobscheduler.master.gui.data.system.{JavaInformation, SystemInformation}
import io.circe.Decoder
import io.circe.generic.semiauto._

/**
  * @author Joacim Zschimmer
  */
final case class MasterOverview(
  version: String,
  startedAt: Timestamp,
  orderCount: Int,
  system: SystemInformation,
  java: JavaInformation)

object MasterOverview {
  implicit val jsonDecoder: Decoder[MasterOverview] = deriveDecoder[MasterOverview]
}


