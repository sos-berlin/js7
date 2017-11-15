package com.sos.jobscheduler.master.gui.data.system

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/**
  * @author Joacim Zschimmer
  */
final case class SystemInformation(
  hostname: String,
  distribution: Option[String],
  cpuModel: Option[String])
  //mxBeans: Map[String, Any])  Any requires Decoder

object SystemInformation {
  implicit val jsonDecoder: Decoder[SystemInformation] = deriveDecoder[SystemInformation]
}
