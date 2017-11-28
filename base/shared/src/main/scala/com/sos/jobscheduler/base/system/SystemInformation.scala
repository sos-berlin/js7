package com.sos.jobscheduler.base.system

import com.sos.jobscheduler.base.circeutils.AnyJsonCodecs.implicits._
import io.circe.generic.JsonCodec

@JsonCodec
final case class SystemInformation(
  hostname: String,
  distribution: Option[String] = None,
  cpuModel: Option[String] = None,
  mxBeans: Map[String, Any] = Map())

object SystemInformation {
  (MapJsonDecoder, MapJsonEncoder)  // Force import usage for IntelliJ (hidden usage by @JsonCocec)

  val ForTest = SystemInformation(hostname = "HOSTNAME")
}
