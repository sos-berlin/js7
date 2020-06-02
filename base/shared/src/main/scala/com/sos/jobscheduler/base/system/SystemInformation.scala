package com.sos.jobscheduler.base.system

import com.sos.jobscheduler.base.circeutils.AnyJsonCodecs.implicits._
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import java.lang.management.ManagementFactory.getOperatingSystemMXBean

final case class SystemInformation(
  hostname: String,
  distribution: Option[String] = None,
  cpuModel: Option[String] = None,
  mxBeans: Map[String, Any] = Map())

object SystemInformation {
  (MapJsonDecoder, MapJsonEncoder)  // Force import usage for IntelliJ (hidden usage by @JsonCocec)

  def totalPhysicalMemory: Option[Long] =
    getOperatingSystemMXBean match {
      case o: com.sun.management.OperatingSystemMXBean => Some(o.getTotalPhysicalMemorySize)
      case _ => None
  }

  val ForTest = SystemInformation(hostname = "HOSTNAME")

  implicit val jsonCodec = deriveCodec[SystemInformation]
}
