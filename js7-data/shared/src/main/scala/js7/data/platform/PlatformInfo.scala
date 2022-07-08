package js7.data.platform

import io.circe.generic.semiauto.deriveCodec
import js7.base.version.Version
import js7.data.system.JavaInformation

final case class PlatformInfo(
  js7Version: Version,
  hostname: String,
  operatingSystemDistribution: Option[String] = None,
  cpuModel: Option[String] = None,
  java: JavaInformation)

object PlatformInfo {
  implicit val jsonCodec = deriveCodec[PlatformInfo]
}
