package js7.data.platform

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.time.Timestamp
import js7.base.version.Version
import js7.data.system.JavaInformation
import org.jetbrains.annotations.TestOnly

final case class PlatformInfo(
  timestamp: Timestamp,
  timezone: String,
  js7Version: Version,
  hostname: String,
  operatingSystemDistribution: Option[String] = None,
  cpuModel: Option[String] = None,
  java: JavaInformation)

object PlatformInfo {
  implicit val jsonCodec: Codec.AsObject[PlatformInfo] = deriveCodec[PlatformInfo]

  @TestOnly
  val test = PlatformInfo(
    Timestamp("2022-07-08T12:00:00Z"),
    timezone = "Europe/Berlin",
    Version("2.4.0-TEST"),
    hostname = "HOST",
    operatingSystemDistribution = Some("DISTRIBUTION"),
    cpuModel = Some("CPU"),
    JavaInformation(
      version = "x.y.z",
      availableProcessors = 8,
      JavaInformation.Memory(maximum = 3, total = 2, free = 1),
      systemProperties = Map("test" -> "TEST")))}
