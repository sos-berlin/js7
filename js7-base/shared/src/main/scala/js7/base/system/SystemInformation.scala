package js7.base.system

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.AnyJsonCodecs.implicits._

final case class SystemInformation(
  hostname: String,
  distribution: Option[String] = None,
  cpuModel: Option[String] = None,
  mxBeans: Map[String, Any] = Map())

object SystemInformation {
  (MapJsonDecoder, MapJsonEncoder)  // Force import usage for IntelliJ (hidden usage by @JsonCocec)

  val ForTest = SystemInformation(hostname = "HOSTNAME")

  implicit val jsonCodec = deriveCodec[SystemInformation]
}
