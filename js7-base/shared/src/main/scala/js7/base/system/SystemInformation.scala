package js7.base.system

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.AnyJsonCodecs.implicits.*
import js7.base.utils.IntelliJUtils.intelliJuseImport

final case class SystemInformation(
  hostname: String,
  distribution: Option[String] = None,
  cpuModel: Option[String] = None,
  mxBeans: Map[String, Map[String, Any]] = Map())


object SystemInformation:
  intelliJuseImport((MapJsonDecoder, MapJsonEncoder))

  val ForTest: SystemInformation = SystemInformation(hostname = "HOSTNAME")

  implicit val jsonCodec: Codec.AsObject[SystemInformation] = deriveCodec[SystemInformation]
