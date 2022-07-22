package js7.data.controller

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.system.SystemInformation
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.system.JavaInformation
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class ControllerOverview(
  id: ControllerId,
  version: String,
  buildId: String,
  initiallyStartedAt: Option[Timestamp],
  startedAt: Timestamp,
  totalRunningTime: FiniteDuration,
  orderCount: Option[Int],
  system: SystemInformation,
  java: JavaInformation)

object ControllerOverview
{
  implicit val jsonCodec: Codec.AsObject[ControllerOverview] = deriveCodec
  intelliJuseImport(FiniteDurationJsonEncoder)
}
