package js7.controller.data

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.system.SystemInformation
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.controller.ControllerId
import js7.data.system.JavaInformation
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class ControllerOverview(
  id: ControllerId,
  version: String,
  buildId: String,
  startedAt: Option[Timestamp],
  totalRunningTime: FiniteDuration,
  orderCount: Option[Int],
  system: SystemInformation,
  java: JavaInformation)

object ControllerOverview
{
  implicit val jsonCodec = deriveCodec[ControllerOverview]
  intelliJuseImport(FiniteDurationJsonEncoder)
}
