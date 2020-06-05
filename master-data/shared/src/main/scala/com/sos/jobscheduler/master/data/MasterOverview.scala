package js7.master.data

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.system.SystemInformation
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.master.MasterId
import js7.data.system.JavaInformation
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class MasterOverview(
  id: MasterId,
  version: String,
  buildId: String,
  startedAt: Option[Timestamp],
  totalRunningTime: FiniteDuration,
  orderCount: Option[Int],
  system: SystemInformation,
  java: JavaInformation)

object MasterOverview
{
  implicit val jsonCodec = deriveCodec[MasterOverview]
  intelliJuseImport(FiniteDurationJsonEncoder)
}
