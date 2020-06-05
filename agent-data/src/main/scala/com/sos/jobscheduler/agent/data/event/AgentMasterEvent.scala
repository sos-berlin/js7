package js7.agent.data.event

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.event.NoKeyEvent
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
trait AgentMasterEvent extends NoKeyEvent

object AgentMasterEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class AgentReadyForMaster(timezone: String, totalRunningTime: FiniteDuration) extends AgentMasterEvent

  implicit val jsonCodec = TypedJsonCodec[AgentMasterEvent](
    Subtype(deriveCodec[AgentReadyForMaster]))
}
