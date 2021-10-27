package js7.data.controller

import js7.base.circeutils.CirceUtils.{deriveCodec, deriveRenamingCodec}
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.event.NoKeyEvent
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait ControllerEvent extends NoKeyEvent

object ControllerEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class ControllerInitialized(
    controllerId: ControllerId,
    initiallyStartedAt: Timestamp)
  extends ControllerEvent

  final case class ControllerReady(timezone: String, totalRunningTime: FiniteDuration)
  extends ControllerEvent

  final case class ControllerShutDown(clusterAction: Option[ControllerShutDown.ClusterAction] = None)
  extends ControllerEvent
  object ControllerShutDown {
    sealed trait ClusterAction
    object ClusterAction {
      case object Failover extends ClusterAction
      case object CompleteShutdown extends ClusterAction
      implicit val jsonCodec: TypedJsonCodec[ClusterAction] = TypedJsonCodec[ClusterAction](
        Subtype(Failover),
        Subtype(CompleteShutdown))
    }
  }

  case object ControllerTestEvent extends ControllerEvent

  implicit val jsonCodec = TypedJsonCodec[ControllerEvent](
    Subtype(deriveRenamingCodec[ControllerInitialized](Map(
      "startedAt" -> "initiallyStartedAt"))),
    Subtype(deriveCodec[ControllerReady]),
    Subtype(deriveCodec[ControllerShutDown]),
    Subtype(ControllerTestEvent))
}
