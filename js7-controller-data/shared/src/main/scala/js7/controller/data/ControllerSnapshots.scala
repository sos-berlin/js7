package js7.controller.data

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.controller.data.agent.AgentSnapshot
import js7.controller.data.events.ControllerAgentEvent.AgentRegisteredController
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerId
import js7.data.controller.ControllerItems._
import js7.data.event.{JournalHeader, JournalState, SnapshotMeta}
import js7.data.item.RepoEvent
import js7.data.order.Order

/**
  * @author Joacim Zschimmer
  */
object ControllerSnapshots
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class ControllerMetaState(controllerId: ControllerId, startedAt: Timestamp, timezone: String)
  {
    def isDefined = this != ControllerMetaState.Undefined
  }

  object ControllerMetaState {
    val Undefined = ControllerMetaState(ControllerId("UNDEFINED-CONTROLLER-ID"), Timestamp.ofEpochMilli(0), timezone = "UTC")
  }

  val SnapshotJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[JournalHeader],
      Subtype[SnapshotMeta],
      Subtype[JournalState],
      Subtype(deriveCodec[ClusterState.ClusterStateSnapshot]),
      Subtype(deriveCodec[ControllerMetaState]),
      Subtype[RepoEvent],  // These events describe complete objects
      Subtype[AgentSnapshot],
      Subtype[AgentRegisteredController],  // These events describe complete objects
      Subtype[Order[Order.State]])
}
