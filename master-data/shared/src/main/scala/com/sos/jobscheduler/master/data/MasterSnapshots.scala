package js7.master.data

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.cluster.ClusterState
import js7.data.event.JournalState
import js7.data.filebased.RepoEvent
import js7.data.master.MasterFileBaseds._
import js7.data.master.MasterId
import js7.data.order.Order
import js7.master.data.agent.AgentSnapshot
import js7.master.data.events.MasterAgentEvent.AgentRegisteredMaster

/**
  * @author Joacim Zschimmer
  */
object MasterSnapshots
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class MasterMetaState(masterId: MasterId, startedAt: Timestamp, timezone: String)
  {
    def isDefined = this != MasterMetaState.Undefined
  }

  object MasterMetaState {
    val Undefined = MasterMetaState(MasterId("UNDEFINED-MASTER-ID"), Timestamp.ofEpochMilli(0), timezone = "UTC")
  }

  val SnapshotJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[JournalState],
      Subtype(deriveCodec[ClusterState.ClusterStateSnapshot]),
      Subtype(deriveCodec[MasterMetaState]),
      Subtype[RepoEvent],  // These events describe complete objects
      Subtype[AgentSnapshot],
      Subtype[AgentRegisteredMaster],  // These events describe complete objects
      Subtype[Order[Order.State]])
}
