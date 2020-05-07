package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.JournalState
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds._
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentRegisteredMaster

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
