package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds._
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentRegisteredMaster
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
object MasterSnapshots
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class MasterMetaState(
    masterId: MasterId,
    startedAt: Timestamp,
    totalRunningTime: FiniteDuration)

  val SnapshotJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype(deriveCodec[MasterMetaState]),
      Subtype[AgentRegisteredMaster],  // These events describe complete objects
      Subtype[RepoEvent],  // These events describe complete objects
      Subtype[AgentSnapshot],  // TODO case class AgentState(eventId: EventId)
      Subtype[Order[Order.State]])
}
