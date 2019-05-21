package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds._
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentRegisteredMaster

/**
  * @author Joacim Zschimmer
  */
object MasterSnapshots
{
  val SnapshotJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[AgentRegisteredMaster],  // These events describe complete objects
      Subtype[RepoEvent],  // These events describe complete objects
      Subtype[AgentSnapshot],  // TODO case class AgentState(eventId: EventId)
      Subtype[Order[Order.State]])
}
