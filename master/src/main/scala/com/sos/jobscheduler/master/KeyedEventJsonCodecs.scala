package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent, TypedPath}
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowEvent, WorkflowPath}
import com.sos.jobscheduler.master.order.OrderScheduleEvent
import com.sos.jobscheduler.master.order.agent.Agent

/**
  * @author Joacim Zschimmer
  */
object KeyedEventJsonCodecs
{
  implicit val MasterTypedPathCompanions = Set[TypedPath.AnyCompanion](
    WorkflowPath,
    AgentPath)

  implicit val MasterTypedPathJsonCodec: CirceCodec[TypedPath] = TypedPath.jsonCodec(MasterTypedPathCompanions)

  implicit val MasterFileBasedJsonCodec: TypedJsonCodec[FileBased] = TypedJsonCodec(
    Subtype[Workflow.Named],
    Subtype[Agent])

  /**
    * All publicly known event classes.
    */
  implicit val MasterKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[RepoEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype[WorkflowEvent],
      KeyedSubtype.singleEvent[AgentEventIdEvent],
      KeyedSubtype[OrderScheduleEvent])
}
