package com.sos.jobscheduler.master.configuration

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, JournalStateEvent, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent, TypedPath}
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowEvent, WorkflowPath}
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.master.scheduledorder.OrderScheduleEvent

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
    Subtype.named[Workflow]("Workflow"),  // TODO Test is missing
    Subtype[Agent])

  /**
    * All publicly known event classes.
    */
  implicit val MasterKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[MasterEvent],
      KeyedSubtype[RepoEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype.singleEvent[WorkflowEvent.WorkflowAttached],
      KeyedSubtype.singleEvent[AgentEventIdEvent],
      KeyedSubtype[JournalStateEvent],
      KeyedSubtype[OrderScheduleEvent])
}
