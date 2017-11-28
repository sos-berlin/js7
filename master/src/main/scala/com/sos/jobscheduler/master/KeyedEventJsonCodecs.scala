package com.sos.jobscheduler.master

import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonCodec
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.data.workflow.WorkflowEvent
import com.sos.jobscheduler.master.order.OrderScheduleEvent

/**
  * @author Joacim Zschimmer
  */
object KeyedEventJsonCodecs {

  /**
    * All publicly known event classes.
    */
  implicit val MasterKeyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[OrderEvent],
      KeyedSubtype[WorkflowEvent],
      KeyedSubtype.singleEvent[AgentEventIdEvent],
      KeyedSubtype[OrderScheduleEvent])

}
