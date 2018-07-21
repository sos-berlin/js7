package com.sos.jobscheduler.agent.web

import akka.util.Timeout
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.core.event.AbstractEventRoute
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached

/**
  * @author Joacim Zschimmer
  */
trait MastersEventWebService extends AgentRouteProvider with AbstractEventRoute[OrderEvent]
{
  protected def agentHandle: AgentHandle
  implicit protected def akkaAskTimeout: Timeout

  protected def eventClass =
    classOf[OrderEvent]

  protected def keyedEventTypedJsonCodec =
    KeyedEventJsonFormats.keyedEventJsonCodec

  protected def eventWatchFor(userId: UserId) =
    agentHandle.eventWatch(MasterId.fromUserId(userId))

  override protected def isRelevantEvent(keyedEvent: KeyedEvent[OrderEvent]) =
    keyedEvent.event != OrderDetached  // Master knows about detached order by successful executed AgentCommand.DetachOrder

  protected final def masterEventRoute =
    abstractEventRoute
}
