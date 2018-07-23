package com.sos.jobscheduler.agent.web

import akka.util.Timeout
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.keyedEventJsonCodec
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.core.event.GenericEventRoute
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached

/**
  * @author Joacim Zschimmer
  */
trait MastersEventWebService extends AgentRouteProvider with GenericEventRoute
{
  protected def agentHandle: AgentHandle
  implicit protected def akkaAskTimeout: Timeout

  protected final lazy val masterEventRoute = new RouteProvider().route

  private class RouteProvider extends GenericEventRouteProvider
  {
    def keyedEventTypedJsonCodec = keyedEventJsonCodec

    def eventWatchFor(userId: UserId) = agentHandle.eventWatch(MasterId.fromUserId(userId))

    override def isRelevantEvent(keyedEvent: KeyedEvent[Event]) =
      keyedEvent.event != OrderDetached  // Master knows about detached order by successful executed AgentCommand.DetachOrder
  }
}
