package com.sos.jobscheduler.agent.web.master

import akka.util.Timeout
import com.sos.jobscheduler.agent.DirectAgentApi
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.keyedEventJsonCodec
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.event.GenericEventRoute
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached

/**
  * @author Joacim Zschimmer
  */
trait MastersEventRoute extends AgentRouteProvider with GenericEventRoute
{
  protected def agentApi(meta: CommandMeta): DirectAgentApi
  implicit protected def akkaAskTimeout: Timeout

  protected final lazy val masterEventRoute = new RouteProvider().route

  private class RouteProvider extends GenericEventRouteProvider
  {
    def keyedEventTypedJsonCodec = keyedEventJsonCodec

    def eventWatchFor(user: SimpleUser) = agentApi(CommandMeta(user)).eventWatchForMaster(MasterId.fromUserId(user.id))

    override def isRelevantEvent(keyedEvent: KeyedEvent[Event]) =
      keyedEvent.event != OrderDetached  // Master knows about detached order by successful executed AgentCommand.DetachOrder
  }
}
