package js7.agent.web.master

import akka.util.Timeout
import js7.agent.DirectAgentApi
import js7.agent.data.event.KeyedEventJsonFormats.keyedEventJsonCodec
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.SimpleUser
import js7.core.command.CommandMeta
import js7.core.event.GenericEventRoute
import js7.data.event.{Event, KeyedEvent}
import js7.data.master.MasterId
import js7.data.order.OrderEvent.OrderDetached

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
