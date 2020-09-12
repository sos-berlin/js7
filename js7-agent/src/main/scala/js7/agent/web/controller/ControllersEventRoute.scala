package js7.agent.web.controller

import akka.util.Timeout
import js7.agent.DirectAgentApi
import js7.agent.data.AgentState
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.SimpleUser
import js7.core.command.CommandMeta
import js7.core.event.GenericEventRoute
import js7.data.controller.ControllerId

/**
  * @author Joacim Zschimmer
  */
trait ControllersEventRoute extends AgentRouteProvider with GenericEventRoute
{
  protected def agentApi(meta: CommandMeta): DirectAgentApi
  implicit protected def akkaAskTimeout: Timeout

  protected final lazy val controllerEventRoute = new RouteProvider().route

  private class RouteProvider extends GenericEventRouteProvider
  {
    def keyedEventTypedJsonCodec = AgentState.keyedEventJsonCodec

    def eventWatchFor(user: SimpleUser) =
      agentApi(CommandMeta(user)).eventWatchForController(ControllerId.fromUserId(user.id))
  }
}
