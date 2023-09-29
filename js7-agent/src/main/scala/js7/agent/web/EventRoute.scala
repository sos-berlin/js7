package js7.agent.web

import akka.util.Timeout
import js7.agent.data.AgentState
import js7.agent.web.common.AgentRouteProvider
import js7.journal.web.GenericEventRoute

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends AgentRouteProvider with GenericEventRoute:
  implicit protected def akkaAskTimeout: Timeout

  protected final lazy val eventRoute = new RouteProvider().route

  private class RouteProvider extends GenericEventRouteProvider:
    def keyedEventTypedJsonCodec = AgentState.keyedEventJsonCodec
