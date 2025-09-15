package js7.agent.web

import js7.agent.data.AgentState
import js7.agent.web.common.AgentRouteProvider
import js7.journal.web.GenericEventRoute

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends AgentRouteProvider, GenericEventRoute:

  protected final lazy val eventRoute = genericEventRoute(AgentState.keyedEventJsonCodec)
