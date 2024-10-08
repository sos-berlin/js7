package js7.agent.web

import js7.agent.data.AgentState
import js7.agent.web.common.AgentRouteProvider
import js7.journal.web.GenericEventRoute
import org.apache.pekko.util.Timeout

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends AgentRouteProvider, GenericEventRoute:

  implicit protected def pekkoAskTimeout: Timeout

  protected final lazy val eventRoute = genericEventRoute(AgentState.keyedEventJsonCodec)
