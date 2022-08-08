package js7.agent.web.common

import js7.common.akkahttp.web.session.{RouteProvider, SimpleSession}

/**
 * Standard trait for Agent web services.
 * To be able to mix-in multiple web services, use `addRootRoute` to add a `Route`.
 * Method `buildRoute` returns the combined `Route`.
 *
 * @author Joacim Zschimmer
 */
trait AgentRouteProvider extends RouteProvider
{
  protected type OurSession = SimpleSession
}
