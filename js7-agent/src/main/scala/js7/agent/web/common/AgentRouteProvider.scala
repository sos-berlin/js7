package js7.agent.web.common

import io.circe.Encoder
import js7.common.pekkohttp.web.session.RouteProvider
import js7.subagent.SubagentSession

/**
 * Standard trait for Agent web services.
 * To be able to mix-in multiple web services, use `addRootRoute` to add a `Route`.
 * Method `buildRoute` returns the combined `Route`.
 *
 * @author Joacim Zschimmer
 */
trait AgentRouteProvider extends RouteProvider:
  protected type OurSession = SubagentSession
  protected val sessionEncoder = summon[Encoder.AsObject[SubagentSession]]
