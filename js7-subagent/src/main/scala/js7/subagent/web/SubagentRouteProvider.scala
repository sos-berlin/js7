package js7.subagent.web

import io.circe.Encoder
import js7.common.pekkohttp.web.session.RouteProvider
import js7.subagent.SubagentSession

private trait SubagentRouteProvider extends RouteProvider:

  protected type OurSession = SubagentSession
  protected val sessionEncoder = summon[Encoder.AsObject[SubagentSession]]
