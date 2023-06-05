package js7.subagent.web

import js7.common.akkahttp.web.session.RouteProvider
import js7.subagent.SubagentSession

private trait SubagentRouteProvider extends RouteProvider
{
  protected type OurSession = SubagentSession
}
