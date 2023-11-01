package js7.subagent.web

import js7.common.pekkohttp.web.session.{RouteProvider, SimpleSession}

private trait SubagentRouteProvider extends RouteProvider
{
  protected type OurSession = SimpleSession
}
