package js7.subagent.web

import js7.common.akkahttp.web.session.{RouteProvider, SimpleSession}

trait SubagentRouteProvider extends RouteProvider
{
  protected type Session = SimpleSession
}
