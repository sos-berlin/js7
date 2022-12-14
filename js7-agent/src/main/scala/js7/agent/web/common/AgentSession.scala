package js7.agent.web.common

import js7.base.auth.SimpleUser
import js7.common.akkahttp.web.session.{Session, SessionInit}
import js7.core.cluster.watch.ClusterWatchSession

final case class AgentSession(sessionInit: SessionInit)
extends Session with ClusterWatchSession
{
  type User = SimpleUser

  override def toString = sessionToken.toString
}
