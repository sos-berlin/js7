package js7.agent.web.common

import js7.base.auth.SimpleUser
import js7.common.akkahttp.web.session.{Session, SessionInit}

final case class AgentSession(sessionInit: SessionInit)
extends Session
{
  type User = SimpleUser

  override def toString = sessionToken.toString
}
