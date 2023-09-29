package js7.subagent

import js7.base.auth.SimpleUser
import js7.common.akkahttp.web.session.{Session, SessionInit}

final case class SubagentSession(sessionInit: SessionInit)
extends Session:
  type User = SimpleUser

  override def toString = sessionToken.toString
