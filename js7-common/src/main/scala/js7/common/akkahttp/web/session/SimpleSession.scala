package js7.common.akkahttp.web.session

import js7.base.auth.SimpleUser

/**
  * @author Joacim Zschimmer
  */
final case class SimpleSession(sessionInit: SessionInit[SimpleUser]) extends Session
{
  type User = SimpleUser

  override def toString = sessionToken.toString
}
