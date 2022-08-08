package js7.common.akkahttp.web.session

import js7.base.auth.{SessionToken, SimpleUser}

/**
  * @author Joacim Zschimmer
  */
final case class SessionInit(
  sessionToken: SessionToken,
  /** If isAnonymous, the Session's user may change later due to late authentication. */
  loginUser: SimpleUser)
