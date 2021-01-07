package js7.common.akkahttp.web.session

import js7.base.auth.{SessionToken, User}

/**
  * @author Joacim Zschimmer
  */
final case class SessionInit[U <: User](
  sessionToken: SessionToken,
  /** If isAnonymous, the Session's user may change later due to late authentication. */
  loginUser: U)
