package js7.common.pekkohttp.web.session

import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.time.Timestamp
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
final case class SessionInit(
  sessionToken: SessionToken,
  /** If isAnonymous, the Session's user may change later due to late authentication. */
  loginUser: SimpleUser,
  source: String,
  loggedInAt: Timestamp,
  private[session] timeoutAt: Option[Deadline] = None)
