package js7.base.session

import js7.base.auth.SessionToken
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait HasSessionToken:

  protected def sessionToken: Option[SessionToken]

  final def hasSession: Boolean =
    sessionToken.isDefined

  implicit def implicitSessionToken: Task[Option[SessionToken]] =
    Task(sessionToken)
