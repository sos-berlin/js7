package js7.base.session

import cats.effect.IO
import js7.base.auth.SessionToken

/**
  * @author Joacim Zschimmer
  */
trait HasSessionToken:

  protected def sessionToken: Option[SessionToken]

  final def hasSession: Boolean =
    sessionToken.isDefined

  implicit def implicitSessionToken: IO[Option[SessionToken]] =
    IO(sessionToken)
