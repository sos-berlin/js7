package js7.base.auth

import js7.base.generic.SecretString

/**
  * @author Joacim Zschimmer
  */
final case class SessionToken(secret: SecretString) {
  override def toString = "SessionToken"
}

object SessionToken {
  val HeaderName = "X-JS7-Session"
}
