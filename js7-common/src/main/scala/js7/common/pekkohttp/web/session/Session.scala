package js7.common.pekkohttp.web.session

import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.time.Timestamp
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.:=
import Session.*

/**
  * @author Joacim Zschimmer
  */
trait Session:
  private val _isAlive = Atomic(true)
  private[session] var lastUsed = LastUsed("", Timestamp.Epoch)

  protected[session] def sessionInit: SessionInit

  final def isAlive: Boolean =
    _isAlive.get

  private[session] def die(): Unit =
    _isAlive := false

  final def sessionNumber: Long =
    sessionToken.number

  final def sessionToken: SessionToken =
    sessionInit.sessionToken

  private lazy val _user = Atomic[SimpleUser](sessionInit.loginUser)

  /** User may change once concurrently from Anonymous to non-anonymous due to late authentication. */
  final def currentUser: SimpleUser = _user.get()

  /** Succeeds only once. */
  private[session] final def tryUpdateUser(user: SimpleUser): Boolean =
    _user.compareAndSet(sessionInit.loginUser, user)


object Session:
  final case class LastUsed(what: String, timestamp: Timestamp)