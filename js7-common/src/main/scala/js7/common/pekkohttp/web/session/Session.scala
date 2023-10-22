package js7.common.pekkohttp.web.session

import js7.base.auth.SimpleUser
import js7.base.utils.Atomic

/**
  * @author Joacim Zschimmer
  */
trait Session extends HasTimeout:
  protected[session] def sessionInit: SessionInit

  final def sessionNumber = sessionToken.number

  final def sessionToken = sessionInit.sessionToken

  private lazy val _user = Atomic[SimpleUser](sessionInit.loginUser)

  /** User may change once concurrently from Anonymous to non-anonymous due to late authentication. */
  final def currentUser: SimpleUser = _user.get()

  /** Succeeds only once. */
  private[session] final def tryUpdateUser(user: SimpleUser): Boolean =
    _user.compareAndSet(sessionInit.loginUser, user)
