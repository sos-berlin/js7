package com.sos.jobscheduler.common.akkahttp.web.session

import com.sos.jobscheduler.base.auth.{User => User_}
import monix.execution.atomic.AtomicAny

/**
  * @author Joacim Zschimmer
  */
trait Session extends HasTimeout
{
  type User <: User_

  protected[session] def sessionInit: SessionInit[User]

  final def sessionNumber = sessionInit.sessionNumber

  final def sessionToken = sessionInit.sessionToken

  private lazy val _user = AtomicAny[User](sessionInit.loginUser)

  /** User may change once concurrently from Anonymous to non-anonymous due to late authentication. */
  final def currentUser: User = _user.get

  /** Succeeds only once. */
  private[session] final def tryUpdateUser(user: User): Boolean =
    _user.compareAndSet(sessionInit.loginUser, user)
}
