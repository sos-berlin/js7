package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword}
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.session.TestSessionApi._
import monix.eval.Task
import monix.execution.atomic.{AtomicAny, AtomicLong}

final class TestSessionApi(expectedUserAndPassword: Option[UserAndPassword] = None)
extends SessionApi.LoginUntilReachable
{
  private val sessionTokenRef = AtomicAny[Option[SessionToken]](None)

  def login(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean) =
    Task {
      if (userAndPassword == expectedUserAndPassword) {
        sessionTokenRef := Some(sessionTokenGenerator.next())
        Completed
      } else throw new IllegalArgumentException("TestSessionApi: userAndPassword do not match")
    }

  def logout(): Task[Completed] =
    Task {
      sessionTokenRef := None
      Completed
    }

  def hasSession = sessionTokenRef.get.isDefined

  protected def isTemporaryLoginError(throwable: Throwable) = false

  override def toString = "TestSessionApi"
}

object TestSessionApi
{
  private val sessionTokenGenerator = new Iterator[SessionToken] {
    private val counter = AtomicLong(0L)
    def hasNext = true
    def next() = SessionToken(SecretString("SECRET-" + counter.incrementAndGet().toString))
  }
}
