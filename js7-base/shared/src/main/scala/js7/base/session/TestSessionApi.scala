package js7.base.session

import js7.base.auth.{SessionToken, UserAndPassword}
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.{Completed, SecretString}
import js7.base.session.TestSessionApi.*
import monix.eval.Task
import monix.execution.atomic.{AtomicAny, AtomicLong}
import scala.collection.AbstractIterator

final class TestSessionApi(expectedUserAndPassword: Option[UserAndPassword] = None)
extends SessionApi.HasUserAndPassword, HasIsIgnorableStackTrace:

  protected def userAndPassword = expectedUserAndPassword

  private val sessionTokenRef = AtomicAny[Option[SessionToken]](None)

  def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean) =
    Task:
      if userAndPassword == expectedUserAndPassword then
        sessionTokenRef := Some(sessionTokenGenerator.next())
        Completed
      else throw new IllegalArgumentException("TestSessionApi: userAndPassword do not match")

  def logout(): Task[Completed] =
    Task:
      sessionTokenRef := None
      Completed

  def clearSession() =
    sessionTokenRef := None

  def hasSession = sessionTokenRef.get().isDefined

  override protected def isTemporaryUnreachable(throwable: Throwable) = false

  override def toString = "TestSessionApi"


object TestSessionApi:
  private val sessionTokenGenerator = new AbstractIterator[SessionToken]:
    private val counter = AtomicLong(0L)
    def hasNext = true
    def next() = SessionToken(SecretString("SECRET-" + counter.incrementAndGet().toString))
