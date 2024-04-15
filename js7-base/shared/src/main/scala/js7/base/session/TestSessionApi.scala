package js7.base.session

import js7.base.auth.{SessionToken, UserAndPassword}
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.{Completed, SecretString}
import js7.base.session.TestSessionApi.*
import cats.effect.IO
import cats.syntax.option.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import scala.collection.AbstractIterator

final class TestSessionApi(expectedUserAndPassword: Option[UserAndPassword] = None)
extends SessionApi.HasUserAndPassword, HasIsIgnorableStackTrace:

  protected def userAndPassword = expectedUserAndPassword

  private val sessionTokenRef = Atomic(none[SessionToken])

  def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean): IO[Completed] =
    IO:
      if userAndPassword == expectedUserAndPassword then
        sessionTokenRef := Some(sessionTokenGenerator.next())
        Completed
      else throw new IllegalArgumentException("TestSessionApi: userAndPassword do not match")

  def logout(): IO[Completed] =
    IO:
      sessionTokenRef := None
      Completed

  def clearSession(): Unit =
    sessionTokenRef := None

  def hasSession: Boolean =
    sessionTokenRef.get().isDefined

  override protected def isTemporaryUnreachable(throwable: Throwable) = 
    false

  override def toString = 
    "TestSessionApi"


object TestSessionApi:
  private val sessionTokenGenerator = new AbstractIterator[SessionToken]:
    private val counter = Atomic(0L)
    def hasNext = true
    def next() = SessionToken(SecretString("SECRET-" + counter.incrementAndGet().toString))
