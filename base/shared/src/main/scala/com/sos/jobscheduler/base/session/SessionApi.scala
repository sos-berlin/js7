package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.session.SessionApi._
import com.sos.jobscheduler.base.session.SessionCommand.{Login, Logout}
import com.sos.jobscheduler.base.web.HttpClient
import monix.eval.Task
import monix.execution.atomic.AtomicAny
import scala.concurrent.duration.FiniteDuration

// Test in SessionRouteTest

/**
  * @author Joacim Zschimmer
  */
trait SessionApi extends HasSessionToken
{
  protected def httpClient: HttpClient
  protected def sessionUri: String

  private val sessionTokenRef = AtomicAny[Option[SessionToken]](None)

  final def login(userAndPassword: Option[UserAndPassword]): Task[Unit] =
    for (response ← executeSessionCommand(Login(userAndPassword))) yield
      setSessionToken(response.sessionToken)

  final def loginUntilReachable(userAndPassword: Option[UserAndPassword], delays: Iterator[FiniteDuration], onError: Throwable ⇒ Unit)
  : Task[Unit] =
    login(userAndPassword)
      .onErrorRestartLoop(()) { (throwable, _, retry) ⇒
        onError(throwable)
        if (isTemporary(throwable) && delays.hasNext)
          retry(false) delayExecution delays.next()
        else
          Task.raiseError(throwable)
      }

  final def logout(): Task[Completed] = {
    val tokenOption = sessionTokenRef.get
    tokenOption match {
      case None ⇒ Task.pure(Completed)
      case Some(sessionToken) ⇒
        executeSessionCommand(Logout(sessionToken), suppressSessionToken = true) map {
          case SessionCommand.Response.Accepted ⇒
            sessionTokenRef.compareAndSet(tokenOption, None)  // Changes nothing in case of a concurrent successful Logout or Login
            Completed
        }
    }
  }

  private def executeSessionCommand(command: SessionCommand, suppressSessionToken: Boolean = false): Task[command.Response] =
    httpClient.post[SessionCommand, SessionCommand.Response](sessionUri, command, suppressSessionToken = suppressSessionToken)
      .map(_.asInstanceOf[command.Response])

  final def clearSession(): Unit =
    sessionTokenRef := None

  final def setSessionToken(sessionToken: SessionToken): Unit =
    sessionTokenRef := Some(sessionToken)

  // Used by AkkaHttpClient and JsHttpClient
  final def sessionToken: Option[SessionToken] =
    sessionTokenRef.get

  def hasSession = sessionTokenRef.get.nonEmpty
}

object SessionApi
{
  private def isTemporary(throwable: Throwable) =
    throwable match {
      case e: HttpClient.HttpException ⇒ e.isUnreachable
      case _ ⇒ true  // May be a TCP exception
    }
}
