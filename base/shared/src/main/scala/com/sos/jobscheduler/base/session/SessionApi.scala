package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.session.SessionCommand.{Login, Logout}
import com.sos.jobscheduler.base.web.HttpClient
import monix.eval.Task
import monix.execution.atomic.AtomicAny

// Test in SessionRouteTest

/**
  * @author Joacim Zschimmer
  */
trait SessionApi extends HasSessionToken
{
  protected def httpClient: HttpClient
  protected def sessionUri: String

  private val sessionTokenRef = AtomicAny[Option[SessionToken]](None)

  final def login(userAndPassword: Option[UserAndPassword]): Task[SessionToken] =
    for (response ← executeSessionCommand(Login(userAndPassword))) yield {
      setSessionToken(response.sessionToken)
      response.sessionToken
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
