package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.session.SessionCommand.{Login, Logout}
import com.sos.jobscheduler.base.web.{HttpClient, Uri}
import monix.eval.Task
import monix.execution.atomic.AtomicAny

// Test in SessionRouteTest

/**
  * @author Joacim Zschimmer
  */
trait HttpSessionApi extends SessionApi.HasUserAndPassword with HasSessionToken
{
  protected def httpClient: HttpClient
  protected def sessionUri: Uri

  private val sessionTokenRef = AtomicAny[Option[SessionToken]](None)

  final def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false): Task[Completed] =
    Task.defer {
      if (onlyIfNotLoggedIn && hasSession)
        Task.pure(Completed)
      else
        for (response <- executeSessionCommand(Login(userAndPassword))) yield {
          setSessionToken(response.sessionToken)
          Completed
        }
    }

  protected def isTemporaryUnreachable(throwable: Throwable) =
    throwable match {
      case e: HttpClient.HttpException => e.isTemporaryUnreachable
      case _ => true  // Maybe a TCP exception
    }

  final def logout(): Task[Completed] =
    Task.defer {
      sessionTokenRef.get match {
        case None => Task.pure(Completed)
        case sometoken @ Some(sessionToken) =>
          executeSessionCommand(Logout(sessionToken), suppressSessionToken = true)
            .doOnFinish(_ => Task {
              sessionTokenRef.compareAndSet(sometoken, None)   // Changes nothing in case of a concurrent successful Logout or Login
            })
            .map { _: SessionCommand.Response.Accepted =>
              Completed
            }
      }
    }

  private def executeSessionCommand(command: SessionCommand, suppressSessionToken: Boolean = false): Task[command.Response] = {
    implicit def implicitSessionToken = if (suppressSessionToken) Task.pure(None) else Task(sessionToken)
    Task { scribe.debug(s"$toString: $command") } >>
      httpClient.post[SessionCommand, SessionCommand.Response](sessionUri, command)
        .map(_.asInstanceOf[command.Response])
  }

  final def clearSession(): Unit =
    sessionTokenRef := None

  final def setSessionToken(sessionToken: SessionToken): Unit =
    sessionTokenRef := Some(sessionToken)

  final def sessionToken: Option[SessionToken] =
    sessionTokenRef.get
}
