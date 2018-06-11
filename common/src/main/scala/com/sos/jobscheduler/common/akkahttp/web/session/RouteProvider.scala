package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import akka.http.scaladsl.model.headers.{HttpChallenge, `WWW-Authenticate`}
import akka.http.scaladsl.server.Directives.{complete, onSuccess, optionalHeaderValueByName, respondWithHeader}
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.{PermissionBundle, SessionToken, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.RouteProvider._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait RouteProvider
{
  protected type Session <: LoginSession

  protected def gateKeeper: GateKeeper[Session#User]

  protected def sessionRegister: SessionRegister[Session]

  protected def scheduler: Scheduler

  private implicit def implicitScheduler = scheduler

  protected final def authorizedUser(requiredPermissions: PermissionBundle = PermissionBundle.empty): Directive1[Session#User] =
    new Directive[Tuple1[Session#User]] {
      def tapply(inner: Tuple1[Session#User] ⇒ Route) =
        maybeSession(requiredPermissions) {
          case (_, Some(session)) ⇒
            inner(Tuple1(session.user))

          case (user, None) ⇒
            inner(Tuple1(user))
        }
    }

  private def maybeSession(requiredPermissions: PermissionBundle = PermissionBundle.empty): Directive1[(Session#User, Option[Session])] =
    new Directive[Tuple1[(Session#User, Option[Session])]]
    {
      def tapply(inner: Tuple1[(Session#User, Option[Session])] ⇒ Route) =
        gateKeeper.authenticate { user ⇒
          // user == Anonymous iff no credentials are given
          sessionOption(user.id) { sessionOption ⇒
            val u = sessionOption.fold(user)(_.user)
            gateKeeper.authorize(u, requiredPermissions) {
              inner(Tuple1((u, sessionOption)))
            }
          }
        }
    }

  /** Returns the session denoted by optional header `X-JobScheduler-Session-Token` or None.
    * The request is `Forbidden` if
    * the SessionToken is invalid or
    * the given `userId` is not Anonymous and does not match the sessions UserId.*/
  protected def sessionOption(userId: UserId = UserId.Anonymous): Directive[Tuple1[Option[Session]]] = {
    // user == Anonymous iff no credentials are given
    new Directive[Tuple1[Option[Session]]] {
      def tapply(inner: Tuple1[Option[Session]] ⇒ Route) =
        optionalHeaderValueByName(SessionToken.HeaderName) {
          case None ⇒
            inner(Tuple1(None))

          case Some(string) ⇒
            val userIdOption = (userId != UserId.Anonymous) ? userId
            onSuccess(sessionRegister.sessionFuture(userIdOption, SessionToken(SecretString(string)))) {
              case Invalid(problem) ⇒
                completeUnauthenticatedLogin(problem)

              case Valid(session) ⇒
                inner(Tuple1(Some(session)))
            }
        }
    }
  }

  protected def completeUnauthenticatedLogin(problem: Problem): Route =
    respondWithHeader(LoginWWWAuthenticate) {
      logger.warn(s"Login with invalid authentication rejected - delaying response for ${gateKeeper.invalidAuthenticationDelay.pretty}")
      complete {
        Task.pure(Unauthorized → problem) delayExecution gateKeeper.invalidAuthenticationDelay.toFiniteDuration
      }
    }
}

object RouteProvider {
  private val logger = Logger(getClass)
  val LoginWWWAuthenticate = `WWW-Authenticate`(HttpChallenge("X-JobScheduler-Login", realm = None))
}
