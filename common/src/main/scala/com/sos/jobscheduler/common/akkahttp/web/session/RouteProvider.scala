package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.server.Directives.{complete, onSuccess, optionalHeaderValueByName, pass, respondWithHeader}
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.{Permission, SessionToken}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.ExceptionHandling
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.RouteProvider._
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRoute.InvalidLoginProblem
import com.sos.jobscheduler.common.akkahttp.web.session.{Session => Session_}
import com.sos.jobscheduler.common.scalautil.Logger
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
trait RouteProvider extends ExceptionHandling
{
  protected type Session <: Session_

  protected def gateKeeper: GateKeeper[Session#User]

  protected def sessionRegister: SessionRegister[Session]

  protected def scheduler: Scheduler

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final def authorizedUser(requiredPermission: Permission): Directive1[Session#User] =
    authorizedUser(Set(requiredPermission))

  protected final def authorizedUser(requiredPermissions: Set[Permission] = Set.empty): Directive1[Session#User] =
    new Directive[Tuple1[Session#User]] {
      def tapply(inner: Tuple1[Session#User] => Route) =
        maybeSession(requiredPermissions) {
          case (user, Some(session)) =>
            inner(Tuple1(user))

          case (user, None) =>
            inner(Tuple1(user))
        }
    }

  private def maybeSession(requiredPermissions: Set[Permission]): Directive1[(Session#User, Option[Session])] =
    new Directive[Tuple1[(Session#User, Option[Session])]]
    {
      def tapply(inner: Tuple1[(Session#User, Option[Session])] => Route) =
        gateKeeper.authenticate { authenticatedUser =>
          // user == Anonymous iff no credentials are given
          sessionOption(authenticatedUser) { sessionOption =>
            gateKeeper.authorize(sessionOption.fold(authenticatedUser)(_.currentUser), requiredPermissions) { authorizedUser =>
              // If and only if gateKeeper allows public access, the authorizedUser may be an empowered authenticatedUser.
              inner(Tuple1((authorizedUser, sessionOption)))
            }
          }
        }
    }

  /** Returns the session denoted by optional header `X-JobScheduler-Session-Token` or None.
    * The request is `Forbidden` if
    * the SessionToken is invalid or
    * the given `userId` is not Anonymous and does not match the sessions UserId.*/
  protected final def sessionOption(httpUser: Session#User): Directive[Tuple1[Option[Session]]] =
    new Directive[Tuple1[Option[Session]]] {
      def tapply(inner: Tuple1[Option[Session]] => Route) =
        sessionTokenOption(httpUser) {
          case None =>
            inner(Tuple1(None))

          case Some(sessionToken) =>
            onSuccess(sessionRegister.sessionFuture(!httpUser.isAnonymous ? httpUser, sessionToken)) {
              case Invalid(problem) =>
                completeUnauthenticatedLogin(Forbidden, problem)

              case Valid(session) =>
                inner(Tuple1(Some(session)))
            }
          }
    }

  protected final def sessionTokenOption(httpUser: Session#User): Directive[Tuple1[Option[SessionToken]]] =
    // user == Anonymous iff no credentials are given
    new Directive[Tuple1[Option[SessionToken]]] {
      def tapply(inner: Tuple1[Option[SessionToken]] => Route) =
        optionalHeaderValueByName(SessionToken.HeaderName) { option =>
          inner(Tuple1(option.map(string => SessionToken(SecretString(string)))))
        }
    }

  protected final def completeUnauthenticatedLogin(statusCode: StatusCode, problem: Problem): Route =
    (if (statusCode == Unauthorized) respondWithHeader(gateKeeper.wwwAuthenticateHeader) else pass) {
      val delay =
        if (problem == InvalidLoginProblem) {
          logger.debug(s"$problem - delaying response for ${gateKeeper.invalidAuthenticationDelay.pretty}")
          gateKeeper.invalidAuthenticationDelay
        } else Duration.Zero
      complete {
        Task.pure(statusCode -> problem)
          .delayExecution(delay)
          .runToFuture
      }
    }
}

object RouteProvider {
  private val logger = Logger(getClass)
}
