package js7.common.pekkohttp.web.session

import js7.base.auth.{Permission, SessionToken, SimpleUser, UserId}
import js7.base.configutils.Configs.RichConfig
import js7.base.generic.SecretString
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.http.PekkoHttpClient.`x-js7-session`
import js7.common.pekkohttp.ExceptionHandling
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.RouteProvider.*
import js7.data.problems.InvalidLoginProblem
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.http.scaladsl.model.StatusCode
import org.apache.pekko.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import org.apache.pekko.http.scaladsl.server.Directives.{complete, onSuccess, optionalHeaderValuePF, pass, respondWithHeader}
import org.apache.pekko.http.scaladsl.server.{Directive, Directive1, Route}

/**
  * @author Joacim Zschimmer
  */
trait RouteProvider extends ExceptionHandling
{
  protected type OurSession <: Session
  protected val sessionRegister: SessionRegister[OurSession]

  protected def gateKeeper: GateKeeper[SimpleUser]

  protected def scheduler: Scheduler

  protected final lazy val chunkSize = config.memorySizeAsInt("js7.web.chunk-size").orThrow

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final def authorizedUser(requiredPermission: Permission): Directive1[SimpleUser] =
    authorizedUser(Set(requiredPermission))

  protected final def authorizedUser(requiredPermissions: Set[Permission] = Set.empty): Directive1[SimpleUser] =
    new Directive[Tuple1[SimpleUser]] {
      def tapply(inner: Tuple1[SimpleUser] => Route) =
        maybeSession(requiredPermissions) {
          case (user, Some(session)) =>
            inner(Tuple1(user))

          case (user, None) =>
            inner(Tuple1(user))
        }
    }

  protected final def maybeSession(requiredPermissions: Set[Permission])
  : Directive1[(SimpleUser, Option[OurSession])] =
    new Directive[Tuple1[(SimpleUser, Option[OurSession])]]
    {
      def tapply(inner: Tuple1[(SimpleUser, Option[OurSession])] => Route) =
        gateKeeper.preAuthenticate { idsOrUser =>
          // idsOrUser == Right(Anonymous) iff no credentials are given
          sessionOption(idsOrUser) {
            case None =>
              gateKeeper.authorize(idsOrUser getOrElse gateKeeper.anonymous, requiredPermissions) { authorizedUser =>
                // If and only if gateKeeper allows public access, the authorizedUser may be an empowered user.
                inner(Tuple1((authorizedUser, None)))
              }

            case Some(session) =>
              val user = session.currentUser./*???*/asInstanceOf[SimpleUser]
              gateKeeper.authorize(user, requiredPermissions) { authorizedUser =>
                // If and only if gateKeeper allows public access, the authorizedUser may be an empowered session.currentUser.
                inner(Tuple1((authorizedUser, Some(session))))
              }
          }
        }
    }

  /** Returns the session denoted by optional header `x-js7-session` or None.
    * The request is `Forbidden` if
    * the SessionToken is invalid or
    * the given `userId` is not Anonymous and does not match the sessions UserId.*/
  protected final def sessionOption(idsOrUser: Either[Set[UserId], SimpleUser]): Directive[Tuple1[Option[OurSession]]] =
    new Directive[Tuple1[Option[OurSession]]] {
      def tapply(inner: Tuple1[Option[OurSession]] => Route) =
        sessionTokenOption {
          case None =>
            inner(Tuple1(None))

          case Some(sessionToken) =>
            onSuccess(sessionRegister.sessionFuture(sessionToken, idsOrUser)) {
              case Left(problem) =>
                completeUnauthenticatedLogin(Forbidden, problem)

              case Right(session) =>
                inner(Tuple1(Some(session)))
            }
          }
    }

  protected final val sessionTokenOption: Directive[Tuple1[Option[SessionToken]]] =
    optionalHeaderValuePF {
      case `x-js7-session`(secret) => SessionToken(SecretString(secret))
    }

  protected final def completeUnauthenticatedLogin(statusCode: StatusCode, problem: Problem): Route =
    (if (statusCode == Unauthorized) respondWithHeader(gateKeeper.wwwAuthenticateHeader) else pass) {
      val delay =
        if (problem == InvalidLoginProblem) {
          logger.debug(s"$problem - delaying response for ${gateKeeper.invalidAuthenticationDelay.pretty}")
          gateKeeper.invalidAuthenticationDelay
        } else
          ZeroDuration
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
