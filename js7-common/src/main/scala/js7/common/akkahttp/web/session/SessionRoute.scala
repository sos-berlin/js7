package js7.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.server.Directives.*
import js7.base.Js7Version
import js7.base.auth.{SessionToken, SimpleUser, UserAndPassword, UserId}
import js7.base.generic.Completed
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.session.SessionCommand
import js7.base.session.SessionCommand.{Login, Logout}
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.session.SessionRoute.*
import js7.data.problems.InvalidLoginProblem
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait SessionRoute extends RouteProvider
{
  private implicit def implictScheduler: Scheduler = scheduler
  protected[session] val specificLoginRequiredProblem: Problem = InvalidLoginProblem
  protected[session] lazy val preAuthenticate: Directive1[Either[Set[UserId], SimpleUser]] =
    gateKeeper.preAuthenticate

  protected final lazy val sessionRoute =
    pathEnd {
      post {
        sessionTokenOption { tokenOption =>
          preAuthenticate { idsOrUser =>
            entity(as[SessionCommand]) { command =>
              onSuccess(execute(command, idsOrUser, tokenOption).runToFuture) {
                case Left(problem @ (InvalidLoginProblem | AnonymousLoginProblem)) =>
                  completeUnauthenticatedLogin(Unauthorized, problem)

                case checked =>
                  complete(checked)
              }
            }
          }
        }
      }
    }

  private def execute(command: SessionCommand, idsOrUser: Either[Set[UserId], SimpleUser], sessionTokenOption: Option[SessionToken])
  : Task[Checked[SessionCommand.Response]] =
    command match {
      case Login(userAndPasswordOption, version) =>
        Task(authenticateOrUseHttpUser(idsOrUser, userAndPasswordOption))
          .flatMapT(user =>
            sessionRegister.login(user, version, sessionTokenOption)
              .map(_.map(Login.LoggedIn(_, Js7Version))))

      case Logout(sessionToken) =>
        sessionRegister.logout(sessionToken)
          .map((_: Completed) => Right(SessionCommand.Response.Accepted))
    }

  private def authenticateOrUseHttpUser(
    idsOrUser: Either[Set[UserId], SimpleUser],
    userAndPasswordOption: Option[UserAndPassword])
  =
    userAndPasswordOption match {
      case None =>
        idsOrUser match {
          case Left(_) =>
            Left(specificLoginRequiredProblem)

          case Right(u) => Right(u)
        }

      case Some(userAndPassword) =>
        if (userAndPassword.userId.isAnonymous)
          Left(InvalidLoginProblem)
        else
          gateKeeper.authenticateUser(userAndPassword)
            .toChecked(InvalidLoginProblem)
            .flatMap(authenticatedUser =>
              idsOrUser match {
                case Left(allowedUserIds) =>
                  if (!allowedUserIds.contains(userAndPassword.userId))
                    Left(specificLoginRequiredProblem)
                  else
                    Right(authenticatedUser)

                case Right(httpUser) =>
                  gateKeeper.authenticateUser(userAndPassword)
                    .toChecked(InvalidLoginProblem)
                    .flatMap(authenticatedUser =>
                      if (!httpUser.id.isAnonymous && httpUser.id != userAndPassword.userId)
                        Left(Problem("Login user does not match HTTP(S) user"))
                      else
                        Right(authenticatedUser))
              }
            )
    }
}

object SessionRoute
{
  private object AnonymousLoginProblem extends Problem.Eager("Login: user and password required")
}
