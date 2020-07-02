package js7.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import akka.http.scaladsl.server.Directives._
import js7.base.auth.{SessionToken, UserAndPassword}
import js7.base.generic.Completed
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.session.SessionCommand
import js7.base.session.SessionCommand.{Login, Logout}
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkahttp.web.session.SessionRoute._
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait SessionRoute extends RouteProvider
{
  private implicit def implictScheduler: Scheduler = scheduler

  protected final lazy val sessionRoute =
    pathEnd {
      post {
        gateKeeper.authenticate { httpUser =>
          sessionTokenOption(httpUser) { tokenOption =>
            entity(as[SessionCommand]) { command =>
              onSuccess(execute(command, httpUser, tokenOption).runToFuture) {
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

  private def execute(command: SessionCommand, httpUser: Session#User, sessionTokenOption: Option[SessionToken])
  : Task[Checked[SessionCommand.Response]] =
    command match {
      case Login(userAndPasswordOption) =>
        authenticateOrUseHttpUser(httpUser, userAndPasswordOption)
          .traverse(user =>
            sessionRegister.login(user, sessionTokenOption)
              .map(Login.LoggedIn.apply))

      case Logout(sessionToken) =>
        sessionRegister.logout(sessionToken)
          .map { _: Completed => Right(SessionCommand.Response.Accepted) }
    }

  private def authenticateOrUseHttpUser(httpUser: Session#User, userAndPasswordOption: Option[UserAndPassword]) =
    userAndPasswordOption match {
      case Some(userAndPassword) =>
        if (!httpUser.id.isAnonymous/* && httpUser.id != userAndPassword.userId*/)
          Left(Problem.pure("Pointless Login authentication after HTTP(S) authentication"))
        else if (userAndPassword.userId.isAnonymous)
          Left(AnonymousLoginProblem)
        else
          gateKeeper.authenticateUser(userAndPassword) toChecked InvalidLoginProblem

      case None =>
        // Authenticated user from HTTP header `Authorization` or HTTPS client certificate, or Anonymous
        Right(httpUser)
    }
}

object SessionRoute {
  object InvalidLoginProblem extends Problem.Eager("Login: unknown user or invalid password")
  private object AnonymousLoginProblem extends Problem.Eager("Login: user and password required")
}
