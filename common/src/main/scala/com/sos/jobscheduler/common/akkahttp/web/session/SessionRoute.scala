package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.session.SessionCommand
import com.sos.jobscheduler.base.session.SessionCommand.{Login, Logout}
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRoute._
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait SessionRoute extends RouteProvider {

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

  private def execute(command: SessionCommand, httpUser: Session#User, sessionTokenOption: Option[SessionToken]): Task[Checked[SessionCommand.Response]] =
    command match {
      case Login(userAndPasswordOption) =>
        authenticateOrUseHttpUser(userAndPasswordOption, httpUser)
        .map(user =>
          sessionRegister.login(user, sessionTokenOption).map(Login.LoggedIn.apply)
        ).evert

      case Logout(sessionToken) =>
        sessionRegister.logout(sessionToken)
          .map { _: Completed => Right(SessionCommand.Response.Accepted) }
    }

  private def authenticateOrUseHttpUser(userAndPasswordOption: Option[UserAndPassword], httpUser: Session#User) =
    userAndPasswordOption match {
      case Some(userAndPassword) =>
        if (!httpUser.id.isAnonymous)
          Left(Problem("Both command Login and HTTP header authentication?"))
        else if (userAndPassword.userId.isAnonymous)
          Left(AnonymousLoginProblem)
        else
          gateKeeper.authenticateUser(userAndPassword) toChecked InvalidLoginProblem

      case None =>
        Right(httpUser)  // Take authenticated user from HTTP header `Authorization` or Anonymous
    }
}

object SessionRoute {
  object InvalidLoginProblem extends Problem.Eager("Login: unknown user or invalid password")
  private object AnonymousLoginProblem extends Problem.Eager("Login: user and password required")
}
