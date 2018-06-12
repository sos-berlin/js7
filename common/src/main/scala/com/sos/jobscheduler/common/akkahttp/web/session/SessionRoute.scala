package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.session.SessionCommand
import com.sos.jobscheduler.base.session.SessionCommand.{Login, Logout}
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRoute._
import com.sos.jobscheduler.common.monix.MonixForCats.taskApplicative
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait SessionRoute extends RouteProvider {

  protected implicit def scheduler: Scheduler

  protected final val sessionRoute = Route.seal {
    pathEnd {
      post {
        gateKeeper.authenticate { user ⇒
          sessionOption(user.id) { sessionOption ⇒
            entity(as[SessionCommand]) { command ⇒
              val u = sessionOption.fold(user)(_.user)
              val s = sessionOption map (_.sessionToken)
              onSuccess(execute(command, u, s).runAsync) {
                case Invalid(AnonymousLoginProblem) ⇒
                  // No credentials via Authorization header or Login(Some(...))
                  // Let a browser show authentication dialog!
                  reject(gateKeeper.credentialsMissing)

                case Invalid(p @ InvalidLoginProblem) ⇒
                  completeUnauthenticatedLogin(p)

                case checked ⇒
                  complete(checked)
              }
            }
          }
        }
      }
    }
  }

  private def execute(command: SessionCommand, httpUser: Session#User, sessionTokenOption: Option[SessionToken]): Task[Checked[SessionCommand.Response]] =
    command match {
      case Login(userAndPasswordOption) ⇒
        authenticateOrUseHttpUser(userAndPasswordOption, httpUser)
        .map(user ⇒
          sessionRegister.login(user, sessionTokenOption).map(Login.Response.apply)
        ).evert

      case Logout ⇒
        sessionTokenOption match {
          case None ⇒ Task.pure(Problem("Logout without session token?"))
          case Some(sessionToken) ⇒
            sessionRegister.logout(sessionToken)
              .map((_: Completed) ⇒ Valid(SessionCommand.Response.Accepted))
        }
    }

  private def authenticateOrUseHttpUser(userAndPasswordOption: Option[UserAndPassword], httpUser: Session#User) =
    userAndPasswordOption match {
      case Some(userAndPassword) ⇒
        gateKeeper.authenticateUser(userAndPassword) toChecked InvalidLoginProblem

      case None ⇒
        if (httpUser.id == UserId.Anonymous)  // No HTTP credentials (header `Authorization`)
          Invalid(AnonymousLoginProblem)      // and Login without credentials? Login requires credentials.
        else
          Valid(httpUser)  // Take authenticated user from HTTP header `Authorization`
    }
}

object SessionRoute {
  private object AnonymousLoginProblem extends Problem.Eager("Anonymous Login?")  // Internal only
  private object InvalidLoginProblem extends Problem.Eager("Login: unknown user or invalid password")
}
