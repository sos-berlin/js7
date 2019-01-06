package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.server.Directives._
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

/**
  * @author Joacim Zschimmer
  */
trait SessionRoute extends RouteProvider {

  private implicit def implictScheduler = scheduler

  protected final lazy val sessionRoute =
    pathEnd {
      post {
        gateKeeper.authenticate { httpUser ⇒
          sessionOption(httpUser.id) { sessionOption ⇒
            entity(as[SessionCommand]) { command ⇒
              val token = sessionOption map (_.sessionToken)
              onSuccess(execute(command, httpUser, token).runToFuture) {
                case Invalid(AnonymousLoginProblem) ⇒
                  // No credentials via Authorization header or Login(Some(...))
                  // Let a browser show authentication dialog!
                  reject(gateKeeper.credentialsMissing)

                case Invalid(InvalidLoginProblem) ⇒
                  completeUnauthenticatedLogin(InvalidLoginProblem)

                case checked ⇒
                  complete(checked)
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
          sessionRegister.login(user, sessionTokenOption).map(Login.LoggedIn.apply)
        ).evert

      case Logout(sessionToken) ⇒
        sessionRegister.logout(sessionToken)
          .map { _: Completed ⇒ Valid(SessionCommand.Response.Accepted) }
    }

  private def authenticateOrUseHttpUser(userAndPasswordOption: Option[UserAndPassword], httpUser: Session#User) =
    userAndPasswordOption match {
      case Some(userAndPassword) ⇒
        if (httpUser.id != UserId.Anonymous)
          Invalid(Problem("Both command Login and HTTP header Authentication?"))
        else if (userAndPassword.userId == UserId.Anonymous)
          Invalid(InvalidLoginProblem)  // Anonymous is used only if there is no authentication at all
        else
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
