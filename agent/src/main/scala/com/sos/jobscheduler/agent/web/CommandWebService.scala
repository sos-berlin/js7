package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.ServiceUnavailable
import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.problems.AgentIsShuttingDownProblem
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.{SessionToken, ValidUserPermission}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.ProblemException
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.Failure

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService extends AgentRouteProvider {

  protected def commandExecute(meta: CommandMeta, command: AgentCommand): Task[AgentCommand.Response]
  protected def commandOverview: Task[CommandHandlerOverview]
  protected def commandDetailed: Task[CommandHandlerDetailed[AgentCommand]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final lazy val commandRoute: Route =
    authorizedUser(ValidUserPermission) { user ⇒
      post {
        pathEnd {
          optionalHeaderValueByName(SessionToken.HeaderName) { sessionTokenOption ⇒
            entity(as[AgentCommand]) { command ⇒
              completeTask {
                val meta = CommandMeta(user, sessionTokenOption map { o ⇒ SessionToken(SecretString(o)) })
                commandExecute(meta, command).materialize.map {
                  case Failure(e: ProblemException) if e.problem == AgentIsShuttingDownProblem ⇒
                    ToResponseMarshallable(ServiceUnavailable → e.problem)
                  case o ⇒
                    ToResponseMarshallable(o)
                }
              }
            }
          }
        }
      } ~
      get {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {
          pathEnd {
            completeTask(
              commandOverview)
          } ~
          pathSingleSlash {
            completeTask(
              commandDetailed map (_.commandRuns))
          }
        }
      }
    }
}
