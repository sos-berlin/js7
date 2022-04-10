package js7.agent.web

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.agent.data.commands.AgentCommand
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.StandardMarshallers._
import js7.core.command.CommandMeta
import js7.core.web.EntitySizeLimitProvider
import js7.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import monix.eval.Task
import monix.execution.Scheduler

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService
extends AgentRouteProvider with EntitySizeLimitProvider
{
  protected def commandExecute(meta: CommandMeta, command: AgentCommand): Task[Checked[AgentCommand.Response]]
  protected def commandOverview: Task[CommandHandlerOverview]
  protected def commandDetailed: Task[CommandHandlerDetailed[AgentCommand]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final lazy val commandRoute: Route =
    authorizedUser(ValidUserPermission) { user =>
      post {
        pathEnd {
          withSizeLimit(entitySizeLimit) {
            sessionTokenOption { maybeSessionToken =>
              entity(as[AgentCommand]) { command =>
                completeTask {
                  commandExecute(CommandMeta(user, maybeSessionToken), command)
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
              commandDetailed.map(_.commandRuns))
          }
        }
      }
    }
}
