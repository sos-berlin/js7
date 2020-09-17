package js7.controller.web.controller.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardMarshallers._
import js7.controller.data.ControllerCommand
import js7.controller.web.common.ControllerRouteProvider
import js7.core.command.CommandMeta
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait CommandRoute extends ControllerRouteProvider {

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final val commandRoute: Route =
    pathEnd {
      post {
        authorizedUser(ValidUserPermission) { user =>
          entity(as[ControllerCommand]) { command =>
            completeTask {
              executeCommand(command, CommandMeta(user))
                .map(_.map(o => o: ControllerCommand.Response))
            }
          }
        }
      }
    }
}
