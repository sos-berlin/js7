package js7.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardMarshallers._
import js7.core.command.CommandMeta
import js7.master.data.MasterCommand
import js7.master.web.common.MasterRouteProvider
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait CommandRoute extends MasterRouteProvider {

  protected def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[MasterCommand.Response]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final val commandRoute: Route =
    pathEnd {
      post {
        authorizedUser(ValidUserPermission) { user =>
          entity(as[MasterCommand]) { command =>
            completeTask {
              executeCommand(command, CommandMeta(user))
            }
          }
        }
      }
    }
}
