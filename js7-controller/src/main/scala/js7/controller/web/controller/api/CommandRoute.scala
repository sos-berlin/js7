package js7.controller.web.controller.api

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import com.typesafe.config.Config
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.StandardMarshallers.*
import js7.controller.web.common.ControllerRouteProvider
import js7.core.command.CommandMeta
import js7.data.controller.ControllerCommand
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait CommandRoute extends ControllerRouteProvider:
  protected def config: Config

  private lazy val entitySizeLimit =
    config.getMemorySize("js7.web.server.services.command-size-limit").toBytes

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final val commandRoute: Route =
    pathEnd:
      post:
        authorizedUser(ValidUserPermission) { user =>
          withSizeLimit(entitySizeLimit):
            entity(as[ControllerCommand]) { command =>
              completeTask:
                executeCommand(command, CommandMeta(user))
                  .map(_.map(o => o: ControllerCommand.Response))
            }
        }
