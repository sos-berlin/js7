package js7.agent.web

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import js7.agent.data.commands.AgentCommand
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.StandardMarshallers.*
import js7.core.command.CommandMeta
import js7.core.web.EntitySizeLimitProvider
import monix.eval.Task
import monix.execution.Scheduler

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService
extends AgentRouteProvider with EntitySizeLimitProvider:
  protected val executeCommand: (AgentCommand, CommandMeta) => Task[Checked[AgentCommand.Response]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final lazy val commandRoute: Route =
    authorizedUser(ValidUserPermission) { user =>
      post:
        pathEnd:
          withSizeLimit(entitySizeLimit):
            entity(as[AgentCommand]) { command =>
              completeTask:
                executeCommand(command, CommandMeta(user))
            }
    }
