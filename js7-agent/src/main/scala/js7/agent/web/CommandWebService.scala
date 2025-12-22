package js7.agent.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.agent.data.commands.AgentCommand
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.pekkohttp.StandardMarshallers.*
import js7.core.command.CommandMeta
import js7.core.command.CommandMeta.pekkoDirectives.commandMeta
import js7.core.web.EntitySizeLimitProvider
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService
extends AgentRouteProvider, EntitySizeLimitProvider:

  protected val executeCommand: (AgentCommand, CommandMeta) => IO[Checked[AgentCommand.Response]]

  private given IORuntime = ioRuntime

  final lazy val commandRoute: Route =
    (post & pathEnd & withSizeLimit(entitySizeLimit)):
      authorizedUser(ValidUserPermission): user =>
        entity(as[AgentCommand]): command =>
          commandMeta(user): meta =>
            completeIO:
              executeCommand(command, meta)
