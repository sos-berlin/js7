package js7.controller.web.controller.api

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.pekkohttp.StandardMarshallers.*
import js7.controller.web.common.ControllerRouteProvider
import js7.core.command.CommandMeta
import js7.data.controller.ControllerCommand
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait CommandRoute extends ControllerRouteProvider:

  protected def config: Config

  private lazy val entitySizeLimit =
    config.getMemorySize("js7.web.server.services.command-size-limit").toBytes

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta): IO[Checked[command.Response]]

  private given IORuntime = ioRuntime

  final val commandRoute: Route =
    pathEnd:
      post:
        authorizedUser(ValidUserPermission) { user =>
          withSizeLimit(entitySizeLimit):
            entity(as[ControllerCommand]) { command =>
              completeIO:
                executeCommand(command, CommandMeta(user))
                  .map(_.map(o => o: ControllerCommand.Response))
            }
        }
