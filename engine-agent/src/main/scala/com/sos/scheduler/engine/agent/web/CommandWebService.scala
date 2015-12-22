package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.command.CommandMeta
import com.sos.scheduler.engine.agent.data.commandresponses.Response
import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.data.web.AgentUris.LicenseKeyHeaderName
import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.soslicense.LicenseKeyBunch
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import scala.concurrent.{ExecutionContext, Future}
import spray.http.RemoteAddress
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService extends AgentWebService {

  protected def executeCommand(command: Command, meta: CommandMeta): Future[Response]
  protected implicit def executionContext: ExecutionContext

  addApiRoute {
    (path("command") & post) {
      optionalHeaderValueByName(LicenseKeyHeaderName) { licenseKeys ⇒
        (clientIP | provide[RemoteAddress](RemoteAddress.Unknown)) { clientIp ⇒  // Requires Spray configuration spray.can.remote-address-header = on
          entity(as[Command]) { command ⇒
            val future = executeCommand(command, CommandMeta(
              clientIp.toOption,
              LicenseKeyBunch(licenseKeys getOrElse "")))
            onSuccess(future) { response: Response ⇒ complete(response) }
          }
        }
      }
    }
  }
}
