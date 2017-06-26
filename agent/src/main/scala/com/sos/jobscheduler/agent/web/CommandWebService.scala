package com.sos.jobscheduler.agent.web

import com.sos.jobscheduler.agent.command.CommandMeta
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.web.AgentUris.LicenseKeyHeaderName
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.soslicense.LicenseKeyBunch
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.data.session.SessionToken
import scala.concurrent.{ExecutionContext, Future}
import spray.http.RemoteAddress
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService extends AgentWebService {

  protected def executeCommand(command: AgentCommand, meta: CommandMeta): Future[AgentCommand.Response]
  protected implicit def executionContext: ExecutionContext

  routeBuilder.addApiRoute { user ⇒
    (path("command") & post) {
      optionalHeaderValueByName(SessionToken.HeaderName) { sessionTokenOption ⇒
        optionalHeaderValueByName(LicenseKeyHeaderName) { licenseKeys ⇒
          (clientIP | provide[RemoteAddress](RemoteAddress.Unknown)) { clientIp ⇒  // Requires Spray configuration spray.can.remote-address-header = on
            entity(as[AgentCommand]) { command ⇒
              val response: Future[AgentCommand.Response] = executeCommand(command, CommandMeta(
                user,
                clientIp.toOption,
                sessionTokenOption map { o ⇒ SessionToken(SecretString(o)) },
                LicenseKeyBunch(licenseKeys getOrElse "")))
              complete(response)
            }
          }
        }
      }
    }
  }
}
