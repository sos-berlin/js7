package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.data.responses.Response
import com.sos.scheduler.engine.agent.web.CommandService._
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.soslicense.LicenseKey
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait CommandService extends ServiceStandards {

  protected def executeCommand(command: Command, licenseKey: Option[LicenseKey]): Future[Response]

  addStandardRoute {
    (post & path("agent" / "command")) {
      optionalHeaderValueByName(LicenseKeyHeaderName) { licenseKeyString ⇒
        entity(as[Command]) { command ⇒
          val future = executeCommand(command, licenseKeyString map LicenseKey.apply)
          onSuccess(future) { response: Response ⇒ complete(response) }
        }
      }
    }
  }
}

object CommandService {
  private val LicenseKeyHeaderName = "X-JobScheduler-LicenseKey"     // Duplicate in AgentUris
}
